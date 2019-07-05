add_legende_joignantes <-
function(map,titre=NULL,lng=NULL,lat=NULL,dom="0",precision=0,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error4 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    idx_carte <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_joignantes"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_joignantes") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[3][[1]]$nom_couche=="legende_joignantes") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende)) # la legende existe
        {
          if(map$x$calls[[i]]$method %in% "addMarkers")
          {
            if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_joignantes") idx_legende <- c(idx_legende,i)
          }
        }
      }
    }
    idx_fleche <- idx_carte[length(idx_carte)]
    idx_carte <- idx_carte[-length(idx_carte)]
    
    code_epsg <- map$x$calls[[idx_fleche]]$args[[3]]$code_epsg
    var_flux <- map$x$calls[[idx_fleche]]$args[[3]]$var_flux
    
    lng_init <- lng
    lat_init <- lat
    if(!is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }else
    {
      if(!is.null(idx_legende)) map$x$calls <- map$x$calls[-idx_legende] # Si la legende existe, on la supprime pour la recreer
      
      coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
      
      lng_init <- lng
      lat_init <- lat
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map$x$fitBounds[[4]]
        lat <- map$x$fitBounds[[3]]-coeff*8
      }
      
      vmax <- map$x$calls[[idx_fleche]]$args[[3]]$max_var
      
      coord_fleche_max <- data.frame(lng=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lng,lat=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lat)
      
      large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[2,1],coord_fleche_max[2,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
      long <- coeff*2
      
      flux_leg <- flux_legende_joignantes(lng,lat,long,large)
      flux_legWGS84 <- flux_leg[[1]]
      flux_legWGS84 <- cbind(flux_legWGS84,VALEUR=c(vmax,vmax/3))
      pointe1 <- flux_leg[[2]]
      pointe2 <- flux_leg[[3]]
      
      # leaflet du cadre blanc en 1er
      map <- addRectangles(map = map,
                           lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                           lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                           stroke = FALSE,
                           options = pathOptions(clickable = F),
                           fill = T,
                           fillColor = "white",
                           fillOpacity = 0.8,
                           group=list(nom_couche="legende_joignantes")
      )
      
      map <- addPolygons(map = map,
                         data=flux_legWGS84,
                         stroke = TRUE,
                         opacity = 1,
                         color = "#303030",
                         weight = 1,
                         options = pathOptions(clickable = F),
                         fill = T,
                         fillColor = "#CD853F",
                         fillOpacity = 1,
                         group=list(nom_couche="legende_joignantes",nom_fond="fond_flux_leg")
      )
      
      # leaflet valeur flux
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe1[1], lat = pointe1[2], #grande fleche
                                 label = as.character(format(round(vmax,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group=list(nom_couche="legende_joignantes")
      )
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe2[1], lat = pointe2[2], #petite fleche
                                 label = as.character(format(round(vmax/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group=list(nom_couche="legende_joignantes")
      )
      
      #leaflet titre 1
      map <- addLabelOnlyMarkers(map = map,
                                 lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group=list(nom_couche="legende_joignantes")
      )
      
      
    }
    
    message(simpleMessage(paste0("Les coordonnees de la legende des fleches joignantes sont : longitude (x) = ",lng," degre ; latitude (y) = ",lat," degre")))
    
    return(map)
  }
