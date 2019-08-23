add_legende_joignantes <-
function(map,titre=NULL,lng=NULL,lat=NULL,precision=0,zoom=8,map_leaflet=NULL)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if (!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error4 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
    
    if(any(class(map) %in% "leaflet"))
    {
      idx_fleche <- NULL
      idx_legende <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(any(map$x$calls[[i]]$args[3][[1]] %in% c("carte_joignantes"))) idx_fleche <- i
        }
        
        if(map$x$calls[[i]]$method %in% "addRectangles")
        {
          if(map$x$calls[[i]]$args[[6]]=="legende_joignantes") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende))
        {
          if(map$x$calls[[i]]$method %in% "addPolygons")
          {
            if(map$x$calls[[i]]$args[3][[1]]=="legende_joignantes") idx_legende <- c(idx_legende,i)
          }
          if(!is.null(idx_legende)) # la legende existe
          {
            if(map$x$calls[[i]]$method %in% "addMarkers")
            {
              if(map$x$calls[[i]]$args[5][[1]]=="legende_joignantes") idx_legende <- c(idx_legende,i)
            }
          }
        }
      }
      
      code_epsg <- map$x$calls[[idx_fleche]]$args[[2]]$code_epsg
      var_flux <- map$x$calls[[idx_fleche]]$args[[2]]$var_flux
      colBorder <- map$x$calls[[idx_fleche]]$args[[4]]$color
      colFleche <- map$x$calls[[idx_fleche]]$args[[4]]$fillColor
      
      if(!is.null(idx_legende)) map$x$calls <- map$x$calls[-idx_legende] # Si la legende existe, on la supprime pour la recreer
      
      lng_init <- lng
      lat_init <- lat
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map$x$fitBounds[[4]]
        lat <- map$x$fitBounds[[3]]-coeff*8
      }
      
      vmax <- map$x$calls[[idx_fleche]]$args[[2]]$max_var
      
      coord_fleche_max <- data.frame(lng=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lng,lat=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lat)
      
    }else if(any(class(map) %in% "leaflet_proxy"))
    {
      idx_fleche <- NULL
      idx_legende <- NULL
      for(i in 1:length(map_leaflet$x$calls))
      {
        if(map_leaflet$x$calls[[i]]$method %in% "addPolygons")
        {
          if(any(map_leaflet$x$calls[[i]]$args[3][[1]] %in% c("carte_joignantes"))) idx_fleche <- i
        }
        
        if(map_leaflet$x$calls[[i]]$method %in% "addRectangles")
        {
          if(map_leaflet$x$calls[[i]]$args[[6]]=="legende_joignantes") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende))
        {
          if(map_leaflet$x$calls[[i]]$method %in% "addPolygons")
          {
            if(map_leaflet$x$calls[[i]]$args[3][[1]]=="legende_joignantes") idx_legende <- c(idx_legende,i)
          }
          if(!is.null(idx_legende)) # la legende existe
          {
            if(map_leaflet$x$calls[[i]]$method %in% "addMarkers")
            {
              if(map_leaflet$x$calls[[i]]$args[5][[1]]=="legende_joignantes") idx_legende <- c(idx_legende,i)
            }
          }
        }
      }
      
      code_epsg <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$code_epsg
      var_flux <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$var_flux
      colBorder <- map_leaflet$x$calls[[idx_fleche]]$args[[4]]$color
      colFleche <- map_leaflet$x$calls[[idx_fleche]]$args[[4]]$fillColor
      
      lng_init <- lng
      lat_init <- lat
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map_leaflet$x$fitBounds[[4]]
        lat <- map_leaflet$x$fitBounds[[3]]-coeff*8
      }
      
      vmax <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$max_var
      
      coord_fleche_max <- data.frame(lng=map_leaflet$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lng,lat=map_leaflet$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lat)
      
      clearGroup(map, group = "legende_joignantes")
    }
    
    if(any(class(map) %in% "leaflet") & !is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }else
    {
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
                           options = pathOptions(pane = "fond_legende", clickable = F),
                           fill = T,
                           fillColor = "white",
                           fillOpacity = 0.8,
                           group="legende_joignantes"
      )
      
      map <- addPolygons(map = map,
                         data=flux_legWGS84,
                         stroke = TRUE,
                         opacity = 1,
                         color = colBorder,
                         weight = 1,
                         options = pathOptions(pane = "fond_legende", clickable = F),
                         fill = T,
                         fillColor = colFleche,
                         fillOpacity = 1,
                         group="legende_joignantes",
                         layerId=list(titre=titre,lng=lng,lat=lat,nom_fond="fond_flux_leg",zoom=zoom)
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
                                 group="legende_joignantes"
      )
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe2[1], lat = pointe2[2], #petite fleche
                                 label = as.character(format(round(vmax/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group="legende_joignantes"
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
                                 group="legende_joignantes"
      )
      
      
    }
    
    message(simpleMessage(paste0("Les coordonnees de la legende des fleches joignantes sont : longitude (x) = ",lng," degre ; latitude (y) = ",lat," degre")))
    
    return(map)
  }
