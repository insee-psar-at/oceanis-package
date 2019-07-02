add_legende_ronds <-
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
      if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
    }
    if(map$x$calls[[i]]$method %in% "addCircles")
    {
      if(any(map$x$calls[[i]]$args[5][[1]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
    }
    if(map$x$calls[[i]]$method %in% "addCircles")
    {
      if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
    }
    if(!is.null(idx_legende)) # la legende existe
    {
      if(map$x$calls[[i]]$method=="addPolylines")
      {
        if(map$x$calls[[i]]$args[3][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
      }
      if(map$x$calls[[i]]$method %in% "addMarkers")
      {
        if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
      }
    }
  }
  
  code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[5]]$code_epsg
  
  lng_init <- lng
  lat_init <- lat
  if(!is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
  {
    map$x$calls <- map$x$calls[-idx_legende]
  }else
  {
    if(!is.null(idx_legende)) map$x$calls <- map$x$calls[-idx_legende] # Si la legende existe, on la supprime pour la recreer
  
    j <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        j <- i
      }
    }
    
    rayonRond <- max(map$x$calls[[j]]$args[[3]])
    
    max_var <- map$x$calls[[j]]$args[[5]]$max_var
    max_var <- as.numeric(str_replace_all(max_var,",","."))
    
    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
    
    lng_init <- lng
    lat_init <- lat
    if(is.null(lng_init) | is.null(lat_init))
    {
      lng <- map$x$fitBounds[[4]]
      lat <- map$x$fitBounds[[3]]
    }
    
    ronds_leg <- construction_ronds_legende(lng,lat,code_epsg,rayonRond)
    
    ronds_sf_leg <- ronds_leg[[1]]
    
    # Pour le leaflet en WGS84
    x1_grand <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])]
    y1_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
    pts1_grand <- c(x1_grand,y1_grand)
    x2_grand <- x1_grand+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_grand)+coeff*0.5
    y2_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
    pts2_grand <- c(x2_grand,y2_grand)
    ligne_grand <- rbind(pts1_grand,pts2_grand)
    
    x1_petit <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])]
    y1_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
    pts1_petit <- c(x1_petit,y1_petit)
    x2_petit <- x1_petit+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_petit)+coeff*0.5
    y2_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
    pts2_petit <- c(x2_petit,y2_petit)
    ligne_petit <- rbind(pts1_petit,pts2_petit)
    
    lignes <- st_multilinestring(list(ligne_grand,ligne_petit))
    
    suppressWarnings(map <- addCircles(map = map,
                                       lng = st_coordinates(st_centroid(ronds_sf_leg))[,1],
                                       lat = st_coordinates(st_centroid(ronds_sf_leg))[,2],
                                       stroke = TRUE,
                                       opacity = 1,
                                       color = "#2B3E50",
                                       weight = 2,
                                       radius = c(rayonRond,rayonRond/sqrt(3)),
                                       options = pathOptions(clickable = F),
                                       popup = c(max_var,round(max_var/3,0)),
                                       fill = T,
                                       fillColor = "white",
                                       fillOpacity = 1,
                                       group = list(nom_couche="legende_ronds",code_epsg=code_epsg,nom_fond="fond_ronds_leg_carte"))
    )
    
    # leaflet lignes
    map <- addPolylines(map = map, data = lignes,
                       stroke = TRUE,
                       opacity = 1,
                       color = "#2B3E50",
                       weight = 2,
                       options = pathOptions(clickable = F),
                       fill = F,
                       fillOpacity = 1,
                       group = list(nom_couche="legende_ronds",code_epsg=code_epsg,nom_fond="fond_lignes_leg")
    )
    
    # leaflet valeur ronds
    map <- addLabelOnlyMarkers(map = map,
                               lng = ligne_grand[2,1], lat = ligne_grand[2,2], #ligne_grand
                               label = as.character(format(round(max_var,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                               labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                           style = list(
                                                             "color" = "black",
                                                             "font-size" = "12px"
                                                           )),
                               group = list(nom_couche="legende_ronds",code_epsg=code_epsg)
    )
    
    map <- addLabelOnlyMarkers(map = map,
                               lng = ligne_petit[2,1], lat = ligne_petit[2,2], #ligne_petit
                               label = as.character(format(round(max_var/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                               labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                           style = list(
                                                             "color" = "black",
                                                             "font-size" = "12px"
                                                           )),
                               group = list(nom_couche="legende_ronds",code_epsg=code_epsg)
    )
    
    if(!is.null(titre))
    {
      # On ajoute un titre a la legende
      x_titre_1 <- min(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])
      y_titre_1 <- y1_grand+coeff*0.8
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = x_titre_1, lat = y_titre_1,
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group = list(nom_couche="legende_ronds",code_epsg=code_epsg)
      )
    }
  }
  
  message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende des ronds sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
  return(map)
}
