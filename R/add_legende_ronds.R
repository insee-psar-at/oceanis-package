add_legende_ronds <-
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
      idx_carte <- NULL
      idx_legende <- NULL

      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(any(map$x$calls[[i]]$args[[3]] %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(any(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende)) # la legende existe
        {
          if(map$x$calls[[i]]$method=="addPolylines")
          {
            if(map$x$calls[[i]]$args[[3]]=="legende_ronds") idx_legende <- c(idx_legende,i)
          }
          if(map$x$calls[[i]]$method %in% "addMarkers")
          {
            if(map$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende <- c(idx_legende,i)
          }
        }
      }

      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$code_epsg

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
        if(is.null(rayonRond)) rayonRond <- 1
        if(rayonRond==0) rayonRond <- 1

        max_var <- map$x$calls[[j]]$args[[4]]$max_var
        max_var <- as.numeric(str_replace_all(max_var,",","."))
        
        lng_init <- lng
        lat_init <- lat
        if(is.null(lng_init) | is.null(lat_init))
        {
          lng <- map$x$fitBounds[[4]]
          lat <- map$x$fitBounds[[3]]
        }
      }
    }else if(any(class(map) %in% "leaflet_proxy"))
    {
      idx_carte <- NULL
      idx_legende <- NULL

      for(i in 1:length(map_leaflet$x$calls))
      {
        if(map_leaflet$x$calls[[i]]$method %in% "addPolygons")
        {
          if(any(map_leaflet$x$calls[[i]]$args[[3]] %in% "carte_classes")) idx_carte <- c(idx_carte,i)
        }
        if(map_leaflet$x$calls[[i]]$method %in% "addCircles")
        {
          if(any(map_leaflet$x$calls[[i]]$args[[5]] %in% "carte_ronds")) idx_carte <- c(idx_carte,i)
        }
        if(map_leaflet$x$calls[[i]]$method %in% "addCircles")
        {
          if(map_leaflet$x$calls[[i]]$args[[5]] %in% "legende_ronds") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende)) # la legende existe
        {
          if(map_leaflet$x$calls[[i]]$method=="addPolylines")
          {
            if(map_leaflet$x$calls[[i]]$args[[3]]=="legende_ronds") idx_legende <- c(idx_legende,i)
          }
          if(map_leaflet$x$calls[[i]]$method %in% "addMarkers")
          {
            if(map_leaflet$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende <- c(idx_legende,i)
          }
        }
      }

      code_epsg <- map_leaflet$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$code_epsg

      j <- NULL
      for(i in 1:length(map_leaflet$x$calls))
      {
        if(map_leaflet$x$calls[[i]]$method %in% "addCircles")
        {
          if(any(map_leaflet$x$calls[[i]]$args[[5]] %in% "carte_ronds")) j <- i
        }
      }

      rayonRond <- max(map_leaflet$x$calls[[j]]$args[[3]])
      if(is.null(rayonRond)) rayonRond <- 1
      if(rayonRond==0) rayonRond <- 1

      max_var <- map_leaflet$x$calls[[j]]$args[[4]]$max_var
      max_var <- as.numeric(str_replace_all(max_var,",","."))

      lng_init <- lng
      lat_init <- lat
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map_leaflet$x$fitBounds[[4]]
        lat <- map_leaflet$x$fitBounds[[3]]
      }

      clearGroup(map, group = "legende_ronds")
    }

    if(any(class(map) %in% "leaflet") & !is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }else
    {
      ronds_leg <- construction_ronds_legende(lng,lat,code_epsg,rayonRond)

      ronds_sf_leg <- ronds_leg[[1]]

      lignes <- construction_lignes_legende(ronds_leg,coeff,code_epsg)
      
      # On ajoute un cadre blanc autour de la legende
      bbox_ronds <- st_bbox(ronds_leg[[2]])
      bbox_lignes <- st_bbox(lignes[[2]])
      rectangle <- c(bbox_ronds[1],bbox_ronds[2],bbox_lignes[3],bbox_ronds[4])
      large <- rectangle[3]-rectangle[1]
      rectangle[1] <- rectangle[1] - large / 3
      rectangle[2] <- rectangle[2] - large / 3
      rectangle[3] <- rectangle[3] + large / 6 * nchar(max_var)
      rectangle[4] <- rectangle[4] + large / 2
      
      vec <- matrix(c(rectangle[1],rectangle[2],   rectangle[3],rectangle[2],   rectangle[3],rectangle[4],   rectangle[1],rectangle[4],   rectangle[1],rectangle[2]),5,2,byrow=T)
      rectangle <- st_sfc(st_polygon(list(vec)), crs = as.numeric(code_epsg))
      
      rectangle <- st_transform(rectangle, crs = 4326)
      
      # leaflet du cadre blanc en 1er
      map <- addPolygons(map = map,
                         data = rectangle,
                         stroke = FALSE,
                         options = pathOptions(pane = "fond_legende", clickable = F),
                         fill = T,
                         fillColor = "white",
                         fillOpacity = 0.8,
                         group = "legende_ronds"
      )
      
      suppressWarnings(map <- addCircles(map = map,
                                         lng = st_coordinates(st_centroid(ronds_sf_leg))[,1],
                                         lat = st_coordinates(st_centroid(ronds_sf_leg))[,2],
                                         stroke = TRUE,
                                         opacity = 1,
                                         color = "#2B3E50",
                                         weight = 2,
                                         radius = c(rayonRond,rayonRond/sqrt(3)),
                                         options = pathOptions(pane = "fond_legende", clickable = F),
                                         popup = c(max_var,round(max_var/3,0)),
                                         fill = T,
                                         fillColor = "white",
                                         fillOpacity = 1,
                                         group = "legende_ronds",
                                         layerId = list(code_epsg=code_epsg,nom_fond="fond_ronds_leg")
                              )
      )

      # leaflet lignes
      map <- addPolylines(map = map,
                          data = lignes[[1]],
                          stroke = TRUE,
                          opacity = 1,
                          color = "#2B3E50",
                          weight = 2,
                          options = pathOptions(pane = "fond_legende", clickable = F),
                          fill = F,
                          fillOpacity = 1,
                          group = "legende_ronds",
                          layerId = list(code_epsg=code_epsg,nom_fond="fond_lignes")
      )

      # leaflet valeur ronds
      map <- addLabelOnlyMarkers(map = map,
                                 lng = st_bbox(lignes[[1]][1,])[3],
                                 lat = st_bbox(lignes[[1]][1,])[4], #ligne_grand
                                 label = as.character(format(round(max_var,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group = "legende_ronds",
                                 layerId = list(code_epsg=code_epsg)
      )

      map <- addLabelOnlyMarkers(map = map,
                                 lng = st_bbox(lignes[[1]][2,])[3],
                                 lat = st_bbox(lignes[[1]][2,])[4], #ligne_petit
                                 label = as.character(format(round(max_var/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group = "legende_ronds",
                                 layerId = list(code_epsg=code_epsg)
      )

      # leaflet titre
      
      rectangle <- st_transform(rectangle, crs = as.numeric(code_epsg))
      
      pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(rectangle)[,"X"]) + large/6,
                                                max(st_coordinates(rectangle)[,"Y"]) - large/4))),
                         crs = as.numeric(code_epsg))
      pt_titre <- st_transform(pt_titre, crs = 4326)
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = st_coordinates(pt_titre)[1],
                                 lat = st_coordinates(pt_titre)[2],
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group = "legende_ronds",
                                 layerId = list(code_epsg=code_epsg)
      )
    }

    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende des ronds sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))

    return(map)
  }
