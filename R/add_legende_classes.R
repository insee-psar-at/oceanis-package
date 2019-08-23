add_legende_classes <-
function(map,titre=NULL,lng=NULL,lat=NULL,typeLegende=1,zoom=8,map_leaflet=NULL)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6 <- NULL
    
    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(any(class(typeLegende)!="numeric")) msg_error4 <- "Le type de legende doit etre de type numerique (1:litterale ou 2:en echelle) / "
    if(!typeLegende %in% c(1,2)) msg_error5 <- "La variable typeLegende doit etre 1 ou 2 / "
    if (!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error6 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),!is.null(msg_error6)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }
    
    idx_carte <- NULL
    idx_legende <- NULL
    ronds <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[[3]] %in% "carte_classes")) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_ronds_classes_carte","fond_ronds_classes_elargi_carte"))
        {
          idx_carte <- c(idx_carte,i)
          ronds <- T
        }
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]=="legende_classes") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
    }

    if(ronds) arg <- 4 else arg <- 2
    code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$code_epsg
    
    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
    
    lng_init <- lng
    lat_init <- lat
    if(is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init))) # La legende n'a pas encore ete creee, on la cree avec une position par defaut
    {
      lng <- map$x$fitBounds[[4]]
      lat <- map$x$fitBounds[[1]]
    }else if(is.null(idx_legende)) # La legende n'a pas encore ete creee, on la cree avec la position definie par l'utilisateur
    {
      # voir plus loin
    }else # l'utilisateur veut modifier la legende existante, on la supprime pour la recreer
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }
    
    if(is.null(idx_legende) | !is.null(idx_legende) & !(is.null(lng_init) | is.null(lat_init))) # Si la legende doit etre creee ou recreee
    {
      # on calcule idx_carte au cas ou la legende ait ete supprimee, c'est le nombre de polygons dans le leaflet
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_ronds_classes_carte","fond_ronds_classes_elargi_carte"))
          {
            idx_carte <- c(idx_carte,i)
          }
        }
      }
      
      if(!ronds)
      {
        nb_classes <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
        pal_classes <- unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor)
      }else
      {
        nb_classes <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[6]]$fillColor))
        pal_classes <- unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[6]]$fillColor)
      }
      
      palette <- recup_palette(stylePalette=map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$style)
      pal_classes_pos <- palette[[1]]
      pal_classes_neg <- palette[[2]]
      pal_classes_pos <- pal_classes_pos[pal_classes_pos %in% pal_classes]
      pal_classes_neg <- pal_classes_neg[pal_classes_neg %in% pal_classes]
      pal_classes <- c(pal_classes_pos,pal_classes_neg)
      
      # Coordonnees du point haut/gauche des rectangles de la legende
      if(typeLegende==1) decalage <- 0.7 else decalage <- 0.5
      x_coord_rectangle <- lng
      for(i in 1:nb_classes)
      {
        if(i==1) #1er rectangle
        {
          y_coord_rectangle <- lat-coeff
        }else
        {
          y_coord_rectangle <- y_coord_rectangle-coeff*decalage
        }
        assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
      }
      
      if(ronds) arg <- 4 else arg <- 2
      precision <- as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$precision)
      
      if(!is.null(map_leaflet))
      {
        map_leaflet <- map
        map <- map_proxy
        clearGroup(map, group = "legende_classes")
      }
      
      if(typeLegende==1) # Litterale
      {
        # On ajoute un cadre blanc autour de la legende
        y_coord_rectangle <- min(get(paste0("rectangle_",nb_classes))[[1]][,2])
        
        # leaflet du cadre blanc en 1er
        map <- addRectangles(map = map,
                             lng1 = lng-coeff*0.5, lat1 = lat+coeff*0.5,
                             lng2 = x_coord_rectangle+coeff*10, lat2 = y_coord_rectangle-coeff*0.8,
                             stroke = TRUE,
                             color = paste0("#2B3E50", ";background: #ffffff;
                                            border-left:2px solid #2B3E50;
                                            border-right:2px solid #2B3E50;
                                            border-top:2px solid #2B3E50;
                                            border-bottom:2px solid #2B3E50;
                                            border-radius: 5%"),
                             weight = 1,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = "white",
                             fillOpacity = 0.5,
                             group = "legende_classes"
                             )
        
        # leaflet rectangles et valeurs classes
        label_rectangle <- NULL
        
        if(!is.null(map_leaflet))
        {
          map_proxy <- map
          map <- map_leaflet
        }
        
        for(i in 1:nb_classes)
        {
          if(i==1)
          {
            label_rectangle <- c(label_rectangle,paste0(format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i+1]),precision), big.mark=" ",decimal.mark=",",nsmall=0)," et plus"))
          }else if(i==nb_classes)
          {
            label_rectangle <- c(label_rectangle,paste0("Moins de ", format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i]),precision), big.mark=" ",decimal.mark=",",nsmall=0)))
          }else
          {
            label_rectangle <- c(label_rectangle,paste0("De ", format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i+1]),precision), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i]),precision), big.mark=" ",decimal.mark=",",nsmall=0)))
          }
        }
        
        if(!is.null(map_leaflet))
        {
          map_leaflet <- map
          map <- map_proxy
        }
        
        for(i in 1:nb_classes)
        {
          map <- addLabelOnlyMarkers(map = map,
                                     lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                     label = label_rectangle[i],
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group = "legende_classes"
          )
        }
        
        # On cree les polygons ensemble a la fin de l'objet leaflet juste avant le titre
        
        for(i in 1:nb_classes)
        {
          map <- addPolygons(map = map, data = st_polygon(get(paste0("rectangle_",i))),
                             stroke = FALSE,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = pal_classes[i],
                             fillOpacity = 1,
                             group = "legende_classes",
                             layerId = list(typeLegende=typeLegende, zoom=zoom)
          )
        }
        
        # leaflet titre
        x_titre <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
        y_titre <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.4
        
        map <- addLabelOnlyMarkers(map = map,
                                   lng = x_titre, lat = y_titre,
                                   label = titre,
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "14px"
                                                               )),
                                   group = "legende_classes"
        )
      }
      
      if(typeLegende==2) # En echelle
      {
        # On ajoute un cadre blanc autour de la legende
        y_coord_rectangle <- min(get(paste0("rectangle_",nb_classes))[[1]][,2])
        
        # leaflet du cadre blanc en 1er
        map <- addRectangles(map = map,
                             lng1 = lng-coeff*0.5, lat1 = lat+coeff*0.5,
                             lng2 = x_coord_rectangle+coeff*5, lat2 = y_coord_rectangle-coeff*0.8,
                             stroke = TRUE,
                             color = paste0("#2B3E50", ";background: #ffffff;
                                            border-left:2px solid #2B3E50;
                                            border-right:2px solid #2B3E50;
                                            border-top:2px solid #2B3E50;
                                            border-bottom:2px solid #2B3E50;
                                            border-radius: 5%"),
                             weight = 1,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = "white",
                             fillOpacity = 0.5,
                             group = "legende_classes"
                             )
        
        if(ronds) arg <- 4 else arg <- 2
        
        if(!is.null(map_leaflet))
        {
          bornes <- map_leaflet$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes
        }else
        {
          bornes <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes
        }
        
        for(i in 1:nb_classes)
        {
          if(i<nb_classes)
          {
            x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
            y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
            x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
            y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
            ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
            
            map <- addPolylines(map = map, data = ligne,
                                color = "black",
                                weight = 1,
                                options = pathOptions(pane = "fond_legende", clickable = F),
                                fill = F,
                                fillOpacity = 1,
                                group = "legende_classes"
            )
            
            map <- addLabelOnlyMarkers(map = map,
                                       lng = x2, lat = y2,
                                       label = as.character(format(round(as.numeric(bornes[i+1]),precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "12px"
                                                                   )),
                                       group = "legende_classes"
            )
          }
        }
        
        # On cree les polygons ensemble a la fin de l'objet leaflet juste avant le titre
        for(i in 1:nb_classes)
        {
          map <- addPolygons(map = map, data = st_polygon(get(paste0("rectangle_",i))),
                             stroke = FALSE,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = pal_classes[i],
                             fillOpacity = 1,
                             group = "legende_classes",
                             layerId = list(typeLegende=typeLegende, zoom=zoom)
          )
        }
        
        # leaflet titre
        x_titre <- min(get("rectangle_1")[[1]][,1])
        y_titre <- max(get("rectangle_1")[[1]][,2])+coeff*0.6
        
        map <- addLabelOnlyMarkers(map = map,
                                   lng = x_titre, lat = y_titre,
                                   label = titre,
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "14px"
                                                               )),
                                   group = "legende_classes"
        )
      }
    }
    
    if(!is.null(map_leaflet))
    {
      message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende des classes sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
    }
    
    return(map)
  }
