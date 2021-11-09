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
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_classes_rectangle") idx_legende <- c(idx_legende,i)
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
          if(!is.null(map$x$calls[[i]]$args[[2]]$nom_fond))
          {
            if(map$x$calls[[i]]$args[[2]]$nom_fond %in% c("fond_pays","fond_france","fond_territoire","fond_maille","fond_maille_carte","fond_maille_elargi","fond_maille_elargi_carte"))
            {
              idx_carte <- c(idx_carte,i)
            }
          }
        }
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(!is.null(map$x$calls[[i]]$args[[4]]$nom_fond))
          {
            if(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_ronds_classes_carte","fond_ronds_classes_elargi_carte"))
            {
              idx_carte <- c(idx_carte,i)
            }
          }
        }
      }

      if(!ronds)
      {
        pal_classes <- rev(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$palette)
      }else
      {
        pal_classes <- rev(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$palette)
      }
      nb_classes <- length(pal_classes)

      if(ronds) arg <- 4 else arg <- 2
      precision <- as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$precision)
      
      pt <- st_sfc(st_geometry(st_point(c(lng,lat))), crs = 4326)
      pt <- st_transform(pt, crs = as.numeric(code_epsg))
      coord_pt <- st_coordinates(pt)[1:2]
      
      pt_up <- st_sfc(st_geometry(st_point(c(map$x$fitBounds[[2]],map$x$fitBounds[[3]]))), crs = 4326)
      pt_up <- st_transform(pt_up, crs = as.numeric(code_epsg))
      pt_down <- st_sfc(st_geometry(st_point(c(map$x$fitBounds[[2]],map$x$fitBounds[[1]]))), crs = 4326)
      pt_down <- st_transform(pt_down, crs = as.numeric(code_epsg))
      
      large <- abs(st_coordinates(pt_up)[2] - st_coordinates(pt_down)[2]) / 20
      
      position_leg <- t(data.frame(c(coord_pt[1],coord_pt[2] - large)))
      
      if(!is.null(map_leaflet))
      {
        map_leaflet <- map
        map <- map_proxy
        clearGroup(map, group = "legende_classes")
      }

      # On cree les rectangles
      
      if(!is.null(map_leaflet))
      {
        bornes <- sort(map_leaflet$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes, decreasing = TRUE)
      }else
      {
        bornes <- sort(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes, decreasing = TRUE)
      }
      
      if(typeLegende==1) # Litterale
      {
        for(i in 1:nb_classes)
        {
          # Coordonnees du point haut/gauche des rectangles de la legende
          x_coord_rectangle <- position_leg[1]
          if(i==1) #1er rectangle
          {
            y_coord_rectangle <- position_leg[2]
          }else
          {
            y_coord_rectangle <- y_coord_rectangle - large - large / 4
          }
          assign(paste0("rectangle_",i),st_sfc(st_polygon(list(matrix(c(x_coord_rectangle,               y_coord_rectangle,
                                                                        x_coord_rectangle + large * 1.5, y_coord_rectangle,
                                                                        x_coord_rectangle + large * 1.5, y_coord_rectangle - large,
                                                                        x_coord_rectangle,               y_coord_rectangle - large,
                                                                        x_coord_rectangle,               y_coord_rectangle),
                                                                      ncol=2, byrow=TRUE))),
                                               crs = as.numeric(code_epsg)))
        }
        
        # On ajoute un cadre blanc autour de la legende
        
        # leaflet rectangles et valeurs classes
        label_rectangle <- c()
        
        for(i in 1:nb_classes)
        {
          if(i==1)
          {
            label_rectangle <- c(label_rectangle,paste0(format(round(as.numeric(bornes[i+1]),precision), big.mark=" ",decimal.mark=",",nsmall=0)," et plus"))
          }else if(i==nb_classes)
          {
            label_rectangle <- c(label_rectangle,paste0("Moins de ", format(round(as.numeric(bornes[i]),precision), big.mark=" ",decimal.mark=",",nsmall=0)))
          }else
          {
            label_rectangle <- c(label_rectangle,paste0("De ", format(round(as.numeric(bornes[i+1]),precision), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(as.numeric(bornes[i]),precision), big.mark=" ",decimal.mark=",",nsmall=0)))
          }
        }
        
        ltext <- max(nchar(label_rectangle)) / 2.5
        
        vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                        position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                        position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (nb_classes + (nb_classes-1)/4 + 1),
                        position_leg[1] - large / 2,                     position_leg[2] - large * (nb_classes + (nb_classes-1)/4 + 1),
                        position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                      5,2,byrow=T)
        
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
                           group = "legende_classes_rectangle"
        )
        
        for(i in 1:nb_classes)
        {
          map <- addPolygons(map = map,
                             data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                             stroke = FALSE,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = pal_classes[i],
                             fillOpacity = 1,
                             group = "legende_classes"
          )
          
          pt_label <- st_sfc(st_geometry(st_point(c(max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large / 10,
                                                    mean(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])))),
                             crs = as.numeric(code_epsg))
          pt_label <- st_transform(pt_label, crs = 4326)
          
          map <- addLabelOnlyMarkers(map = map,
                                     lng = st_coordinates(pt_label)[1],
                                     lat = st_coordinates(pt_label)[2],
                                     label = label_rectangle[i],
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group = "legende_classes"
          )
        }
        
        # On ajoute la legende de classes a l'analyse
        
        # leaflet titre
        
        pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                  max(st_coordinates(pt)[,"Y"])))),
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
                                   group = "legende_classes"
        )
      }

      if(typeLegende==2) # En echelle
      {
        for(i in 1:nb_classes)
        {
          # Coordonnees du point haut/gauche des rectangles de la legende
          x_coord_rectangle <- position_leg[1]
          if(i==1) #1er rectangle
          {
            y_coord_rectangle <- position_leg[2]
          }else
          {
            y_coord_rectangle <- y_coord_rectangle - large
          }
          assign(paste0("rectangle_",i),st_sfc(st_polygon(list(matrix(c(x_coord_rectangle,               y_coord_rectangle,
                                                                        x_coord_rectangle + large * 1.5, y_coord_rectangle,
                                                                        x_coord_rectangle + large * 1.5, y_coord_rectangle - large,
                                                                        x_coord_rectangle,               y_coord_rectangle - large,
                                                                        x_coord_rectangle,               y_coord_rectangle),
                                                                      ncol=2, byrow=TRUE))),
                                               crs = as.numeric(code_epsg)))
        }
        
        # On ajoute un cadre blanc autour de la legende
        
        # leaflet rectangles et valeurs classes
        ltext <- max(nchar(bornes)) / 2.5
        
        vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                        position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                        position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (nb_classes + 1),
                        position_leg[1] - large / 2,                     position_leg[2] - large * (nb_classes + 1),
                        position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                      5,2,byrow=T)
        
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
                           group = "legende_classes_rectangle"
        )
        
        for(i in 1:nb_classes)
        {
          map <- addPolygons(map = map,
                             data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                             stroke = FALSE,
                             options = pathOptions(pane = "fond_legende", clickable = F),
                             fill = T,
                             fillColor = pal_classes[i],
                             fillOpacity = 1,
                             group = "legende_classes"
          )
          
          if(i<nb_classes)
          {
            x1 <- max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1])
            y1 <- min(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])
            x2 <- max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large*0.2
            y2 <- min(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])
            ligne <- st_sfc(st_linestring(rbind(c(x1,y1),c(x2,y2))), crs = as.numeric(code_epsg))
            
            map <- addPolygons(map = map,
                               data = st_transform(ligne, crs = 4326),
                               color = "black",
                               weight = 1,
                               options = pathOptions(pane = "fond_legende", clickable = F),
                               fill = F,
                               fillOpacity = 1,
                               group = "legende_classes"
            )
            
            pt_label <- st_sfc(st_geometry(st_point(c(x2,y2))),
                               crs = as.numeric(code_epsg))
            pt_label <- st_transform(pt_label, crs = 4326)
            
            map <- addLabelOnlyMarkers(map = map,
                                       lng = st_coordinates(pt_label)[1],
                                       lat = st_coordinates(pt_label)[2],
                                       label = as.character(format(round(bornes[i+1],3),big.mark=" ",decimal.mark=",",nsmall=0)),
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "12px"
                                                                   )),
                                       group = "legende_classes"
            )
            
          }
        }
        
        # leaflet titre
        
        pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                  max(st_coordinates(pt)[,"Y"])))),
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
                                   group = "legende_classes"
        )
      }
    }

    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende des classes sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))

    return(map)
  }
