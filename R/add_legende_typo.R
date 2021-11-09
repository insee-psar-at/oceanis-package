add_legende_typo <-
function(map,titre=NULL,lng=NULL,lat=NULL,labels=NULL,zoom=8,map_leaflet=NULL)
  {
    # Verification des parametres

    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL

    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(labels)) if(any(class(labels)!="character")) msg_error4 <- "Les labels doivent etre un vecteur de type caractere / "
    if (!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error5 <- "La carte doit etre un objet leaflet / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5)))
    }

    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    if(!is.null(labels))
    {
      labels<-iconv(labels,"latin1","utf8")
    }

    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }

    idx_carte <- NULL
    idx_legende <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="carte_typo") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_typo_rectangle") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_typo") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }

    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

    lng_init <- lng
    lat_init <- lat
    if(is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init))) # La legende n'a pas encore ete creee, on la cree avec une position par defaut
    {
      lng <- map$x$fitBounds[[4]]
      lat <- map$x$fitBounds[[3]]
    }else if(is.null(idx_legende)) # La legende n'a pas encore ete creee, on la cree avec la position definie par l'utilisateur
    {
      # voir plus loin
    }else # l'utilisateur veut modifier la legende existante, on la supprime pour la recreer
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }

    if(is.null(idx_legende) | !is.null(idx_legende) & !(is.null(lng_init) | is.null(lat_init))) # Si la legende doit etre creee ou recreee
    {
      # on calcule idx_carte au cas oC9 la legende ait ete supprimee, c'est le nombre de polygons dans le leaflet
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          idx_carte <- c(idx_carte,i)
        }
      }

      nb_classes <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
      pal_classes <- unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor)
      code_epsg <- as.numeric(substr(map$x$options$crs$code,6,9))
      
      # Coordonnees du point haut/gauche des rectangles de la legende
      
      pt <- st_sfc(st_geometry(st_point(c(lng,lat))), crs = 4326)
      pt <- st_transform(pt, crs = code_epsg)
      coord_pt <- st_coordinates(pt)[1:2]
      
      position_leg <- t(data.frame(c(coord_pt[1],coord_pt[2])))
      
      # On cree les rectangles
      
      pt_up <- st_sfc(st_geometry(st_point(c(map$x$fitBounds[[2]],map$x$fitBounds[[3]]))), crs = 4326)
      pt_up <- st_transform(pt_up, crs = code_epsg)
      pt_down <- st_sfc(st_geometry(st_point(c(map$x$fitBounds[[2]],map$x$fitBounds[[1]]))), crs = 4326)
      pt_down <- st_transform(pt_down, crs = code_epsg)
      
      large <- abs(st_coordinates(pt_up)[2] - st_coordinates(pt_down)[2]) / 20
      
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
                                             crs = code_epsg))
      }

      # leaflet rectangles et valeurs classes
      label_rectangle <- NULL
      txt <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[5]]
      aa <- sapply(1:length(txt), function(x) label_rectangle <<- c(label_rectangle,substring(txt[x],str_locate_all(txt[x],">")[[1]][11]+1,nchar(txt[x])-15)))
      label_rectangle <- unique(label_rectangle)

      if(is.null(labels))
      {
        labels <- label_rectangle
      }

      ltext <- max(nchar(labels)) / 1.5
      
      vec <- matrix(c(position_leg[1] - large / 2,                     position_leg[2] + large * 2,
                      position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] + large * 2,
                      position_leg[1] + large * 1.5 + (large * ltext), position_leg[2] - large * (nb_classes + 3.5),
                      position_leg[1] - large / 2,                     position_leg[2] - large * (nb_classes + 3.5),
                      position_leg[1] - large / 2,                     position_leg[2] + large * 2),
                    5,2,byrow=T)
      
      rectangle <- st_sfc(st_polygon(list(vec)), crs = code_epsg)
      
      rectangle <- st_transform(rectangle, crs = 4326)
      
      if(!is.null(map_leaflet))
      {
        map_leaflet <- map
        map <- map_proxy
        clearGroup(map, group = "legende_typo")
      }

      # leaflet du cadre blanc en 1er
      map <- addPolygons(map = map,
                         data = rectangle,
                         stroke = FALSE,
                         options = pathOptions(pane = "fond_legende", clickable = F),
                         fill = T,
                         fillColor = "white",
                         fillOpacity = 0.5,
                         group = "legende_typo_rectangle"
      )

      # On cree les polygones de la legende
      for(i in 1: nb_classes)
      {
        map <- addPolygons(map = map,
                           data = st_transform(get(paste0("rectangle_",i)), crs = 4326),
                           stroke = FALSE,
                           options = pathOptions(pane = "fond_legende", clickable = F),
                           fill = T,
                           fillColor = pal_classes[i],
                           fillOpacity = 1,
                           group = "legende_typo"
        )
        
        pt_label <- st_sfc(st_geometry(st_point(c(max(st_coordinates(get(paste0("rectangle_",i))[[1]])[,1]) + large / 10,
                                                  mean(st_coordinates(get(paste0("rectangle_",i))[[1]])[,2])))),
                           crs = code_epsg)
        pt_label <- st_transform(pt_label, crs = 4326)
        
        map <- addLabelOnlyMarkers(map = map,
                                   lng = st_coordinates(pt_label)[1],
                                   lat = st_coordinates(pt_label)[2],
                                   label = labels[i],
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "12px"
                                                               )),
                                   group = "legende_typo"
        )
      }

      # leaflet titre
      pt_titre <- st_sfc(st_geometry(st_point(c(min(st_coordinates(pt)[,"X"]),
                                                max(st_coordinates(pt)[,"Y"]) + large))),
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
                                 group = "legende_typo"
      )
    }

    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende de la typologie sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
    return(map)
  }
