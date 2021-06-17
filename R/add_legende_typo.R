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
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]=="legende_typo") idx_legende <- c(idx_legende,i)
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

      # Coordonnees du point haut/gauche des rectangles de la legende
      decalage <- 0.7
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

      # On ajoute un cadre blanc autour de la legende
      y_coord_rectangle <- min(get(paste0("rectangle_",nb_classes))[[1]][,2])

      # leaflet rectangles et valeurs classes
      label_rectangle <- NULL
      txt <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[5]]
      aa <- sapply(1:length(txt), function(x) label_rectangle <<- c(label_rectangle,substring(txt[x],str_locate_all(txt[x],">")[[1]][11]+1,nchar(txt[x])-15)))
      label_rectangle <- unique(label_rectangle)

      if(is.null(labels))
      {
        labels <- label_rectangle
      }

      if(!is.null(map_leaflet))
      {
        map_leaflet <- map
        map <- map_proxy
        clearGroup(map, group = "legende_typo")
      }

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
                           group="legende_typo"
                           )

      for(i in 1:nb_classes)
      {
        map <- addLabelOnlyMarkers(map = map,
                                   lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                   label = labels[i],
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "12px"
                                                               )),
                                   group="legende_typo"
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
                           group="legende_typo"
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
                                 group="legende_typo"
      )
    }

    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende de la typologie sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
    return(map)
  }
