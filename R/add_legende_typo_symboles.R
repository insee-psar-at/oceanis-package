add_legende_typo_symboles <-
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
    if(any(Encoding(titre) %in% "latin1")){
      titre<-iconv(titre,"latin1","UTF-8")
    }
    if(!is.null(labels))
    {
      if(any(Encoding(labels) %in% "latin1")){
        labels<-iconv(labels,"latin1","UTF-8")
      }
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
      if(map$x$calls[[i]]$method %in% "addMarkers")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="carte_typo_symb") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addMarkers")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_typo_symb") idx_legende <- c(idx_legende,i)
      }
    }

    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude

    types <- NULL
    couleurs <- NULL
    tailles <- NULL
    epaisseurs <- NULL
    for(i in 1:length(idx_carte))
    {
      types <- c(types,map$x$calls[[idx_carte[i]]]$args[[5]]$types)
      couleurs <- c(couleurs,map$x$calls[[idx_carte[i]]]$args[[5]]$couleurs)
      tailles <- c(tailles,map$x$calls[[idx_carte[i]]]$args[[5]]$tailles)
      epaisseurs <- c(epaisseurs,map$x$calls[[idx_carte[i]]]$args[[5]]$epaisseurs)
    }
    symbLeg <- unique(data.frame(types, couleurs, tailles, epaisseurs))
    nbSymbLeg <- nrow(symbLeg)

    lng_init <- lng
    lat_init <- lat
    if(is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init))) # La legende n'a pas encore ete creee, on la cree avec une position par defaut
    {
      lng <- map$x$limits$lng[2]-1
      lat <- map$x$limits$lat[2]-1

      for(i in 1:nbSymbLeg)
      {
        if(i>1)
        {
          lat <- c(lat,lat[length(lat)]-coeff*2)
        }
      }
      lng <- c(lng,rep(lng,nbSymbLeg-1))

    }else if(is.null(idx_legende)) # La legende n'a pas encore ete creee, on la cree avec la position definie par l'utilisateur
    {
      for(i in 1:nbSymbLeg)
      {
        if(i>1)
        {
          lat <- c(lat,lat[length(lat)]-coeff*2)
        }
      }
      lng <- c(lng,rep(lng,nbSymbLeg-1))
    }else # l'utilisateur veut modifier la legende existante, on la supprime pour la recreer
    {
      map$x$calls <- map$x$calls[-idx_legende]

      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map$x$limits$lng[2]-1
        lat <- map$x$limits$lat[2]-1
      }

      for(i in 1:nbSymbLeg)
      {
        if(i>1)
        {
          lat <- c(lat,lat[length(lat)]-coeff*2)
        }
      }
      lng <- c(lng,rep(lng,nbSymbLeg-1))
    }

    if(is.null(idx_legende) | !is.null(idx_legende) & !(is.null(lng_init) | is.null(lat_init))) # Si la legende doit etre creee ou recreee
    {
      # on calcule idx_carte au cas ou la legende ait ete supprimee, c'est le nombre de polygons dans le leaflet
      idx_carte <- NULL
      idx_marker <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          idx_marker <- c(idx_marker,i)
        }
      }

      #voir aide de ?graphics::points
      pchIcons <- function(pch = 0:25, width = 30, height = 30, ...) {
        n <- length(pch)
        files <- character(n)
        # create a sequence of png images
        for (i in seq_len(n)) {
          f <- tempfile(fileext = ".png")
          suppressWarnings(png(f, width = width, height = height, bg = "transparent"))
          par(mar = c(0, 0, 0, 0))
          plot.new()
          points(0.5, 0.5, pch = pch[i], cex = min(width, height) / 8, ...)
          dev.off()
          files[i] <- f
        }
        files
      }

      if(!is.null(map_leaflet)) map <- map_proxy

      for(i in 1:nbSymbLeg)
      {
        iconFiles <- pchIcons(pch = symbLeg$types[i],
                              width = symbLeg$tailles[i],
                              height = symbLeg$tailles[i],
                              col = symbLeg$couleurs[i],
                              lwd = symbLeg$epaisseurs[i])
        iconX <- symbLeg$tailles[i]/2
        iconY <- symbLeg$tailles[i]/2

        map <- addMarkers(map = map, lng = lng[i], lat = lat[i], icon = icons(iconUrl = iconFiles, iconAnchorX = iconX, iconAnchorY = iconY),
                          options = pathOptions(clickable = F),
                          group = list(nom_couche="legende_typo_symb"))

        map <- addLabelOnlyMarkers(map = map,
                                   lng = lng[i]+coeff*1.5, lat = lat[i],
                                   label = labels[i],
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "12px"
                                                               )),
                                   group=list(nom_couche="legende_typo_symb")
        )
      }

      # leaflet titre
      map <- addLabelOnlyMarkers(map = map,
                                 lng = lng[1], lat = lat[1]+coeff*2,
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group=list(nom_couche="legende_typo_symb")
      )

      unlink(iconFiles)
    }

    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende de la typologie de symboles sont : longitude (x) = ",lng[1]," degr\u00e9 ; latitude (y) = ",lat[1]," degr\u00e9")))
    return(map)
  }
