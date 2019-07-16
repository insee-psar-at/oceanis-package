add_legende_typo_symboles <-
function(map,titre=NULL,lng=NULL,lat=NULL,labels=NULL,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(labels)) if(any(class(labels)!="character")) msg_error4 <- "Les labels doivent etre un vecteur de type caractere / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    if(!is.null(labels))
    {
      labels<-iconv(labels,"latin1","utf8")
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
    
    code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
    
    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
    
    nbSymbLeg <- length(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[5]]$types)
    
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
          png(f, width = width, height = height, bg = "transparent")
          par(mar = c(0, 0, 0, 0))
          plot.new()
          points(0.5, 0.5, pch = pch[i], cex = min(width, height) / 8, ...)
          dev.off()
          files[i] <- f
        }
        files
      }
      
      for(i in 1:nbSymbLeg)
      {
        iconFiles <- pchIcons(pch = map$x$calls[[idx_marker[i]]]$args[[5]]$types[i], width = map$x$calls[[idx_marker[i]]]$args[[5]]$tailles[i], height = map$x$calls[[idx_marker[i]]]$args[[5]]$tailles[i], col = map$x$calls[[idx_marker[i]]]$args[[5]]$couleurs[i], lwd = map$x$calls[[idx_marker[i]]]$args[[5]]$epaisseurs[i])
        
        map <- addMarkers(map = map, lng = lng[i], lat = lat[i], icon = icons(iconUrl = iconFiles, iconAnchorX = map$x$calls[[idx_marker[i]]]$args[[5]]$tailles[i]/2, iconAnchorY = map$x$calls[[idx_marker[i]]]$args[[5]]$tailles[i]/2),
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
