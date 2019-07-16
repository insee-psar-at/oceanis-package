leaflet_fonds_simples <-
function(listFonds)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2 <- NULL
    
    if(any(class(listFonds)!="list")) msg_error1 <- "Le parametre listFOnds doit etre une liste / "
    if(any(!any(class(listFonds[[1]]) %in% "sf"),!any(class(listFonds[[1]]) %in% "data.frame"))) msg_error2 <- "Le parametre listFonds doit etre un objet sf / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }
    
    # CONSTRUCTION DE LA MAP EN LEAFLET
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">OCEANIS - \u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    for(i in 1:length(listFonds))
    {
      unFond <- listFonds[[i]][,c(1,ncol(listFonds[[i]]))]
      
      names(unFond)[1] <- "LIBELLE"
      
      unFond$LIBELLE<-iconv(unFond$LIBELLE,"latin1","utf8")
      
      unFond <- st_transform(unFond,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      
      map <- addPolygons(map = map, data = unFond, opacity = i/length(listFonds),
                         stroke = TRUE, color = "black",
                         weight = 1.5,
                         popup = as.data.frame(unFond)$LIBELLE,
                         options = pathOptions(clickable = T),
                         fill = T, fillColor = "white", fillOpacity = 1,
                         group = list(nom_couche="carte_fonds")
      )
    }
    
    return(map)
  }
