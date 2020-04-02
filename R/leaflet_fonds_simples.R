leaflet_fonds_simples <-
function(listFonds,popup=NULL,init=TRUE,map=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    leafletVerifParamFondsSimples(listFonds,popup,init,map)

    if(is.null(popup)) popup <- c(1:length(listFonds))

    if(!init) groupe <- "carte_fonds" else groupe <- "carte_fonds_init"

    # CONSTRUCTION DE LA MAP EN LEAFLET

    if(is.null(map))
    {
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
        if(any(!is.na(listFonds[[i]])))
        {
          unFond <- listFonds[[i]]

          if(any(popup %in% i)) affiche_popup <- TRUE else affiche_popup <- FALSE

          if(affiche_popup)
          {
            names(unFond)[2] <- "LIBELLE"
            unFond$LIBELLE<-iconv(unFond$LIBELLE,"latin1","utf8")
            libelle <- as.data.frame(unFond)$LIBELLE
            clic <- T
          }else
          {
            libelle <- NULL
            clic <- F
          }

          unFond <- st_transform(unFond,"+init=epsg:4326 +proj=longlat +ellps=WGS84")

          map <- addPolygons(map = map, data = unFond, opacity = i/length(listFonds),
                             stroke = TRUE, color = "black",
                             weight = 1.5,
                             popup = libelle,
                             options = pathOptions(clickable = clic),
                             fill = T, fillColor = "white", fillOpacity = 1,
                             group = groupe
          )
        }
      }
    }else
    {
      for(i in 1:length(listFonds))
      {
        if(any(!is.na(listFonds[[i]])))
        {
          unFond <- listFonds[[i]]

          if(any(popup %in% i)) affiche_popup <- TRUE else affiche_popup <- FALSE

          if(affiche_popup)
          {
            names(unFond)[2] <- "LIBELLE"
            unFond$LIBELLE<-iconv(unFond$LIBELLE,"latin1","utf8")
            libelle <- as.data.frame(unFond)$LIBELLE
            clic <- T
          }else
          {
            libelle <- NULL
            clic <- F
          }

          unFond <- st_transform(unFond,"+init=epsg:4326 +proj=longlat +ellps=WGS84")

          map <- addPolygons(map = map, data = unFond, opacity = i/length(listFonds),
                             stroke = TRUE, color = "black",
                             weight = 1.5,
                             popup = libelle,
                             options = pathOptions(clickable = clic),
                             fill = T, fillColor = "white", fillOpacity = 1,
                             group = groupe
          )
        }
      }
    }

    return(map)
  }
