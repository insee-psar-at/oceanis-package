set_fonds_simples <-
function(map,colRemplissageFonds=NULL,colBordureFonds=NULL,transparenceFonds=NULL,epaisseurFonds=NULL,map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6 <- NULL
    
    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(colRemplissageFonds)) if(any(class(colRemplissageFonds)!="character")) msg_error2 <- "Le vecteur de couleurs de remplissage doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(colBordureFonds)) if(any(class(colBordureFonds)!="character")) msg_error3 <- "Le vecteur de couleurs de bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(transparenceFonds)) if(any(class(transparenceFonds)!="numeric")) msg_error4 <- "Le vecteur de transparence doit etre de type numeric / "
    if(!is.null(epaisseurFonds)) if(any(class(epaisseurFonds)!="numeric")) msg_error5 <- "Le vecteur d'epaisseur de bordure doit etre de type numeric / "
    if (!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error6 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),!is.null(msg_error6)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6)))
    }

    if(is.null(map_leaflet)) # contexte leaflet
    {
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]] %in% c("carte_fonds","carte_fonds_init")) idx_carte <- c(idx_carte,i)
        }
      }
      
      if(!is.null(idx_carte))
      {
        for(i in 1:length(idx_carte))
        {
          if(!is.na(colRemplissageFonds[i])) map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor <- colRemplissageFonds[i]
          if(!is.na(colBordureFonds[i])) map$x$calls[[idx_carte[i]]]$args[[4]]$color <- colBordureFonds[i]
          if(!is.na(transparenceFonds[i])) map$x$calls[[idx_carte[i]]]$args[[4]]$fillOpacity <- transparenceFonds[i]
          if(!is.na(epaisseurFonds[i])) map$x$calls[[idx_carte[i]]]$args[[4]]$weight <- epaisseurFonds[i]
        }
      }
    }else # contexte proxy
    {
      idx_carte <- NULL
      for(i in 1:length(map_leaflet$x$calls))
      {
        if(map_leaflet$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map_leaflet$x$calls[[i]]$args[[3]] %in% "carte_fonds") idx_carte <- c(idx_carte,i)
        }
      }
      
      clearGroup(map, group = "carte_fonds")
      
      for(i in 1:length(idx_carte))
      {
        unPoly <- lapply(1:length(map_leaflet$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map_leaflet$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map_leaflet$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        unFond <- list()
        for(j in 1:length(unPoly))
        {
          unFond[[j]] <- st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(unPoly[[j]]), function(x) unPoly[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        }
        unFond <- do.call(rbind,unFond)
        
        rm(unPoly)
      
        clic <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$interactive
        if(clic)
        {
          libelles <- map_leaflet$x$calls[[idx_carte[i]]]$args[[5]]
        }else
        {
          libelles <- NULL
        }
        
        remp <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
        col <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$color
        transp <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$fillOpacity
        epais <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$weight
        
        if(!is.na(colRemplissageFonds[i])) remp <- colRemplissageFonds[i]
        if(!is.na(colBordureFonds[i])) col <- colBordureFonds[i]
        if(!is.na(transparenceFonds[i])) transp <- transparenceFonds[i]
        if(!is.na(epaisseurFonds[i])) epais <- epaisseurFonds[i]
        
        map <- addPolygons(map = map, data = unFond, opacity = i/length(idx_carte),
                           stroke = TRUE, color = col,
                           weight = epais,
                           popup = libelles,
                           options = pathOptions(clickable = clic),
                           fill = T, fillColor = remp, fillOpacity = transp/100,
                           group = "carte_fonds"
        )
      }
    }
    
    return(map)
  }
