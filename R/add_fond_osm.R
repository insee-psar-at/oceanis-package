add_fond_osm <-
function(map,opacityAnalyse=1,colTrait="white",epaisseurBordure=1)
  {
    msg_error1 <- msg_error2 <- msg_error3 <- msg_error4 <- NULL
    
    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(opacityAnalyse)!="numeric")) msg_error2 <- "La variable doit etre de type numerique / "
    if(any(class(colTrait)!="character")) msg_error3 <- "Le style de couleur doit etre de type caractere / "
    if(any(class(epaisseurBordure)!="numeric")) msg_error4 <- "L'epaisseur de bordure doit etre de type numerique / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(any(class(map) %in% "leaflet"))
    {
      j <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          j <- c(j,i)
        }
      }
      map$x$calls <- map$x$calls[-j[c(1,2)]]
      j <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          j <- c(j,i)
        }
      }
      map$x$calls[[j[length(j)]]]$args[[4]]$opacity <- opacityAnalyse
      map$x$calls[[j[length(j)]]]$args[[4]]$fillOpacity <- opacityAnalyse
      map$x$calls[[j[length(j)]]]$args[[4]]$color <- colTrait
      map$x$calls[[j[length(j)]]]$args[[4]]$weight <- epaisseurBordure
      
      map <- addTiles(map,
                      urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                      attribution = "OCEANIS - <a href=\"http://www.insee.fr\">INSEE</a>",
                      layerId = 4)
    }else if(any(class(map) %in% "leaflet_proxy"))
    {
      removeTiles(map, layerId = 4)
      map <- addTiles(map,
                      urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                      attribution = "OCEANIS - <a href=\"http://www.insee.fr\">INSEE</a>",
                      layerId = 4)
    }
    
    return(map)
  }
