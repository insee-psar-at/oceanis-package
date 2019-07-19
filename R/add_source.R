add_source <-
function(map, source, map_leaflet = NULL)
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(source)) if(any(class(titre)!="character")) msg_error2 <- "La source doit etre de type caractere / "
    if (!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error3 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }
    
    idx_source <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(any(map$x$calls[[i]]$args[[4]]=="map-sourc")) idx_source <- i
      }
    }
    
    tag.map.sourc <- tags$style(HTML("
                                     .leaflet-control.map-sourc {
                                     position: fixed !important;
                                     left: 2%;
                                     text-align: center;
                                     font-size: 12px;
                                     }
                                     "))
    
    if(!is.null(source))
    {
      source <- iconv(source,"latin1","utf8")
      
      sourc <- tags$div(
        tag.map.sourc, HTML(source)
      )
      
      if(is.null(idx_source)) # on n'a pas encore mis de source
      {
        if(!is.null(map_leaflet)) map <- map_proxy
        map <- addControl(map = map, sourc, position = "bottomright", className="map-sourc")
      }else # une source existe deja, on la remplace
      {
        ancienne_source <- map$x$calls[[idx_source]]$args[[1]]
        map$x$calls[[idx_source]]$args[[1]] <- paste0(
          substr(ancienne_source,1,str_locate(ancienne_source,"</style>\n")[2]+2),
          source,
          substr(ancienne_source,str_locate(ancienne_source,"\n</div>")[1],nchar(ancienne_source))
        )
      }
    }else # la source est NULL, on la supprime si elle existe
    {
      if(!is.null(idx_source))# une source existe deja, on la supprime
      {
        map$x$calls[[idx_source]]$args[[1]] <- ""
        map$x$calls[[idx_source]]$args[[4]] <- ""
      }
    }
    
    return(map)
  }
