set_couleur_joignantes <-
function(map,colFleche="#CD853F",colBorder="black")
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(colFleche)) if(any(class(colFleche)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error3 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    idx_fleche <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux") idx_fleche <- i
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux_leg") idx_legende <- i
      }
    }
    
    if(!is.null(idx_fleche))
    {
      map$x$calls[[idx_fleche]]$args[[4]]$fillColor <- colFleche
      map$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder
    }
    
    if(!is.null(idx_legende))
    {
      map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colFleche
      map$x$calls[[idx_legende]]$args[[4]]$color <- colBorder
    }
    
    return(map)
  }
