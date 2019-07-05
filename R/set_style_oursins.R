set_style_oursins <-
function(map,epaisseur=2,colTrait="black")
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(any(class(epaisseur)!="numeric")) msg_error2 <- "L'epaisseur du trait doit etre de type numerique / "
    if(any(class(colTrait)!="character")) msg_error3 <- "La couleur du trait doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    idx_fleche <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolylines")
      {
        idx_fleche <- i
      }
    }
    
    map$x$calls[[idx_fleche]]$args[4][[1]]$weight <- epaisseur
    map$x$calls[[idx_fleche]]$args[4][[1]]$color <- colTrait
    
    return(map)
  }
