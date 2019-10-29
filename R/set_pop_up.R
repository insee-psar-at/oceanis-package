set_pop_up <-
function(map,popupRonds=NULL,popupClasses=NULL)
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(popupRonds)) if(any(class(popupRonds)!="character")) msg_error2 <- "Le parametre popupRonds doit etre un vecteur de caractere (contenu html possible) / "
    if(!is.null(popupClasses)) if(any(class(popupClasses)!="character")) msg_error3 <- "Le parametre popupClasses doit etre un vecteur de caractere (contenu html possible) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    idx_carte_classes <- NULL
    idx_carte_ronds <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes","carte_classes_elargi"))
        {
          idx_carte_classes <- c(idx_carte_classes,i)
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds","carte_ronds_elargi"))
        {
          idx_carte_ronds <- c(idx_carte_ronds,i)
        }
      }
    }
    
    if(length(idx_carte_classes)==2)
    {
      map$x$calls[[idx_carte_classes[1]]]$args[[5]] <- popupClasses
      length_analyse <- length(map$x$calls[[idx_carte_classes[2]]]$args[[5]])
      map$x$calls[[idx_carte_classes[2]]]$args[[5]] <- popupClasses[1:length_analyse]
    }else if(length(idx_carte_classes)==1)
    {
      map$x$calls[[idx_carte_classes[1]]]$args[[5]] <- popupClasses
    }
    
    if(length(idx_carte_ronds)==2)
    {
      map$x$calls[[idx_carte_ronds[1]]]$args[[7]] <- popupRonds
      length_analyse <- length(map$x$calls[[idx_carte_ronds[2]]]$args[[7]])
      map$x$calls[[idx_carte_ronds[2]]]$args[[7]] <- popupRonds[1:length_analyse]
    }else if(length(idx_carte_ronds)==1)
    {
      map$x$calls[[idx_carte_ronds[1]]]$args[[7]] <- popupRonds
    }
    
    return(map)
  }
