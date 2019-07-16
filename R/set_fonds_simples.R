set_fonds_simples <-
function(map,colRemplissageFonds=NULL,colBordureFonds=NULL,transparenceFonds=NULL,epaisseurFonds=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(colRemplissageFonds)) if(any(class(colRemplissageFonds)!="character")) msg_error2 <- "Le vecteur de couleurs de remplissage doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(colBordureFonds)) if(any(class(colBordureFonds)!="character")) msg_error3 <- "Le vecteur de couleurs de bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(transparenceFonds)) if(any(class(transparenceFonds)!="numeric")) msg_error4 <- "Le vecteur de transparence doit etre de type numeric / "
    if(!is.null(epaisseurFonds)) if(any(class(epaisseurFonds)!="numeric")) msg_error5 <- "Le vecteur d'epaisseur de bordure doit etre de type numeric / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5)))
    }
    
    idx_carte <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche=="carte_fonds") idx_carte <- c(idx_carte,i)
      }
    }
    
    if(!is.null(idx_carte))
    {
      for(i in 1:length(idx_carte))
      {
        map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor <- colRemplissageFonds[i]
        map$x$calls[[idx_carte[i]]]$args[[4]]$color <- colBordureFonds[i]
        map$x$calls[[idx_carte[i]]]$args[[4]]$fillOpacity <- transparenceFonds[i]
        map$x$calls[[idx_carte[i]]]$args[[4]]$weight <- epaisseurFonds[i]
      }
    }
    
    return(map)
  }
