set_couleur_ronds <-
function(map,colorPos="#CD853F",colorNeg="#6495ED",colBorder="white")
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(colorPos)) if(any(class(colorPos)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(colorNeg)) if(any(class(colorNeg)!="character")) msg_error3 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error4 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    idx_carte <- NULL
    classes <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))
        {
          idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% c("carte_ronds_classes","carte_classes_ronds"))
        {
          classes <- T
        }
      }
    }
    
    for(i in 1:length(idx_carte))
    {
      if(!classes)
      {
        val_pos <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],max(str_locate_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11),">")[[1]])+1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11)," ",""))>=0)
        
        if(length(val_pos)>0)
        {
          map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor[1:length(map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor)] <- colorPos
        }
        
        val_neg <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],max(str_locate_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11),">")[[1]])+1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11)," ",""))<0)
        
        if(length(val_neg)>0)
        {
          map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor[1:length(map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor)] <- colorNeg
        }
      }
      
      map$x$calls[[idx_carte[i]]]$args[[6]]$color <- colBorder
    }
    return(map)
  }
