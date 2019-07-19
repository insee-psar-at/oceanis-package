set_couleur_typo <-
function(map,paletteTypo=NULL,colBorder="white")
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(paletteTypo)) if(any(class(paletteTypo)!="character")) msg_error2 <- "La palette de la typologie doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error3 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    if(is.null(paletteTypo))
    {
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="carte_typo") idx_carte <- c(idx_carte,i)
        }
      }
      nb_col <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
      paletteTypo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
    }
    
    idx_carte <- NULL
    idx_legende <- NULL
    legende <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche=="carte_typo")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_maille_typo_carte") idx_carte <- i
        }
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_typo") legende <- T
      }
      if(legende) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(!is.null(idx_carte))
    {
      couleur_analyse <- data.frame(col=map$x$calls[[idx_carte]]$args[[4]]$fillColor)
      couleur_analyse$id1 <- c(1:nrow(couleur_analyse))
      pal_anc <- data.frame(col=unique(couleur_analyse$col))
      pal_anc$id2 <- c(1:nrow(pal_anc))
      couleur_analyse <- merge(couleur_analyse,pal_anc,by="col")
      
      aa <- sapply(1:(length(paletteTypo)), function(x) couleur_analyse[couleur_analyse$id2==x,"col"] <<- paletteTypo[x])
      rm(aa)
      couleur_analyse <- couleur_analyse[order(couleur_analyse$id1),]
      couleur_analyse <- couleur_analyse$col
      
      map$x$calls[[idx_carte]]$args[[4]]$fillColor <- couleur_analyse
      
      map$x$calls[[idx_carte]]$args[[4]]$color <- colBorder
    }
    
    if(legende)
    {
      for(i in 1:length(idx_legende))
      {
        map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor <- paletteTypo[i]
      }
    }
    return(map)
  }
