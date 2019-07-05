set_couleur_classes <-
function(map,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL,colBorder="white")
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(any(class(stylePalette)!="character")) msg_error2 <- "Le style de la palette doit etre de type caractere ('InseeFlash', 'InseeAnalyse', 'InseeDossier', 'InseePremiere' ou 'defaut') / "
    if(!is.null(palettePos)) if(any(class(palettePos)!="character")) msg_error3 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(!is.null(paletteNeg)) if(any(class(paletteNeg)!="character")) msg_error4 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error5 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5)))
    }
    
    if(is.null(palettePos) & is.null(paletteNeg))
    {
      palette <- recup_palette(stylePalette)
      inseePos <- palette[[1]]
      inseeNeg <- palette[[2]]
    }else
    {
      inseePos <- NULL
      inseeNeg <- NULL
      if(!is.null(palettePos)) inseePos <- palettePos
      if(!is.null(paletteNeg)) inseeNeg <- paletteNeg
    }
    
    idx_carte <- NULL
    idx_legende <- NULL
    ronds <- F
    legende <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[[3]]$nom_couche %in% c("carte_classes","carte_ronds_classes","carte_classes_ronds")))
        {
          if(any(map$x$calls[[i]]$args[[3]]$nom_fond %in% c("fond_maille_carte","fond_maille_elargi_carte")))
            idx_carte <- c(idx_carte,i)
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% "carte_ronds_classes")
        {
          idx_carte <- c(idx_carte,i)
          ronds <- T
        }
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") legende <- T
      }
      if(legende) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    for(i in 1:length(idx_carte))
    {
      if(!ronds)
      {
        bornes <- map$x$calls[[idx_carte[i]]]$args[[3]]$bornes
        arg <- 4
        style_anc <- map$x$calls[[idx_carte[i]]]$args[[3]]$style
        map$x$calls[[idx_carte[i]]]$args[[3]]$style <- stylePalette
      }else
      {
        bornes <- map$x$calls[[idx_carte[i]]]$args[[5]]$bornes
        arg <- 6
        style_anc <- map$x$calls[[idx_carte[i]]]$args[[5]]$style
        map$x$calls[[idx_carte[i]]]$args[[5]]$style <- stylePalette
      }
      
      palette <- recup_palette(stylePalette=style_anc)
      inseePos_anc <- palette[[1]]
      inseeNeg_anc <- palette[[2]]
      
      nb_col_pos <- length(bornes[bornes>0])
      nb_col_neg <- length(bornes[bornes<0])
      
      couleur_pos <- NULL
      couleur_neg <- NULL
      if(!is.null(inseePos) & nb_col_pos>0) couleur_pos <- inseePos[c((length(inseePos)-nb_col_pos+1):length(inseePos))]
      if(!is.null(inseeNeg) & nb_col_neg>0) couleur_neg <- inseeNeg[c(1:nb_col_neg)]
      pal_new <- c(couleur_pos,couleur_neg)
      
      couleur_pos_anc <- NULL
      couleur_neg_anc <- NULL
      if(!is.null(inseePos_anc) & nb_col_pos>0) couleur_pos_anc <- inseePos_anc[c((length(inseePos_anc)-nb_col_pos+1):length(inseePos_anc))]
      if(!is.null(inseeNeg_anc) & nb_col_neg>0) couleur_neg_anc <- inseeNeg_anc[c(1:nb_col_neg)]
      pal_anc <- data.frame(col=c(couleur_pos_anc,couleur_neg_anc))
      
      couleur_analyse <- data.frame(col=map$x$calls[[idx_carte[i]]]$args[[arg]]$fillColor)
      couleur_analyse$id1 <- c(1:nrow(couleur_analyse))
      
      pal_anc$id2 <- c(1:nrow(pal_anc))
      couleur_analyse <- merge(couleur_analyse,pal_anc,by="col")
      
      aa <- sapply(1:(length(pal_new)), function(x) couleur_analyse[couleur_analyse$id2==x,"col"] <<- pal_new[x])
      rm(aa)
      couleur_analyse <- couleur_analyse[order(couleur_analyse$id1),]
      couleur_analyse <- couleur_analyse$col
      
      map$x$calls[[idx_carte[i]]]$args[[arg]]$fillColor <- couleur_analyse
      
      map$x$calls[[idx_carte[i]]]$args[[arg]]$color <- colBorder
    }
    
    if(!is.null(idx_legende))
    {
      for(i in 1:length(idx_legende))
      {
        map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor <- pal_new[i]
      }
    }
    return(map)
  }
