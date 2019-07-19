plot_classes <-
function(data,fondMaille,fondSousAnalyse=NULL,fondSurAnalyse=NULL,idData,varRatio,methode="kmeans",nbClasses=3,bornes=NULL,precisionLegClasses=1,titreLeg="",xLeg=NULL,yLeg=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL,colBorder="white",xlim=NULL,ylim=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24<-msg_error25<-msg_error26<-msg_error27 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSousAnalyse)) if(any(!any(class(fondSousAnalyse[[1]]) %in% "sf"),!any(class(fondSousAnalyse[[1]]) %in% "data.frame"))) msg_error3 <- "Les fonds a positionner en-dessous de l'analyse doivent etre une liste d'objets sf / "
    if(!is.null(fondSurAnalyse)) if(any(!any(class(fondSurAnalyse[[1]]) %in% "sf"),!any(class(fondSurAnalyse[[1]]) %in% "data.frame"))) msg_error4 <- "Les fonds a positionner au-dessus de l'analyse doivent etre une liste d'objets sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(methode)!="character")) msg_error7 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error9 <- "La variable doit etre un vecteur numerique / "
    if(any(class(precisionLegClasses)!="numeric")) msg_error10 <- "La variable precisionLegClasses doit etre de type numerique / "
    if(any(class(titreLeg)!="character")) msg_error11 <- "Le titre de la legende doit etre de type caractere / "
    if(!is.null(xLeg)) if(any(class(xLeg)!="numeric")) msg_error12 <- "La variable xLeg doit etre de type numerique / "
    if(!is.null(yLeg)) if(any(class(yLeg)!="numeric")) msg_error13 <- "La variable yLeg doit etre de type numerique / "
    if(any(class(titreCarte)!="character")) msg_error14 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error15 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error16 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(any(class(stylePalette)!="character")) msg_error17 <- "Le style de la palette doit etre de type caractere ('InseeFlash', 'InseeAnalyse', 'InseeDossier', 'InseePremiere' ou 'defaut') / "
    if(!is.null(palettePos)) if(any(class(palettePos)!="character")) msg_error18 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(!is.null(paletteNeg)) if(any(class(paletteNeg)!="character")) msg_error19 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error20 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(xlim)) if(any(class(xlim)!="numeric")) msg_error21 <- "La variable xlim doit etre de type numerique / "
    if(!is.null(ylim)) if(any(class(ylim)!="numeric")) msg_error22 <- "La variable yim doit etre de type numerique / "
    
    if(length(names(data))<2) msg_error23 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error24 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error25 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error26 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error27 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
           !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24),
           !is.null(msg_error25),!is.null(msg_error26),!is.null(msg_error27)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                              msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,msg_error23,msg_error24,msg_error25,msg_error26,msg_error27)))
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSousAnalyse)) 
    {
      for(i in 1:length(fondSousAnalyse))
      {
        names(fondSousAnalyse[[i]])[1] <- "CODE"
        names(fondSousAnalyse[[i]])[2] <- "LIBELLE"
        fondSousAnalyse[[i]]$LIBELLE<-iconv(fondSousAnalyse[[i]]$LIBELLE,"latin1","utf8")
      }
    }
    if(!is.null(fondSurAnalyse)) 
    {
      for(i in 1:length(fondSurAnalyse))
      {
        names(fondSurAnalyse[[i]])[1] <- "CODE"
        names(fondSurAnalyse[[i]])[2] <- "LIBELLE"
        fondSurAnalyse[[i]]$LIBELLE<-iconv(fondSurAnalyse[[i]]$LIBELLE,"latin1","utf8")
      }
    }
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    if(titreLeg!="")
    {
      titreLeg<-iconv(titreLeg,"latin1","utf8")
    }
    if(titreCarte!="")
    {
      titreCarte<-iconv(titreCarte,"latin1","utf8")
    }
    if(sourceCarte!="")
    {
      sourceCarte<-iconv(sourceCarte,"latin1","utf8")
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
      stylePalette <- NULL
      if(!is.null(palettePos)) inseePos <- palettePos
      if(!is.null(paletteNeg)) inseeNeg <- paletteNeg
    }
    
    data[,varRatio] <- round(data[,varRatio],precisionLegClasses)
    
    analyse <- merge(fondMaille[,c("CODE","geometry")],data,by="CODE")
    
    max <- max(as.data.frame(analyse)[,varRatio])
    min <- min(as.data.frame(analyse)[,varRatio])
    
    if(is.null(bornes))
    {
      suppressWarnings(bornes_analyse <- classIntervals(as.numeric(as.data.frame(analyse)[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"))
      
      if(!is.null(stylePalette))
      {
        carac_bornes <- calcul_bornes(as.data.frame(analyse),bornes_analyse,varRatio,nbClasses,methode,stylePalette=stylePalette)
      }else
      {
        carac_bornes <- calcul_bornes(as.data.frame(analyse),bornes_analyse,varRatio,nbClasses,methode,stylePalette=stylePalette,palettePos=palettePos,paletteNeg=paletteNeg)
      }
      
      bornes <- carac_bornes[[1]]
      bornes[1] <- max(as.numeric(as.data.frame(analyse)[,varRatio]))
      bornes_sansext <- bornes[-1]
      bornes_sansext <- bornes_sansext[-length(bornes_sansext)]
      bornes_sansext <- sort(bornes_sansext, decreasing = TRUE)
      bornes <- c(max,bornes_sansext,min)
      bornes <- round(bornes,precisionLegClasses)
      
      pal_classes <- carac_bornes[[2]]
    }else
    {
      bornes_sansext <- sort(bornes, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precisionLegClasses)
      
      if(min<0 & max>=0) # Si - et +
      {
        pal_classes_pos <- inseePos[(length(inseePos)-length(bornes[bornes>0])+1):length(inseePos)]
        pal_classes_neg <- inseeNeg[1:length(bornes[bornes<0])]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }
      if(min>=0) # Si +
      {
        if(length(bornes)>=(length(inseePos)+1))
        {
          pal_classes <- inseePos
        }else
        {
          pal_classes <- inseePos[-c(1:(length(inseePos)-length(bornes)+1))] # On enleve les couleurs fonces inutiles
        }
      }
      if(max<0) # Si -
      {
        if(length(bornes)>=(length(inseeNeg)+1))
        {
          pal_classes <- inseeNeg
        }else
        {
          pal_classes <- inseeNeg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
        }
      }
    }
    
    if(is.null(pal_classes)) pal_classes <- "grey"
    
    pal_classes[is.na(pal_classes)] <- "grey"
    palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
    col <- palette(as.data.frame(analyse)[,varRatio])
    
    analyse <- cbind(as.data.frame(analyse)[,-ncol(analyse)],PALETTE=col,geometry=analyse$geometry)
    
    ff <- lapply(1:length(pal_classes), function(x) analyse[analyse$PALETTE %in% rev(pal_classes)[x],"classe"] <<- x)
    rm(ff)
    analyse <- analyse[,c(1:(ncol(analyse)-2),ncol(analyse),ncol(analyse)-1)]
    
    fond_classes <- st_as_sf(analyse)
    
    x_marge <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_marge <- (st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20
    
    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- st_bbox(fondMaille)$xmax-(st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/10
      yLeg <- st_bbox(fondMaille)$ymax-(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/10
    }
    x_large <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_large <- x_large/1.5
    
    rectangle <- matrix(c(xLeg-x_large,yLeg,xLeg,yLeg,xLeg,yLeg-y_large,xLeg-x_large,yLeg-y_large,xLeg-x_large,yLeg),ncol=2, byrow=TRUE)
    fond_leg_classes <- st_sf(geometry=st_sfc(st_polygon(list(rectangle))),crs=st_crs(fondMaille))
    for(i in 2:length(pal_classes))
    {
      rectangle <- matrix(c(xLeg-x_large,yLeg-(i-1)*x_large,xLeg,yLeg-(i-1)*x_large,xLeg,(yLeg-(i-1)*x_large)-y_large,xLeg-x_large,(yLeg-(i-1)*x_large)-y_large,xLeg-x_large,yLeg-(i-1)*x_large),ncol=2, byrow=TRUE)
      rectangle <- st_sf(geometry=st_sfc(st_polygon(list(rectangle))),crs=st_crs(fondMaille))
      fond_leg_classes <- rbind(fond_leg_classes,rectangle)
    }
    
    label_rectangle <- NULL
    for(i in 1:length(pal_classes))
    {
      if(i==1)
      {
        label_rectangle <- c(label_rectangle,paste0(format(round(bornes[i+1],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)," et plus"))
      }else if(i==length(pal_classes))
      {
        label_rectangle <- c(label_rectangle,paste0("Moins de ", format(round(bornes[i],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)))
      }else
      {
        label_rectangle <- c(label_rectangle,paste0("De ", format(round(bornes[i+1],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)," a moins de ", format(round(bornes[i],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)))
      }
    }
    
    if(!is.null(etiquettes))
    {
      tableEtiquettes <- table_etiquettes(fondMaille,etiquettes)
    }
    
    if(is.null(xlim)) xlim <- c(st_bbox(fondMaille)$xmin,st_bbox(fondMaille)$xmax+x_marge*3)
    if(is.null(ylim)) ylim <- c(st_bbox(fondMaille)$ymin,st_bbox(fondMaille)$ymax+y_marge*3)
    
    par(mai=c(0,0,0,0))
    plot(st_geometry(fondMaille),xlim=xlim,ylim=ylim,border=colBorderMaille)
    
    if(!is.null(fondSousAnalyse))
    {
      for(i in 1:length(fondSousAnalyse))
      {
        names_fond <- names(as.data.frame(fondSousAnalyse[[i]]))
        if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(fondSousAnalyse[[i]])[,"COL"]) else colFond <- "transparent"
        if(any(names_fond %in% "BORDER")) colBorder <- unique(as.data.frame(fondSousAnalyse[[i]])[,"BORDER"]) else colBorder <- "black"
        if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(fondSousAnalyse[[i]])[,"EPAISSEUR"]) else epaisseur <- 1
        plot(st_geometry(fondSousAnalyse[[i]]),col=colFond,border=colBorder,lwd=epaisseur,add=T)
      }
    }
    
    plot(st_geometry(fondMaille),col="transparent",border=colBorder,add=T)
    
    for(i in 1:(length(bornes)-1))
    {
      suppressWarnings(plot(fond_classes[between(as.data.frame(fond_classes)[,varRatio],bornes[i+1],bornes[i]),],add=T,col=pal_classes[i],border=colBorder,lwd=1))
    }
    
    for(i in 1:length(pal_classes))
    {
      suppressWarnings(plot(st_geometry(fond_leg_classes[i,]),add=T,col=pal_classes[i],border="black",lwd=1))
      text(max(st_coordinates(fond_leg_classes[i,])[,1])+y_large/2,mean(st_coordinates(fond_leg_classes[i,])[,2]),labels=label_rectangle[i],cex=0.9,adj=0)
    }
    
    text(min(st_coordinates(fond_leg_classes[1,])[,1]),max(st_coordinates(fond_leg_classes[1,])[,2])+y_large,labels=titreLeg,cex=1,adj=0)
    
    if(!is.null(fondSurAnalyse))
    {
      for(i in 1:length(fondSurAnalyse))
      {
        names_fond <- names(as.data.frame(fondSurAnalyse[[i]]))
        if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(fondSurAnalyse[[i]])[,"COL"]) else colFond <- "transparent"
        if(any(names_fond %in% "BORDER")) colBorder <- unique(as.data.frame(fondSurAnalyse[[i]])[,"BORDER"]) else colBorder <- "black"
        if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(fondSurAnalyse[[i]])[,"EPAISSEUR"]) else epaisseur <- 1
        plot(st_geometry(fondSurAnalyse[[i]]),col=colFond,border=colBorder,lwd=epaisseur,add=T)
      }
    }
    
    if(!is.null(etiquettes))
    {
      for(i in 1:nrow(tableEtiquettes))
      {
        text(tableEtiquettes[i,"X"],tableEtiquettes[i,"Y"],labels=tableEtiquettes[i,"LIBELLE"],cex=tableEtiquettes[i,"TAILLE"],col=tableEtiquettes[i,"COL"],font=tableEtiquettes[i,"FONT"])
      }
    }
    
    if(titreCarte!="")
    {
      text(((st_bbox(fondMaille)$xmax+x_marge*3)-st_bbox(fondMaille)$xmin)/2,st_bbox(fondMaille)$ymax+y_marge*3,labels=titreCarte)
    }
    
    if(sourceCarte!="")
    {
      text(((st_bbox(fondMaille)$xmax+x_marge*3)-st_bbox(fondMaille)$xmin)/6,st_bbox(fondMaille)$ymin,labels=sourceCarte,cex=0.7)
    }
    
    message(simpleMessage(paste0("[INFO] Les coordonnees de la legende sont x = ",round(xLeg-x_large,2)," metres ; y = ",round(yLeg,2)," metres")))
    
    return(fond_classes)
  }
