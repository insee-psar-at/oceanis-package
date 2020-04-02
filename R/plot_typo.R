plot_typo <-
function(data,fondMaille,fondSousAnalyse=NULL,fondSurAnalyse=NULL,idData,varTypo,titreLeg="",xLeg=NULL,yLeg=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,paletteTypo=NULL,labels=NULL,colBorder="white",xlim=NULL,ylim=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres

    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSousAnalyse)) if(any(!any(class(fondSousAnalyse[[1]]) %in% "sf"),!any(class(fondSousAnalyse[[1]]) %in% "data.frame"))) msg_error3 <- "Les fonds a positionner en-dessous de l'analyse doivent etre une liste d'objets sf / "
    if(!is.null(fondSurAnalyse)) if(any(!any(class(fondSurAnalyse[[1]]) %in% "sf"),!any(class(fondSurAnalyse[[1]]) %in% "data.frame"))) msg_error4 <- "Les fonds a positionner au-dessus de l'analyse doivent etre une liste d'objets sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varTypo)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(titreLeg)!="character")) msg_error7 <- "Le titre de la legende doit etre de type caractere / "
    if(!is.null(xLeg)) if(any(class(xLeg)!="numeric")) msg_error8 <- "La variable xLeg doit etre de type numerique / "
    if(!is.null(yLeg)) if(any(class(yLeg)!="numeric")) msg_error9 <- "La variable yLeg doit etre de type numerique / "
    if(any(class(titreCarte)!="character")) msg_error10 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error11 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error12 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(!is.null(paletteTypo)) if(any(class(paletteTypo)!="character")) msg_error13 <- "La palette de la typologie doit etre un vecteur de type caractere / "
    if(!is.null(labels)) if(any(class(labels)!="character")) msg_error14 <- "Les labels doivent etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error15 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(xlim)) if(any(class(xlim)!="numeric")) msg_error16 <- "La variable xlim doit etre de type numerique / "
    if(!is.null(ylim)) if(any(class(ylim)!="numeric")) msg_error17 <- "La variable yim doit etre de type numerique / "

    if(length(names(data))<2) msg_error18 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error19 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!any(names(data) %in% idData))  msg_error20 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varTypo))  msg_error21 <- "La variable a representer n'existe pas dans la table des donnees / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),!is.null(msg_error21)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21)))
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
    if(!is.null(labels))
    {
      labels<-iconv(labels,"latin1","utf8")
    }

    analyse<-k_typo(fondMaille,names(fondMaille)[1],data,"CODE",varTypo)
    analyse <- analyse[[1]]

    table_typo <- unique(as.data.frame(analyse)[,c("classe",varTypo)])
    nb_col <- nrow(table_typo)
    if(is.null(paletteTypo))
    {
      pal_typo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
    }else
    {
      if(length(paletteTypo)>=nb_col)
      {
        pal_typo <- paletteTypo[1:nb_col]
      }else
      {
        pal_typo <- c(paletteTypo,rep("grey",nb_col-length(paletteTypo)))
      }
    }
    pal_typo <- data.frame(cbind(pal_typo,table_typo))
    names(pal_typo) <- c("col","classe","varTypo")
    analyse <- merge(pal_typo,as.data.frame(analyse),by="classe")
    analyse <- analyse[order(as.data.frame(analyse)[,varTypo]),-3]

    fond_typo <- st_as_sf(analyse)

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
    fond_leg_typo <- st_sf(geometry=st_sfc(st_polygon(list(rectangle))),crs=st_crs(fondMaille))
    for(i in 2:length(pal_typo$col))
    {
      rectangle <- matrix(c(xLeg-x_large,yLeg-(i-1)*x_large,xLeg,yLeg-(i-1)*x_large,xLeg,(yLeg-(i-1)*x_large)-y_large,xLeg-x_large,(yLeg-(i-1)*x_large)-y_large,xLeg-x_large,yLeg-(i-1)*x_large),ncol=2, byrow=TRUE)
      rectangle <- st_sf(geometry=st_sfc(st_polygon(list(rectangle))),crs=st_crs(fondMaille))
      fond_leg_typo <- rbind(fond_leg_typo,rectangle)
    }

    label_rectangle <- NULL
    for(i in 1:length(pal_typo$col))
    {
      label_rectangle <- c(label_rectangle,pal_typo[i,"varTypo"])
    }

    if(!is.null(etiquettes))
    {
      tableEtiquettes <- table_etiquettes(fondMaille,etiquettes)
    }

    if(is.null(xlim)) xlim <- c(st_bbox(fondMaille)$xmin,st_bbox(fondMaille)$xmax+x_marge*3)
    if(is.null(ylim)) ylim <- c(st_bbox(fondMaille)$ymin,st_bbox(fondMaille)$ymax+y_marge*3)

    par(mai=c(0,0,0,0))
    plot(st_geometry(fondMaille),xlim=xlim,ylim=ylim,border=colBorder)

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

    for(i in 1:length(pal_typo$col))
    {
      suppressWarnings(plot(fond_typo[as.data.frame(fond_typo)[,"classe"]==i,],add=T,col=pal_typo$col[i],border=colBorder,lwd=1))
    }

    if(is.null(labels))
    {
      for(i in 1:length(pal_typo$col))
      {
        suppressWarnings(plot(st_geometry(fond_leg_typo[i,]),add=T,col=pal_typo$col[i],border="black",lwd=1))
        text(max(st_coordinates(fond_leg_typo[i,])[,1])+y_large/2,mean(st_coordinates(fond_leg_typo[i,])[,2]),labels=label_rectangle[i],cex=0.9,adj=0)
      }
    }else
    {
      for(i in 1:length(pal_typo$col))
      {
        suppressWarnings(plot(st_geometry(fond_leg_typo[i,]),add=T,col=pal_typo$col[i],border="black",lwd=1))
        text(max(st_coordinates(fond_leg_typo[i,])[,1])+y_large/2,mean(st_coordinates(fond_leg_typo[i,])[,2]),labels=labels[i],cex=0.9,adj=0)
      }
    }

    text(min(st_coordinates(fond_leg_typo[1,])[,1]),max(st_coordinates(fond_leg_typo[1,])[,2])+y_large,labels=titreLeg,cex=1,adj=0)

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

    return(fond_typo)
  }
