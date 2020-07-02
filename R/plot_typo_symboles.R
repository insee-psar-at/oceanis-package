plot_typo_symboles <-
  function(fondPoints,listFonds,emprise="FRM",types=NULL,couleurs=NULL,tailles=NULL,epaisseurs=NULL,titreLeg="",xLeg=NULL,yLeg=NULL,cadreLeg=FALSE,xLimCadreLeg=NULL,yLimCadreLeg=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,labels=NULL,xlim=NULL,ylim=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres

    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20 <- NULL

    if(any(!any(class(fondPoints) %in% "sf"),!any(class(fondPoints) %in% "data.frame"))) msg_error1 <- "Le fond de points doit etre un objet sf / "
    if(any(!any(class(listFonds[[1]]) %in% "sf"),!any(class(listFonds[[1]]) %in% "data.frame"))) msg_error2 <- "La liste des fonds doit etre une liste d'objets sf / "
    if(any(class(emprise)!="character")) msg_error3 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974' ou '976') / "
    if(!is.null(types)) if(any(class(types)!="numeric")) msg_error4 <- "Le type des symboles doit etre un vecteur de valeurs numeriques (de 0 a 25 - voir aide) / "
    if(!is.null(couleurs)) if(any(class(couleurs)!="character")) msg_error5 <- "La couleur des symboles doit etre un vecteur de chaines de caracteres (nommee ou hexadecimal) / "
    if(!is.null(tailles)) if(any(class(tailles)!="numeric")) msg_error6 <- "La taille des symboles doit etre un vecteur de valeurs numeriques / "
    if(!is.null(epaisseurs)) if(any(class(epaisseurs)!="numeric")) msg_error7 <- "L'epaisseur des symboles doit etre un vecteur de valeurs numeriques / "
    if(any(class(titreLeg)!="character")) msg_error8 <- "Le titre de la legende doit etre de type caractere / "
    if(!is.null(xLeg)) if(any(class(xLeg)!="numeric")) msg_error9 <- "La variable xLeg doit etre de type numerique / "
    if(!is.null(yLeg)) if(any(class(yLeg)!="numeric")) msg_error10 <- "La variable yLeg doit etre de type numerique / "
    if(any(class(cadreLeg)!="logical")) msg_error11 <- "La variable cadreLeg doit etre logique TRUE ou FALSE / "
    if(!is.null(xLimCadreLeg)) if(any(class(xLimCadreLeg)!="numeric")) msg_error12 <- "La variable xLimCadreLeg doit etre de type numerique / "
    if(!is.null(yLimCadreLeg)) if(any(class(yLimCadreLeg)!="numeric")) msg_error13 <- "La variable yLimCadreLeg doit etre de type numerique / "
    if(any(class(titreCarte)!="character")) msg_error14 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error15 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error16 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(!emprise %in% c("FRM","971","972","973","974","976")) msg_error17 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974' ou '976' / "
    if(!is.null(labels)) if(any(class(labels)!="character")) msg_error18 <- "Les labels doivent etre un vecteur de type caractere / "
    if(!is.null(xlim)) if(any(class(xlim)!="numeric")) msg_error19 <- "La variable xlim doit etre de type numerique / "
    if(!is.null(ylim)) if(any(class(ylim)!="numeric")) msg_error20 <- "La variable yim doit etre de type numerique / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20)))
    }

    names(fondPoints)[1] <- "CODE"
    names(fondPoints)[2] <- "LIBELLE"
    fondPoints$LIBELLE<-iconv(fondPoints$LIBELLE,"latin1","utf8")

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

    code_epsg <- switch(emprise, #emprise
                        "FRM"="2154",# Lambert 93
                        "971"="5490",# UTM 20 N
                        "972"="5490",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S

    if(is.null(types)) types <- rep(15,nrow(fondPoints))
    if(is.null(couleurs)) couleurs <- rep("red",nrow(fondPoints))
    if(is.null(tailles)) tailles <- rep(2,nrow(fondPoints))
    if(is.null(epaisseurs)) epaisseurs <- rep(2,nrow(fondPoints))

    if(is.null(labels)) labels <- as.data.frame(fondPoints)[,"LIBELLE"]

    symbLeg <- unique(data.frame(types,couleurs,tailles,epaisseurs))

    x <- NULL
    y <- NULL
    for(i in 1:length(listFonds))
    {
      x <- c(x,sf::st_bbox(listFonds[[i]])$xmax-sf::st_bbox(listFonds[[i]])$xmin)
      y <- c(y,sf::st_bbox(listFonds[[i]])$ymax-sf::st_bbox(listFonds[[i]])$ymin)
    }
    xMax <- max(x)
    yMax <- max(y)
    ixMax <- which.max(x)
    iyMax <- which.max(y)

    if(is.null(xlim)) xlim <- c(sf::st_bbox(listFonds[[ixMax]])$xmin,sf::st_bbox(listFonds[[ixMax]])$xmax+x_marge*3)
    if(is.null(ylim)) ylim <- c(sf::st_bbox(listFonds[[iyMax]])$ymin,sf::st_bbox(listFonds[[iyMax]])$ymax+y_marge*3)

    decalageLeg <- (xlim[2]-xlim[1])/20

    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- sf::st_bbox(listFonds[[ixMax]])$xmax
      yLeg <- sf::st_bbox(listFonds[[iyMax]])$ymax
    }

    for(i in 1:nrow(symbLeg))
    {
      if(i>1)
      {
        yLeg <- c(yLeg,yLeg[length(yLeg)]-decalageLeg)
      }
    }

    xLeg <- c(xLeg,rep(xLeg,nrow(symbLeg)-1))

    pointsLeg <- data.frame(lng=xLeg,lat=yLeg,stringsAsFactors = FALSE)
    listPointsLeg <- apply(pointsLeg,1, function(x) sf::st_sf(geometry=sf::st_sfc(sf::st_point(x),crs=paste0("+init=epsg:",code_epsg))))
    fondPointsLeg <- do.call("rbind",listPointsLeg)

    x_marge <- xMax/20
    y_marge <- yMax/20

    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- xlim[2]-(xlim[2]-xlim[1])/10
      yLeg <- ylim[2]-(ylim[2]-ylim[1])/10
    }
    x_large <- (xlim[2]-xlim[1])/20
    y_large <- x_large/1.5

    xmin <- min(sf::st_coordinates(fondPointsLeg)[,1]) - x_large
    xmax <- max(sf::st_coordinates(fondPointsLeg)[,1]) + (x_large*5)
    ymin <- min(sf::st_coordinates(fondPointsLeg)[,2]) - (y_large*2)
    ymax <- max(sf::st_coordinates(fondPointsLeg)[,2]) + (y_large*3)

    if(cadreLeg)
    {
      if(is.null(xLimCadreLeg) | is.null(xLimCadreLeg))
      {
        bbox_leg_typo_symboles <- matrix(c(xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin, xmin,ymax),ncol=2, byrow=TRUE)
        bbox_leg_typo_symboles <- st_sf(geometry=st_sfc(st_polygon(list(bbox_leg_typo_symboles))),crs=st_crs(fondPoints))
      }else
      {
        bbox_leg_typo_symboles <- matrix(c(xLimCadreLeg[1],yLimCadreLeg[2], xLimCadreLeg[2],yLimCadreLeg[2], xLimCadreLeg[2],yLimCadreLeg[1], xLimCadreLeg[1],yLimCadreLeg[1], xLimCadreLeg[1],yLimCadreLeg[2]),ncol=2, byrow=TRUE)
        bbox_leg_typo_symboles <- st_sf(geometry=st_sfc(st_polygon(list(bbox_leg_typo_symboles))),crs=st_crs(fondPoints))
      }
    }

    if(!is.null(etiquettes))
    {
      tableEtiquettes <- table_etiquettes(listFonds[[1]],etiquettes)
    }

    par(mai=c(0,0,0,0))

    for(i in 1:length(listFonds))
    {
      names_fond <- names(as.data.frame(listFonds[[i]]))
      if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(listFonds[[i]])[,"COL"]) else colFond <- "transparent"
      if(any(names_fond %in% "BORDER")) colBorder <- unique(as.data.frame(listFonds[[i]])[,"BORDER"]) else colBorder <- "black"
      if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(listFonds[[i]])[,"EPAISSEUR"]) else epaisseur <- 1

      if(i==1)
      {
        plot(sf::st_geometry(listFonds[[1]]),xlim=xlim,ylim=ylim,col=colFond,border=colBorder,lwd=epaisseur)
      }else
      {
        plot(sf::st_geometry(listFonds[[i]]),col=colFond,border=colBorder,lwd=epaisseur,add=T)
      }
    }

    for(i in 1:nrow(fondPoints))
    {
      plot(sf::st_geometry(fondPoints[i,]),pch=types[i],col=couleurs[i],border=colBorder[i],cex=tailles[i],lwd=epaisseurs[i],add=T)
    }

    if(!is.null(etiquettes))
    {
      for(i in 1:nrow(tableEtiquettes))
      {
        text(tableEtiquettes[i,"X"],tableEtiquettes[i,"Y"],labels=tableEtiquettes[i,"LIBELLE"],cex=tableEtiquettes[i,"TAILLE"],col=tableEtiquettes[i,"COL"],font=tableEtiquettes[i,"FONT"])
      }
    }

    if(cadreLeg) suppressWarnings(plot(bbox_leg_typo_symboles,add=T,col="white",border="white",lwd=1))

    for(i in 1:nrow(symbLeg))
    {
      plot(sf::st_geometry(fondPointsLeg[i,]),pch=types[i],col=couleurs[i],border=colBorder[i],cex=tailles[i],lwd=epaisseurs[i],add=T)
      text(pointsLeg[i,1]+decalageLeg,pointsLeg[i,2],labels=labels[i],cex=0.7,adj=0)
    }

    text(pointsLeg[1,1],pointsLeg[1,2]+decalageLeg,labels=titreLeg,cex=1,adj=0)

    if(titreCarte!="")
    {
      text((xlim[1]+xlim[2])/2,ylim[2]-y_marge/2,labels=titreCarte)
    }

    if(sourceCarte!="")
    {
      text(xlim[1]+(xlim[2]-xlim[1])/6,ylim[1]+y_marge/2,labels=sourceCarte,cex=0.7)
    }

    message(simpleMessage(paste0("[INFO] Les coordonnees de la legende sont x = ", xLeg[1]," metres ; y = ", yLeg[1]," metres")))

    return("")
  }
