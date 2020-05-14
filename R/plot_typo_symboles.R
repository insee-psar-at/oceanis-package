plot_typo_symboles <-
function(fondPoints,listFonds,emprise="FRM",types=NULL,couleurs=NULL,tailles=NULL,epaisseurs=NULL,titreLeg="",xLeg=NULL,yLeg=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,labels=NULL,xlim=NULL,ylim=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres

    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17 <- NULL

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
    if(any(class(titreCarte)!="character")) msg_error11 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error12 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error13 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(!emprise %in% c("FRM","971","972","973","974","976")) msg_error14 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974' ou '976' / "
    if(!is.null(labels)) if(any(class(labels)!="character")) msg_error15 <- "Les labels doivent etre un vecteur de type caractere / "
    if(!is.null(xlim)) if(any(class(xlim)!="numeric")) msg_error16 <- "La variable xlim doit etre de type numerique / "
    if(!is.null(ylim)) if(any(class(ylim)!="numeric")) msg_error17 <- "La variable yim doit etre de type numerique / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17)))
    }

    names(fondPoints)[1] <- "CODE"
    names(fondPoints)[2] <- "LIBELLE"
    fondPoints$LIBELLE<-iconv(fondPoints$LIBELLE,"latin1","utf8")

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
      x <- c(x,st_bbox(listFonds[[i]])$xmax-st_bbox(listFonds[[i]])$xmin)
      y <- c(y,st_bbox(listFonds[[i]])$ymax-st_bbox(listFonds[[i]])$ymin)
    }
    xMax <- max(x)
    yMax <- max(y)
    ixMax <- which.max(x)
    iyMax <- which.max(y)

    decalageLeg <- xMax/15

    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- st_bbox(listFonds[[ixMax]])$xmax
      yLeg <- st_bbox(listFonds[[iyMax]])$ymax
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
    listPointsLeg <- apply(pointsLeg,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=paste0("+init=epsg:",code_epsg))))
    fondPointsLeg <- do.call("rbind",listPointsLeg)

    x_marge <- xMax/20
    y_marge <- yMax/20

    if(is.null(xlim)) xlim <- c(st_bbox(listFonds[[1]])$xmin,st_bbox(listFonds[[1]])$xmax+x_marge*3)
    if(is.null(ylim)) ylim <- c(st_bbox(listFonds[[1]])$ymin,st_bbox(listFonds[[1]])$ymax+y_marge*3)

    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- xlim[2]-(xlim[2]-xlim[1])/10
      yLeg <- ylim[2]-(ylim[2]-ylim[1])/10
    }
    x_large <- (xlim[2]-xlim[1])/20
    y_large <- x_large/1.5

    xmin <- min(st_coordinates(fondPointsLeg)[,1]) - x_large
    xmax <- max(st_coordinates(fondPointsLeg)[,1]) + (x_large*5)
    ymin <- min(st_coordinates(fondPointsLeg)[,2]) - (y_large*2)
    ymax <- max(st_coordinates(fondPointsLeg)[,2]) + (y_large*3)
    bbox_leg_typo_symboles <- matrix(c(xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin, xmin,ymax),ncol=2, byrow=TRUE)
    bbox_leg_typo_symboles <- st_sf(geometry=st_sfc(st_polygon(list(bbox_leg_typo_symboles))),crs=st_crs(fondPoints))

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
        if(is.null(xlim) | is.null(ylim))
        {
          plot(st_geometry(listFonds[[1]]),col=colFond,border=colBorder,lwd=epaisseur)
        }else
        {
          plot(st_geometry(listFonds[[1]]),xlim=xlim,ylim=ylim,col=colFond,border=colBorder,lwd=epaisseur)
        }
      }else
      {
        plot(st_geometry(listFonds[[i]]),col=colFond,border=colBorder,lwd=epaisseur,add=T)
      }
    }

    for(i in 1:nrow(fondPoints))
    {
      plot(st_geometry(fondPoints[i,]),pch=types[i],col=couleurs[i],border=colBorder[i],cex=tailles[i],lwd=epaisseurs[i],add=T)
    }

    if(!is.null(etiquettes))
    {
      for(i in 1:nrow(tableEtiquettes))
      {
        text(tableEtiquettes[i,"X"],tableEtiquettes[i,"Y"],labels=tableEtiquettes[i,"LIBELLE"],cex=tableEtiquettes[i,"TAILLE"],col=tableEtiquettes[i,"COL"],font=tableEtiquettes[i,"FONT"])
      }
    }

    suppressWarnings(plot(bbox_leg_typo_symboles,add=T,col="white",border="white",lwd=1))

    for(i in 1:nrow(symbLeg))
    {
      plot(st_geometry(fondPointsLeg[i,]),pch=types[i],col=couleurs[i],border=colBorder[i],cex=tailles[i],lwd=epaisseurs[i],add=T)
      text(pointsLeg[i,1]+decalageLeg,pointsLeg[i,2],labels=labels[i],cex=0.9,adj=0)
    }

    text(pointsLeg[1,1],pointsLeg[1,2]+decalageLeg,labels=titreLeg,cex=1,adj=0)

    if(titreCarte!="")
    {
      text(((st_bbox(listFonds[[ixMax]])$xmin)+(st_bbox(listFonds[[ixMax]])$xmax-st_bbox(listFonds[[ixMax]])$xmin)/2),st_bbox(listFonds[[iyMax]])$ymax+y_marge*3,labels=titreCarte)
    }

    if(sourceCarte!="")
    {
      text(((st_bbox(listFonds[[ixMax]])$xmax+x_marge*3)-st_bbox(listFonds[[ixMax]])$xmin)/6,st_bbox(listFonds[[iyMax]])$ymin,labels=sourceCarte,cex=0.7)
    }

    message(simpleMessage(paste0("[INFO] Les coordonnees de la legende sont x = ", xLeg[1]," metres ; y = ", yLeg[1]," metres")))

    return("")
  }
