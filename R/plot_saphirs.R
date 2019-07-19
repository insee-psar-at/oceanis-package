plot_saphirs <-
function(data,fondMaille,fondSousAnalyse=NULL,fondSurAnalyse=NULL,typeMaille,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax=NULL,direction="Ent",filtreVol=0,dom="0",precisionLegFleches=0,titreLeg="",xLeg=NULL,yLeg=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,colEntree="#CD853F",colSortie="#6495ED",colBorder="white",colBorderMaille="black",xlim=NULL,ylim=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24<-msg_error25<-msg_error26<-msg_error27<-msg_error28<-msg_error29<-msg_error30<-msg_error31<-msg_error32 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSousAnalyse)) if(any(!any(class(fondSousAnalyse[[1]]) %in% "sf"),!any(class(fondSousAnalyse[[1]]) %in% "data.frame"))) msg_error3 <- "Les fonds a positionner en-dessous de l'analyse doivent etre une liste d'objets sf / "
    if(!is.null(fondSurAnalyse)) if(any(!any(class(fondSurAnalyse[[1]]) %in% "sf"),!any(class(fondSurAnalyse[[1]]) %in% "data.frame"))) msg_error4 <- "Les fonds a positionner au-dessus de l'analyse doivent etre une liste d'objets sf / "
    if(any(class(typeMaille)!="character")) msg_error5 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM') / "
    if(any(class(idDataDepart)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error8 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(largeurFlecheMax)) if(any(class(largeurFlecheMax)!="numeric")) msg_error9 <- "La largeur de la fleche max doit etre de type numerique (en km) / "
    if(any(class(direction)!="character")) msg_error10 <- "La direction des fleches doit etre de type caractere / "
    if(any(class(filtreVol)!="numeric")) msg_error11 <- "Le filtre doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error12 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(any(class(precisionLegFleches)!="numeric")) msg_error13 <- "La variable precisionLegFleches doit etre de type numerique / "
    if(any(class(titreLeg)!="character")) msg_error14 <- "Le titre de la legende doit etre de type caractere / "
    if(!is.null(xLeg)) if(any(class(xLeg)!="numeric")) msg_error15 <- "La variable xLeg doit etre de type numerique / "
    if(!is.null(yLeg)) if(any(class(yLeg)!="numeric")) msg_error16 <- "La variable yLeg doit etre de type numerique / "
    if(any(class(titreCarte)!="character")) msg_error17 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error18 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error19 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(any(class(colEntree)!="character")) msg_error20 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colSortie)!="character")) msg_error21 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error22 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorderMaille)!="character")) msg_error23 <- "La couleur de la bordure de maille doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(xlim)) if(any(class(xlim)!="numeric")) msg_error24 <- "La variable xlim doit etre de type numerique / "
    if(!is.null(ylim)) if(any(class(ylim)!="numeric")) msg_error25 <- "La variable yim doit etre de type numerique / "
    
    if(length(names(data))<3) msg_error26 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable de depart, une variable d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error27 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error28 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM' / "
    if(!any(names(data) %in% idDataDepart))  msg_error29 <- "La variable de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error30 <- "La variable d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error31 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error32 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
           !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24),
           !is.null(msg_error25),!is.null(msg_error26),!is.null(msg_error27),!is.null(msg_error28),
           !is.null(msg_error29),!is.null(msg_error30),!is.null(msg_error31),!is.null(msg_error32)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                              msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,
                              msg_error23,msg_error24,msg_error25,msg_error26,msg_error27,msg_error28,msg_error29,msg_error30,msg_error31,msg_error32)))
    }
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
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
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Analyse
    if(is.null(largeurFlecheMax))
    {
      if (typeMaille=="REG") largeurFlecheMax<-100
      if (typeMaille=="DEP") largeurFlecheMax<-30
      if (typeMaille=="ZE") largeurFlecheMax<-10
      if (typeMaille=="AU") largeurFlecheMax<-6
      if (typeMaille=="BV") largeurFlecheMax<-6
      if (typeMaille=="UU") largeurFlecheMax<-6
      if (typeMaille=="EPCI") largeurFlecheMax<-4
      if (typeMaille=="DEPCOM") largeurFlecheMax<-2
    }
    
    if(typeMaille=="REG") longueur<-100000
    if(typeMaille=="DEP") longueur<-30000
    if(!typeMaille %in% c("REG","DEP")) longueur<-10000
    
    data <- data[data$CODE1!=data$CODE2,]
    
    analyse<-k_saphir(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeurFlecheMax*1000,longueur,direction)
    analyse<-analyse[[1]]
    
    if(direction!="Sol")
    {
      fond_saphirs <- analyse[as.data.frame(analyse)[,varFlux]>=100,]
    }else
    {
      fond_saphirs <- analyse
    }
    
    if(direction=="Sol")
    {
      fond_saphirs_ent <- fond_saphirs[as.data.frame(fond_saphirs)[,varFlux]>=0,]
      fond_saphirs_sor <- fond_saphirs[as.data.frame(fond_saphirs)[,varFlux]<0,]
    }
    
    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- st_bbox(fondMaille)$xmax-(st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/10
      yLeg <- st_bbox(fondMaille)$ymax-(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/3
    }
    
    points_L93 <- data.frame(xLeg=xLeg,yLeg=yLeg,stringsAsFactors = FALSE)
    
    list_points <- apply(points_L93,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=paste0("+init=epsg:",code_epsg))))
    fond_points_L93 <- do.call("rbind",list_points)
    fond_points_WGS84 <- st_transform(fond_points_L93,paste0("+init=epsg:4326 +proj=longlat +ellps=WGS84"))
    
    fond_points_WGS84 <- data.frame(st_coordinates(fond_points_WGS84))
    names(fond_points_WGS84) <- c("lng","lat")
    
    vmax <- max(abs(as.data.frame(fond_saphirs)[,varFlux]))
    
    coord_fleche_max_pl <- st_coordinates(analyse[abs(data.frame(analyse)[,varFlux])==vmax,])
    large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[5,1],coord_fleche_max_pl[5,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
    
    long_pl <- large_pl
    
    flux_leg <- flux_legende_saphirs_pl(fond_points_WGS84$lng,fond_points_WGS84$lat,long_pl,large_pl,st_crs(fondMaille)[1]$epsg)
    flux_leg <- cbind(flux_leg,VALEUR=c(vmax,vmax/3))
    
    x_marge <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_marge <- (st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20
    
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
    
    plot(st_geometry(fondMaille),col="transparent",border=colBorderMaille,add=T)
    
    if(direction!="Sol")
    {
      plot(st_geometry(fond_saphirs),border=colBorder,col=colEntree,add=T)
    }else
    {
      if(nrow(fond_saphirs_ent)>0) plot(st_geometry(fond_saphirs_ent),border=colBorder,col=colEntree,add=T)
      if(nrow(fond_saphirs_sor)>0) plot(st_geometry(fond_saphirs_sor),border=colBorder,col=colSortie,add=T)
    }
    
    plot(st_geometry(flux_leg),col=colEntree,border=colBorder,add=T)
    text(max(st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==1,1])+1000,st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==1,2][3],labels=round(vmax,precisionLegFleches),cex=0.9,adj=0)
    text(max(st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==2,1])+1000,st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==2,2][3],labels=round(vmax/3,precisionLegFleches),cex=0.9,adj=0)
    
    text(min(st_coordinates(flux_leg[1,])[,1]),max(st_coordinates(flux_leg[1,])[,2])+(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20,labels=titreLeg,cex=1,adj=0)
    
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
    
    message(simpleMessage(paste0("[INFO] La largeur maximale des fleches = ",largeurFlecheMax)))
    message(simpleMessage(paste0("[INFO] Les coordonnees de la legende sont x = ",round(xLeg,2)," metres ; y = ",round(yLeg,2)," metres")))
    
    return(fond_saphirs)
  }
