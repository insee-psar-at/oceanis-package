plot_joignantes <-
function(data,fondMaille,fondSousAnalyse=NULL,fondSurAnalyse=NULL,typeMaille,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax=NULL,filtreVol=0,filtreDist=100,filtreMajeurs=10,decalageAllerRetour=0,decalageCentroid=0,emprise="FRM",precisionLegFleches=0,titreLeg="",xLeg=NULL,yLeg=NULL,cadreLeg=FALSE,xLimCadreLeg=NULL,yLimCadreLeg=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,colFleche="#286AC7",colBorder="white",colBorderMaille="black",xlim=NULL,ylim=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres

    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24<-msg_error25<-msg_error26<-msg_error27<-msg_error28<-msg_error29<-msg_error30<-msg_error31<-msg_error32<-msg_error33<-msg_error34<-msg_error35<-msg_error36<-msg_error37 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSousAnalyse)) if(any(!any(class(fondSousAnalyse[[1]]) %in% "sf"),!any(class(fondSousAnalyse[[1]]) %in% "data.frame"))) msg_error3 <- "Les fonds a positionner en-dessous de l'analyse doivent etre une liste d'objets sf / "
    if(!is.null(fondSurAnalyse)) if(any(!any(class(fondSurAnalyse[[1]]) %in% "sf"),!any(class(fondSurAnalyse[[1]]) %in% "data.frame"))) msg_error4 <- "Les fonds a positionner au-dessus de l'analyse doivent etre une liste d'objets sf / "
    if(any(class(typeMaille)!="character")) msg_error5 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM') / "
    if(any(class(idDataDepart)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error8 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(largeurFlecheMax)) if(any(class(largeurFlecheMax)!="numeric")) msg_error9 <- "La largeur de la fleche max doit etre de type numerique (en km) / "
    if(any(class(filtreVol)!="numeric")) msg_error10 <- "Le filtre doit etre de type numerique / "
    if(any(class(filtreDist)!="numeric")) msg_error11 <- "Le filtre doit etre de type numerique / "
    if(any(class(filtreMajeurs)!="numeric")) msg_error12 <- "Le filtre doit etre de type numerique / "
    if(any(class(decalageAllerRetour)!="numeric")) msg_error13 <- "La variable decalageAllerRetour doit etre de type numerique / "
    if(any(class(decalageCentroid)!="numeric")) msg_error14 <- "La variable decalageCentroid doit etre de type numerique / "
    if(any(class(emprise)!="character")) msg_error15 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974' ou '976') / "
    if(any(class(precisionLegFleches)!="numeric")) msg_error16 <- "La variable precisionLegFleches doit etre de type numerique / "
    if(any(class(titreLeg)!="character")) msg_error17<- "Le titre de la legende doit etre de type caractere / "
    if(!is.null(xLeg)) if(any(class(xLeg)!="numeric")) msg_error18 <- "La variable xLeg doit etre de type numerique / "
    if(!is.null(yLeg)) if(any(class(yLeg)!="numeric")) msg_error19 <- "La variable yLeg doit etre de type numerique / "
    if(any(class(cadreLeg)!="logical")) msg_error20 <- "La variable cadreLeg doit etre logique TRUE ou FALSE / "
    if(!is.null(xLimCadreLeg)) if(any(class(xLimCadreLeg)!="numeric")) msg_error21 <- "La variable xLimCadreLeg doit etre de type numerique / "
    if(!is.null(yLimCadreLeg)) if(any(class(yLimCadreLeg)!="numeric")) msg_error22 <- "La variable yLimCadreLeg doit etre de type numerique / "
    if(any(class(titreCarte)!="character")) msg_error23 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error24 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error25 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(!is.null(colFleche)) if(any(class(colFleche)!="character")) msg_error26 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error27 <- "La couleur de la bordure de fleche doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorderMaille)!="character")) msg_error28 <- "La couleur de la bordure de maille doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(xlim)) if(any(class(xlim)!="numeric")) msg_error29 <- "La variable xlim doit etre de type numerique / "
    if(!is.null(ylim)) if(any(class(ylim)!="numeric")) msg_error30 <- "La variable yim doit etre de type numerique / "

    if(length(names(data))<3) msg_error31 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable de depart, une variable d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error32 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error33 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'DEPCOM' / "
    if(!any(names(data) %in% idDataDepart))  msg_error34 <- "La variable de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error35 <- "La variable d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error36 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!emprise %in% c("FRM","971","972","973","974","976")) msg_error37 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974' ou '976' / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
           !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24),
           !is.null(msg_error25),!is.null(msg_error26),!is.null(msg_error27),!is.null(msg_error28),
           !is.null(msg_error29),!is.null(msg_error30),!is.null(msg_error31),!is.null(msg_error32),
           !is.null(msg_error33),!is.null(msg_error34),!is.null(msg_error35),!is.null(msg_error36),!is.null(msg_error37)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                              msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,
                              msg_error23,msg_error24,msg_error25,msg_error26,msg_error27,msg_error28,msg_error29,msg_error30,
                              msg_error31,msg_error32,msg_error33,msg_error34,msg_error35,msg_error36,msg_error37)))
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
        if(any(Encoding(fondSousAnalyse[[i]]$LIBELLE) %in% "latin1")){
          fondSousAnalyse[[i]]$LIBELLE<-iconv(fondSousAnalyse[[i]]$LIBELLE,"latin1","UTF-8")
        }
      }
    }
    if(!is.null(fondSurAnalyse))
    {
      for(i in 1:length(fondSurAnalyse))
      {
        names(fondSurAnalyse[[i]])[1] <- "CODE"
        names(fondSurAnalyse[[i]])[2] <- "LIBELLE"
        if(any(Encoding(fondSurAnalyse[[i]]$LIBELLE) %in% "latin1")){
          fondSurAnalyse[[i]]$LIBELLE<-iconv(fondSurAnalyse[[i]]$LIBELLE,"latin1","UTF-8")
        }
      }
    }
    if(any(Encoding(fondMaille$LIBELLE) %in% "latin1")){
      fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","UTF-8")
    }
    if(titreLeg!="")
    {
      if(any(Encoding(titreLeg) %in% "latin1")){
        titreLeg<-iconv(titreLeg,"latin1","UTF-8")
      }
    }
    if(titreCarte!="")
    {
      if(any(Encoding(titreCarte) %in% "latin1")){
        titreCarte<-iconv(titreCarte,"latin1","UTF-8")
      }
    }
    if(sourceCarte!="")
    {
      if(any(Encoding(sourceCarte) %in% "latin1")){
        sourceCarte<-iconv(sourceCarte,"latin1","UTF-8")
      }
    }

    code_epsg <- switch(emprise, #emprise
                        "FRM"="2154",# Lambert 93
                        "971"="5490",# UTM 20 N
                        "972"="5490",# UTM 20 N
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

    data <- data[data$CODE1!=data$CODE2,]

    analyse<-k_joignantes(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeurFlecheMax*1000,decalageAllerRetour,decalageCentroid)
    analyse<-analyse[[1]]

    if(filtreMajeurs>nrow(analyse))
    {
      nb_flux_majeur <- nrow(analyse)
    }else
    {
      nb_flux_majeur <- filtreMajeurs
      if(nb_flux_majeur<1) nb_flux_majeur <- 1
    }

    analyse_list <- split(analyse,factor(analyse$CODE1))
    analyse_1 <- data.frame()
    aa <- lapply(1:length(analyse_list), function(x) analyse_1 <<- rbind(analyse_1,as.data.frame(analyse_list[[x]])[rev(order(as.data.frame(analyse_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
    analyse_1 <- analyse_1[!is.na(analyse_1[,varFlux]),]

    analyse_list <- split(analyse,factor(analyse$CODE2))
    analyse_2 <- data.frame()
    aa <- lapply(1:length(analyse_list), function(x) analyse_2 <<- rbind(analyse_2,as.data.frame(analyse_list[[x]])[rev(order(as.data.frame(analyse_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
    analyse_2 <- analyse_2[!is.na(analyse_2[,varFlux]),]

    fond_joignantes <- unique(rbind(analyse_1,analyse_2))

    fond_joignantes <- st_as_sf(fond_joignantes)

    fond_joignantes_WGS84 <- st_transform(fond_joignantes, crs = 4326)
    
    st_agr(fond_joignantes_WGS84) <- "constant"
    fond_joignantes <- fond_joignantes[as.vector(st_length(st_cast(fond_joignantes_WGS84,"LINESTRING"))/2.2)<=filtreDist*1000,]
    rm(fond_joignantes_WGS84)

    fond_joignantes <- fond_joignantes[as.data.frame(fond_joignantes)[,varFlux]>=filtreVol,]

    fond_joignantes <- fond_joignantes[rev(order(as.data.frame(fond_joignantes)[,varFlux])),]

    x_marge <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_marge <- (st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20

    if(is.null(xLeg) | is.null(yLeg))
    {
      xLeg <- st_bbox(fondMaille)$xmax-(st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/10
      yLeg <- st_bbox(fondMaille)$ymax-(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/10
    }

    points_L93 <- data.frame(xLeg=xLeg,yLeg=yLeg,stringsAsFactors = FALSE)

    list_points <- apply(points_L93,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=as.numeric(code_epsg))))
    fond_points_L93 <- do.call("rbind",list_points)
    fond_points_WGS84 <- st_transform(fond_points_L93, crs = 4326)

    fond_points_WGS84 <- data.frame(st_coordinates(fond_points_WGS84))
    names(fond_points_WGS84) <- c("lng","lat")

    donnees <- merge(as.data.frame(fond_joignantes)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)

    donnees <- donnees[rev(order(donnees[,varFlux])),]

    if(nrow(donnees) > 0){
      vmax <- max(donnees[,varFlux])
      coord_fleche_max_pl <- st_coordinates(fond_joignantes[abs(as.data.frame(fond_joignantes)[,varFlux])==vmax,])
      large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[2,1],coord_fleche_max_pl[2,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
    }else{
      vmax <- 0
      large_pl <- 0
    }
    
    long_pl <- large_pl*2

    flux_leg <- fleche_legende(fond_points_WGS84$lng,fond_points_WGS84$lat,long_pl,large_pl,vmax,code_epsg)[[5]]
    flux_leg <- cbind(flux_leg,ETI_VAL=c(vmax,vmax/3))

    if(!is.null(etiquettes))
    {
      tableEtiquettes <- table_etiquettes(fondMaille,etiquettes)
    }

    if(is.null(xlim)) xlim <- c(st_bbox(fondMaille)$xmin,st_bbox(fondMaille)$xmax+x_marge*3)
    if(is.null(ylim)) ylim <- c(st_bbox(fondMaille)$ymin,st_bbox(fondMaille)$ymax+y_marge*3)

    x_large <- (xlim[2]-xlim[1])/20
    y_large <- x_large/1.5

    xmin <- min(st_coordinates(flux_leg)[,1]) - x_large
    xmax <- max(st_coordinates(flux_leg)[,1]) + (x_large*3)
    ymin <- min(st_coordinates(flux_leg)[,2]) - y_large
    ymax <- max(st_coordinates(flux_leg)[,2]) + (y_large*3)

    if(cadreLeg)
    {
      if(is.null(xLimCadreLeg) | is.null(xLimCadreLeg))
      {
        bbox_leg_joignantes <- matrix(c(xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin, xmin,ymax),ncol=2, byrow=TRUE)
        bbox_leg_joignantes <- st_sf(geometry=st_sfc(st_polygon(list(bbox_leg_joignantes))),crs=st_crs(fondMaille))
      }else
      {
        bbox_leg_joignantes <- matrix(c(xLimCadreLeg[1],yLimCadreLeg[2], xLimCadreLeg[2],yLimCadreLeg[2], xLimCadreLeg[2],yLimCadreLeg[1], xLimCadreLeg[1],yLimCadreLeg[1], xLimCadreLeg[1],yLimCadreLeg[2]),ncol=2, byrow=TRUE)
        bbox_leg_joignantes <- st_sf(geometry=st_sfc(st_polygon(list(bbox_leg_joignantes))),crs=st_crs(fondMaille))
      }
    }

    par(mai=c(0,0,0,0))
    plot(st_geometry(fondMaille),xlim=xlim,ylim=ylim,border=colBorderMaille)

    if(!is.null(fondSousAnalyse))
    {
      for(i in 1:length(fondSousAnalyse))
      {
        names_fond <- names(as.data.frame(fondSousAnalyse[[i]]))
        if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(fondSousAnalyse[[i]])[,"COL"]) else colFond <- "transparent"
        if(any(names_fond %in% "BORDER")) colBorder2 <- unique(as.data.frame(fondSousAnalyse[[i]])[,"BORDER"]) else colBorder2 <- "black"
        if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(fondSousAnalyse[[i]])[,"EPAISSEUR"]) else epaisseur <- 1
        plot(st_geometry(fondSousAnalyse[[i]]),col=colFond,border=colBorder2,lwd=epaisseur,add=T)
      }
    }

    plot(st_geometry(fondMaille),col="transparent",border=colBorderMaille,add=T)

    plot(st_geometry(fond_joignantes),border=colBorder,col=colFleche,add=T)

    if(!is.null(fondSurAnalyse))
    {
      for(i in 1:length(fondSurAnalyse))
      {
        names_fond <- names(as.data.frame(fondSurAnalyse[[i]]))
        if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(fondSurAnalyse[[i]])[,"COL"]) else colFond <- "transparent"
        if(any(names_fond %in% "BORDER")) colBorder2 <- unique(as.data.frame(fondSurAnalyse[[i]])[,"BORDER"]) else colBorder2 <- "black"
        if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(fondSurAnalyse[[i]])[,"EPAISSEUR"]) else epaisseur <- 1
        plot(st_geometry(fondSurAnalyse[[i]]),col=colFond,border=colBorder2,lwd=epaisseur,add=T)
      }
    }

    if(!is.null(etiquettes))
    {
      for(i in 1:nrow(tableEtiquettes))
      {
        text(tableEtiquettes[i,"X"],tableEtiquettes[i,"Y"],labels=tableEtiquettes[i,"LIBELLE"],cex=tableEtiquettes[i,"TAILLE"],col=tableEtiquettes[i,"COL"],font=tableEtiquettes[i,"FONT"])
      }
    }

    if(cadreLeg) suppressWarnings(plot(bbox_leg_joignantes,add=T,col="white",border="white",lwd=1))

    plot(st_geometry(flux_leg),col=colFleche,border=colBorder,add=T)
    text(max(st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==1,1])+1000,st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==1,2][3],labels=round(vmax,precisionLegFleches),cex=0.9,adj=0)
    text(max(st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==2,1])+1000,st_coordinates(flux_leg)[st_coordinates(flux_leg)[,4]==2,2][3],labels=round(vmax/3,precisionLegFleches),cex=0.9,adj=0)

    text(min(st_coordinates(flux_leg[1,])[,1]),max(st_coordinates(flux_leg[1,])[,2])+(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20,labels=titreLeg,cex=1,adj=0)

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

    return(fond_joignantes)
  }
