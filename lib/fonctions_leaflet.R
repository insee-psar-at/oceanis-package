leaflet_ronds <-
  function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varVolume,rayonRond=NULL,rapportRond=NULL,dom="0",fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(rayonRond)) if(any(class(rayonRond)!="numeric")) msg_error7 <- "La variable doit etre de type numerique / "
    if(!is.null(rapportRond)) if(any(class(rapportRond)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error9 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error10 <- "Le fond des chx doit etre un objet sf / "
    
    if(length(names(data))<2) msg_error11 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error12 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error13 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error15 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error16 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error17 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),!is.null(msg_error17)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17)))
    }
    
    if(!is.null(fondChx))
    {
      centroid <- "chx"
    }else
    {
      centroid <- "centroid"
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondChx)) 
    {
      names(fondChx)[1] <- "CODE"
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    if(is.null(fondMailleElargi))
    {
      elargi <- FALSE
    }else
    {
      elargi <- TRUE
    }
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Calcul du rayon du rond max
    
    #Aire totale du territoire d'etude
    aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
    #valeur max de la serie de donnees
    suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
    
    serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
    serie <- serie[!is.na(serie)]
    #on ramene la serie a un quotient fonction de la valeur max
    quotient <- serie/max_var
    #somme des carres
    somme_quotient <- sum(quotient^2)
    #calcul de la surface max du rond le plus grand
    max_surface_rond <- (aire_territoire/(7*somme_quotient))
    #calcul du rayon max du rond le plus grand
    max_rayon_metres <- sqrt(max_surface_rond/pi)
    
    if(!is.null(rayonRond))
    {
      if(rayonRond>max_rayon_metres)
      {
        stop(simpleError(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon max est ",round(max_rayon_metres,2)," metres.")))
      }
    }
    
    if(is.null(rayonRond) & is.null(rapportRond))
    {
      rayonRond <- max_rayon_metres/1.25
    }
    
    if(is.null(rayonRond) & !is.null(rapportRond)) #Calcul du rayon a partir du rapport
    {
      rayonRond <- round(sqrt((rapportRond*max_var)/pi),0)
    }
    
    # Analyse
    
    analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi,centroid,fondChx)
    
    if(is.null(analyse))
    {
      stop(simpleError("La maille ne correspond pas au niveau geographique du fichier de donnees. Veuillez svp choisir une maille adaptee ou modifier le fichier de donnees."))
    }
    
    analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    if(elargi)
    {
      analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    }
    
    analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    if(elargi)
    {
      analyse_WGS84_elargi <- st_transform(analyse$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    if(!is.null(fondMailleElargi))
    {
      fondMailleElargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    # CONSTRUCTION DE LA MAP EN LEAFLET
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_ronds",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_ronds",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = F,
                         group = list(nom_couche="carte_ronds",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    if(!is.null(fondMailleElargi))
    {
      map <- addPolygons(map = map, data = maille_WGS84_elargi,
                         stroke = TRUE, color = "grey", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fondMailleElargi)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_ronds",code_epsg=code_epsg,nom_fond="fond_maille_elargi")
      )
    }
    
    # AFFICHAGE DE LA MAILLE
    
    map <- addPolygons(map = map, data = maille_WGS84, opacity = 1, #maille_WGS84
                       stroke = TRUE, color = "grey", weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                       fill = T, fillColor = "white", fillOpacity = 0.001,
                       group = list(nom_couche="carte_ronds",code_epsg=code_epsg,nom_fond="fond_maille")
    )
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE L'ANALYSE ELARGIE
      
      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_elargi)[,1],
                        lat = st_coordinates(analyse_WGS84_elargi)[,2],
                        stroke = TRUE, color = "white",
                        opacity = 0.6,
                        weight = 1,
                        radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                        options = pathOptions(clickable = T),
                        popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1),
                        fill = T,
                        fillColor = sapply(analyse$donnees_elargi$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                        fillOpacity = 0.6,
                        group = list(nom_couche="carte_ronds",code_epsg=code_epsg,dom=dom,nom_fond=c(if(max(analyse$donnees_elargi$save)>0){"fond_ronds_pos_elargi_carte"}else{" "},
                                                                                                     if(min(analyse$donnees_elargi$save)<0){"fond_ronds_neg_elargi_carte"}else{" "}),
                                     max_var=max_var,var_volume=varVolume)
      )
    }
    
    # AFFICHAGE DE L'ANALYSE
    
    map <- addCircles(map = map,
                      lng = st_coordinates(analyse_WGS84)[,1],
                      lat = st_coordinates(analyse_WGS84)[,2],
                      stroke = TRUE, color = "white",
                      opacity = 1,
                      weight = 1,
                      radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                      options = pathOptions(clickable = T),
                      popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1),
                      fill = T,
                      fillColor = sapply(analyse$donnees$save, function(x) if(x>0){"#CD853F"}else{"#6495ED"}),
                      fillOpacity = 1,
                      group = list(nom_couche="carte_ronds",code_epsg=code_epsg,dom=dom,nom_fond=c(if(max(analyse$donnees$save)>0){"fond_ronds_pos_carte"}else{" "},
                                                                                                   if(min(analyse$donnees$save)<0){"fond_ronds_neg_carte"}else{" "}),
                                   max_var=max_var,var_volume=varVolume)
    )
    
    return(map)
  }


leaflet_classes <-
  function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varRatio,methode="kmeans",nbClasses=3,bornes=NULL,precision=1,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(methode)!="character")) msg_error7 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error9 <- "La variable doit etre un vecteur numerique / "
    if(any(class(dom)!="character")) msg_error10 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(any(class(precision)!="numeric")) msg_error11 <- "La variable doit etre de type numerique / "
    
    if(length(names(data))<2) msg_error12 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error13 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error14 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error15 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error16 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error17 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error18 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error19 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18,msg_error19)))
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    if(is.null(fondMailleElargi))
    {
      elargi <- FALSE
    }else
    {
      elargi <- TRUE
    }
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Analyse
    data[,varRatio] <- round(data[,varRatio],precision)
    
    analyse <- k_classes(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varRatio,elargi)
    
    if(elargi)
    {
      analyse <- list(donnees=analyse[[1]],fond_maille=fondMaille,donnees_elargi=analyse[[2]],fond_maille_elargi=fondMailleElargi)
    }else
    {
      analyse <- list(donnees=analyse[[1]],fond_maille=fondMaille)
    }
    
    analyse$donnees[,varRatio] <- round(analyse$donnees[,varRatio],precision)
    
    analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(round(analyse$donnees[,varRatio],3), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    if(elargi)
    {
      analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(round(analyse$donnees_elargi[,varRatio],3), big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    }
    
    max <- max(analyse$donnees[,varRatio])
    min <- min(analyse$donnees[,varRatio])
    if(elargi)
    {
      max <- max(analyse$donnees_elargi[,varRatio])
      min <- min(analyse$donnees_elargi[,varRatio])
    }
    
    if(is.null(bornes))
    {
      suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse$donnees[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"))
      
      carac_bornes <- calcul_bornes(analyse$donnees,bornes_analyse,varRatio,nbClasses,methode)
      
      bornes <- carac_bornes[[1]]
      bornes[1] <- max(as.numeric(analyse$donnees[,varRatio]))
      bornes_sansext <- bornes[-1]
      bornes_sansext <- bornes_sansext[-length(bornes_sansext)]
      bornes_sansext <- sort(bornes_sansext, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precision)
      
      pal_classes <- carac_bornes[[2]]
    }else
    {
      bornes_sansext <- sort(bornes, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precision)
      
      pal_classes_pos <- c("#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93") # Rouge du +fonce au + clair
      pal_classes_neg <- c("#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050") # Bleu du + clair au + fonce
      
      if(min<0 & max>=0) # Si - et +
      {
        pal_classes_pos <- pal_classes_pos[(7-length(bornes[bornes>0])+1):7]
        pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }
      if(min>=0) # Si +
      {
        if(length(bornes)>=8)
        {
          pal_classes <- pal_classes_pos
        }else
        {
          pal_classes <- pal_classes_pos[-c(1:(7-length(bornes)+1))] # On enleve les couleurs fonces inutiles
        }
      }
      if(max<0) # Si -
      {
        if(length(bornes)>=8)
        {
          pal_classes <- pal_classes_neg
        }else
        {
          pal_classes <- pal_classes_neg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
        }
      }
    }
    
    pal_classes[is.na(pal_classes)] <- "grey"
    palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    if(elargi)
    {
      maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    # Construction de la map par defaut
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_classes",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_classes",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_classes",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE ELARGIE
      
      analyse_maille_classe <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varRatio])),varRatio]
      
      analyse_maille <- merge(maille_WGS84_elargi[,c("CODE","geometry")],analyse$donnees_elargi,by="CODE")
      analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),c("CODE","LIBELLE",varRatio,"TXT1","geometry")]
      analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
      
      map <- addPolygons(map = map, data = analyse_maille, opacity = 0.6,
                         stroke = TRUE, color = "white", weight = 1,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                         fill = T,
                         fillColor = palette(analyse_maille_classe),
                         fillOpacity = 0.6,
                         group = list(nom_couche="carte_classes",code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_elargi_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style="defaut")
      )
    }
    
    # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE
    
    analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varRatio])),varRatio]
    
    analyse_maille <- merge(maille_WGS84[,c("CODE","geometry")],analyse$donnees,by="CODE")
    analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),c("CODE","LIBELLE",varRatio,"TXT1","geometry")]
    analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
    
    map <- addPolygons(map = map, data = analyse_maille, opacity = 1,
                       stroke = TRUE, color = "white", weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                       fill = T,
                       fillColor = palette(analyse_maille_classe),
                       fillOpacity = 1,
                       group = list(nom_couche="carte_classes",code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style="defaut")
    )
    
    return(map)
  }


leaflet_ronds_classes <-
  function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varVolume,varRatio,rayonRond=NULL,rapportRond=NULL,methode="kmeans",nbClasses=3,bornes=NULL,precision=1,dom="0",fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(rayonRond)) if(any(class(rayonRond)!="numeric")) msg_error7 <- "La variable doit etre de type numerique / "
    if(!is.null(rapportRond)) if(any(class(rapportRond)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
    if(any(class(varRatio)!="character")) msg_error9 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(methode)!="character")) msg_error10 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error11 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error12 <- "La variable doit etre un vecteur numerique / "
    if(any(class(dom)!="character")) msg_error13 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(any(class(precision)!="numeric")) msg_error14 <- "La variable doit etre de type numerique / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error15 <- "Le fond des chx doit etre un objet sf / "
    
    if(length(names(data))<2) msg_error16 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error17 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error18 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error19 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error20 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error21 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error22 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error23 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error24 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
           !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,msg_error23,msg_error24)))
    }
    
    if(!is.null(fondChx))
    {
      centroid <- "chx"
    }else
    {
      centroid <- "centroid"
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondChx)) 
    {
      names(fondChx)[1] <- "CODE"
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    if(is.null(fondMailleElargi))
    {
      elargi <- FALSE
    }else
    {
      elargi <- TRUE
    }
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Calcul du rayon du rond max
    
    #Aire totale du territoire d'etude
    aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
    #valeur max de la serie de donnees
    suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
    
    serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
    serie <- serie[!is.na(serie)]
    #on ramene la serie a un quotient fonction de la valeur max
    quotient <- serie/max_var
    #somme des carres
    somme_quotient <- sum(quotient^2)
    #calcul de la surface max du rond le plus grand
    max_surface_rond <- (aire_territoire/(7*somme_quotient))
    #calcul du rayon max du rond le plus grand
    max_rayon_metres <- sqrt(max_surface_rond/pi)
    
    if(!is.null(rayonRond))
    {
      if(rayonRond>max_rayon_metres)
      {
        stop(simpleError(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon max est ",round(max_rayon_metres,2)," metres.")))
      }
    }
    
    if(is.null(rayonRond) & is.null(rapportRond))
    {
      rayonRond <- max_rayon_metres/1.25
    }
    
    if(is.null(rayonRond) & !is.null(rapportRond)) #Calcul du rayon a partir du rapport
    {
      rayonRond <- round(sqrt((rapportRond*max_var)/pi),0)
    }
    
    # Analyse ronds
    
    analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi,centroid,fondChx)
    
    if(is.null(analyse))
    {
      stop(simpleError("La maille ne correspond pas au niveau geographique du fichier de donnees. Veuillez svp choisir une maille adaptee ou modifier le fichier de donnees."))
    }
    
    analyse$donnees[,varRatio] <- round(analyse$donnees[,varRatio],precision)
    
    analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    analyse$donnees[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    if(elargi)
    {
      analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
      analyse$donnees_elargi[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    }
    
    analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    max <- max(analyse$donnees[,varRatio])
    min <- min(analyse$donnees[,varRatio])
    if(elargi)
    {
      max <- max(analyse$donnees_elargi[,varRatio])
      min <- min(analyse$donnees_elargi[,varRatio])
    }
    
    if(is.null(bornes))
    {
      suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse$donnees[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"))
      
      carac_bornes <- calcul_bornes(analyse$donnees,bornes_analyse,varRatio,nbClasses,methode)
      
      bornes <- carac_bornes[[1]]
      bornes[1] <- max(as.numeric(analyse$donnees[,varRatio]))
      bornes_sansext <- bornes[-1]
      bornes_sansext <- bornes_sansext[-length(bornes_sansext)]
      bornes_sansext <- sort(bornes_sansext, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precision)
      
      pal_classes <- carac_bornes[[2]]
    }else
    {
      bornes_sansext <- sort(bornes, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precision)
      
      pal_classes_pos <- c("#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93") # Rouge du +fonce au + clair
      pal_classes_neg <- c("#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050") # Bleu du + clair au + fonce
      
      if(min<0 & max>=0) # Si - et +
      {
        pal_classes_pos <- pal_classes_pos[(7-length(bornes[bornes>0])+1):7]
        pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }
      if(min>=0) # Si +
      {
        if(length(bornes)>=8)
        {
          pal_classes <- pal_classes_pos
        }else
        {
          pal_classes <- pal_classes_pos[-c(1:(7-length(bornes)+1))] # On enleve les couleurs fonces inutiles
        }
      }
      if(max<0) # Si -
      {
        if(length(bornes)>=8)
        {
          pal_classes <- pal_classes_neg
        }else
        {
          pal_classes <- pal_classes_neg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
        }
      }
    }
    
    pal_classes[is.na(pal_classes)] <- "grey"
    palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    if(elargi)
    {
      analyse_WGS84_elargi <- st_transform(analyse$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    # CONSTRUCTION DE LA MAP EN LEAFLET
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE LA MAILLE ELARGIE
      
      map <- addPolygons(map = map, data = maille_WGS84_elargi, opacity = 0.6, #maille_WGS84
                         stroke = TRUE, color = "grey", weight = 1,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84_elargi)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,nom_fond="fond_maille_elargi")
      )
    }
    
    # AFFICHAGE DE LA MAILLE
    
    map <- addPolygons(map = map, data = maille_WGS84, opacity = 1, #maille_WGS84
                       stroke = TRUE, color = "grey", weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                       fill = T, fillColor = "white", fillOpacity = 0.001,
                       group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,nom_fond="fond_maille")
    )
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE L'ANALYSE ELARGIE
      
      analyse_maille_classe <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varVolume])),varRatio]
      
      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_elargi)[,1],
                        lat = st_coordinates(analyse_WGS84_elargi)[,2],
                        stroke = TRUE, color = "white",
                        opacity = 0.6,
                        weight = 1,
                        radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                        options = pathOptions(clickable = T),
                        popup = paste0("<b> <font color=#2B3E50>",analyse$donnees_elargi$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees_elargi$TXT2),
                        fill = T,
                        fillColor = palette(analyse_maille_classe),
                        fillOpacity = 0.6,
                        group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,dom=dom,nom_fond="fond_ronds_elargi_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,precision=precision,style="defaut")
      )
    }
    
    # AFFICHAGE DE L'ANALYSE
    
    analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio]
    
    map <- addCircles(map = map,
                      lng = st_coordinates(analyse_WGS84)[,1],
                      lat = st_coordinates(analyse_WGS84)[,2],
                      stroke = TRUE, color = "white",
                      opacity = 1,
                      weight = 1,
                      radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                      options = pathOptions(clickable = T),
                      popup = paste0("<b> <font color=#2B3E50>",analyse$donnees$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees$TXT2),
                      fill = T,
                      fillColor = palette(analyse_maille_classe),
                      fillOpacity = 1,
                      group = list(nom_couche="carte_ronds_classes",code_epsg=code_epsg,dom=dom,nom_fond="fond_ronds_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,precision=precision,style="defaut")
    )
    
    return(map)
  }


leaflet_classes_ronds <-
  function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varVolume,varRatio,rayonRond=NULL,rapportRond=NULL,methode="kmeans",nbClasses=3,bornes=NULL,precision=1,dom="0",fondChx=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(rayonRond)) if(any(class(rayonRond)!="numeric")) msg_error7 <- "La variable doit etre de type numerique / "
    if(!is.null(rapportRond)) if(any(class(rapportRond)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
    if(any(class(varRatio)!="character")) msg_error9 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(methode)!="character")) msg_error10 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error11 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error12 <- "La variable doit etre un vecteur numerique / "
    if(any(class(dom)!="character")) msg_error13 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(any(class(precision)!="numeric")) msg_error14 <- "La variable doit etre de type numerique / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error15 <- "Le fond des chx doit etre un objet sf / "
    
    if(length(names(data))<2) msg_error16 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error17 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error18 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error19 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error20 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error21 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error22 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error23 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error24 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
           !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,msg_error23,msg_error24)))
    }
    
    if(!is.null(fondChx))
    {
      centroid <- "chx"
    }else
    {
      centroid <- "centroid"
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondMailleElargi)) 
    {
      names(fondMailleElargi)[1] <- "CODE"
      names(fondMailleElargi)[2] <- "LIBELLE"
      fondMailleElargi$LIBELLE<-iconv(fondMailleElargi$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    if(!is.null(fondChx)) 
    {
      names(fondChx)[1] <- "CODE"
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    if(is.null(fondMailleElargi))
    {
      elargi <- FALSE
    }else
    {
      elargi <- TRUE
    }
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Calcul du rayon du rond max
    
    #Aire totale du territoire d'etude
    aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
    #valeur max de la serie de donnees
    suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
    
    serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
    serie <- serie[!is.na(serie)]
    #on ramene la serie a un quotient fonction de la valeur max
    quotient <- serie/max_var
    #somme des carres
    somme_quotient <- sum(quotient^2)
    #calcul de la surface max du rond le plus grand
    max_surface_rond <- (aire_territoire/(7*somme_quotient))
    #calcul du rayon max du rond le plus grand
    max_rayon_metres <- sqrt(max_surface_rond/pi)
    
    if(!is.null(rayonRond))
    {
      if(rayonRond>max_rayon_metres)
      {
        stop(simpleError(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon max est ",round(max_rayon_metres,2)," metres.")))
      }
    }
    
    if(is.null(rayonRond) & is.null(rapportRond))
    {
      rayonRond <- max_rayon_metres/1.25
    }
    
    if(is.null(rayonRond) & !is.null(rapportRond)) #Calcul du rayon a partir du rapport
    {
      rayonRond <- round(sqrt((rapportRond*max_var)/pi),0)
    }
    
    # Analyse ronds
    
    analyse <- k_ronds(fondMaille,fondMailleElargi,names(fondMaille)[1],data,"CODE",varVolume,elargi,centroid,fondChx)
    
    if(is.null(analyse))
    {
      stop(simpleError("La maille ne correspond pas au niveau geographique du fichier de donnees. Veuillez svp choisir une maille adaptee ou modifier le fichier de donnees."))
    }
    
    analyse$donnees[,varRatio] <- round(analyse$donnees[,varRatio],precision)
    
    analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    analyse$donnees[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    if(elargi)
    {
      analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
      analyse$donnees_elargi[,"TXT2"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi[,varRatio], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    }
    
    analyse_WGS84 <- st_transform(analyse$analyse_points,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    max <- max(analyse$donnees[,varRatio])
    min <- min(analyse$donnees[,varRatio])
    if(elargi)
    {
      max <- max(analyse$donnees_elargi[,varRatio])
      min <- min(analyse$donnees_elargi[,varRatio])
    }
    
    if(is.null(bornes))
    {
      suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse$donnees[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"))
      
      carac_bornes <- calcul_bornes(analyse$donnees,bornes_analyse,varRatio,nbClasses,methode)
      
      bornes <- carac_bornes[[1]]
      bornes[1] <- max(as.numeric(analyse$donnees[,varRatio]))
      bornes_sansext <- bornes[-1]
      bornes_sansext <- bornes_sansext[-length(bornes_sansext)]
      bornes_sansext <- sort(bornes, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precision)
      
      pal_classes <- carac_bornes[[2]]
    }else
    {
      bornes_sansext <- sort(bornes, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precision)
      
      pal_classes_pos <- c("#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93") # Rouge du +fonce au + clair
      pal_classes_neg <- c("#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050") # Bleu du + clair au + fonce
      
      if(min<0 & max>=0) # Si - et +
      {
        pal_classes_pos <- pal_classes_pos[(7-length(bornes[bornes>0])+1):7]
        pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }
      if(min>=0) # Si +
      {
        if(length(bornes)>=8)
        {
          pal_classes <- pal_classes_pos
        }else
        {
          pal_classes <- pal_classes_pos[-c(1:(7-length(bornes)+1))] # On enleve les couleurs fonces inutiles
        }
      }
      if(max<0) # Si -
      {
        if(length(bornes)>=8)
        {
          pal_classes <- pal_classes_neg
        }else
        {
          pal_classes <- pal_classes_neg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
        }
      }
    }
    
    pal_classes[is.na(pal_classes)] <- "grey"
    palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    if(elargi)
    {
      analyse_WGS84_elargi <- st_transform(analyse$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    # Construction de la map par defaut
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE EN CLASSES ELARGIE
      
      analyse_maille_classe <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varRatio])),varRatio]
      
      analyse_maille_elargi <- merge(maille_WGS84_elargi[,c("CODE","geometry")],analyse$donnees_elargi,by="CODE")
      analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varRatio])),c("CODE","LIBELLE",varRatio,"TXT1","TXT2","geometry")]
      analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)
      
      map <- addPolygons(map = map, data = analyse_maille_elargi, opacity = 0.6,
                         stroke = TRUE, color = "white", weight = 1,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT2),
                         fill = T,
                         fillColor = palette(analyse_maille_classe),
                         fillOpacity = 0.6,
                         group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_elargi_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style="defaut")
      )
    }
    
    # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE EN CLASSES
    
    analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varRatio])),varRatio]
    
    analyse_maille <- merge(maille_WGS84[,c("CODE","geometry")],analyse$donnees,by="CODE")
    analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),c("CODE","LIBELLE",varRatio,"TXT1","TXT2","geometry")]
    analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)
    
    map <- addPolygons(map = map, data = analyse_maille, opacity = 1,
                       stroke = TRUE, color = "white", weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille)$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT2),
                       fill = T,
                       fillColor = palette(analyse_maille_classe),
                       fillOpacity = 1,
                       group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style="defaut")
    )
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE L'ANALYSE EN RONDS ELARGIE
      
      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_elargi)[,1],
                        lat = st_coordinates(analyse_WGS84_elargi)[,2],
                        stroke = TRUE, color = "#303030",
                        opacity = 0.6,
                        weight = 1.5,
                        radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                        options = pathOptions(clickable = F),
                        popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                        fill = F,
                        group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,dom=dom,nom_fond="fond_ronds_elargi_carte",max_var=max_var,var_volume=varVolume)
      )
    }
    
    # AFFICHAGE DE L'ANALYSE EN RONDS
    
    map <- addCircles(map = map,
                      lng = st_coordinates(analyse_WGS84)[,1],
                      lat = st_coordinates(analyse_WGS84)[,2],
                      stroke = TRUE, color = "#303030",
                      opacity = 1,
                      weight = 1.5,
                      radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                      options = pathOptions(clickable = F),
                      popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                      fill = F,
                      group = list(nom_couche="carte_classes_ronds",code_epsg=code_epsg,dom=dom,nom_fond="fond_ronds_carte",max_var=max_var,var_volume=varVolume)
    )
    
    return(map)
  }


leaflet_typo <-
  function(data,fondMaille,fondSuppl=NULL,idData,varTypo,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error3 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idData)!="character")) msg_error4 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varTypo)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(dom)!="character")) msg_error6 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<2) msg_error7 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error8 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error9 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error10 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varTypo))  msg_error11 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error12 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12)))
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Analyse
    
    analyse<-k_typo(fondMaille,names(fondMaille)[!sapply(fondMaille[-length(names(fondMaille))],is.numeric)][1],data,"CODE",varTypo)
    
    analyse <- analyse[[1]]
    analyse[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(as.data.frame(analyse)[,varTypo], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    analyse_WGS84 <- st_transform(analyse,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    nb_col <- length(unique(as.data.frame(analyse)[,"classe"]))
    pal_typo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
    pal_typo <- data.frame(cbind(pal_typo,unique(as.data.frame(analyse)[,"classe"])))
    names(pal_typo) <- c("col","classe")
    analyse <- merge(as.data.frame(analyse),pal_typo,by="classe")
    analyse <- analyse[order(as.data.frame(analyse)[,varTypo]),]
    
    # Construction de la map par defaut
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_typo",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_typo",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_typo",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE
    
    map <- addPolygons(map = map, data = analyse_WGS84, opacity = 1,
                       stroke = TRUE, color = "white", weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_WGS84)[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_WGS84$TXT1,"<br>"),
                       fill = T,
                       fillColor = analyse$col,
                       fillOpacity = 1,
                       group = list(nom_couche="carte_typo",code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_typo_carte",var_typo=varTypo)
    )
    
    return(map)
  }


leaflet_oursins <-
  function(data,fondMaille,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,filtreVol=0,filtreDist=100,filtreMajeurs=10,decalageAllerRetour=0,decalageCentroid=0,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error3 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(idDataDepart)!="character")) msg_error4 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(filtreVol)!="numeric")) msg_error7 <- "Le filtre doit etre de type numerique / "
    if(any(class(filtreDist)!="numeric")) msg_error8 <- "Le filtre doit etre de type numerique / "
    if(any(class(filtreMajeurs)!="numeric")) msg_error9 <- "Le filtre doit etre de type numerique / "
    if(any(class(decalageAllerRetour)!="numeric")) msg_error10 <- "La variable decalageAllerRetour doit etre de type numerique / "
    if(any(class(decalageCentroid)!="numeric")) msg_error11 <- "La variable decalageCentroid doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error12 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<3) msg_error13 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable de depart, une variable d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error14 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error15 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idDataDepart))  msg_error16 <- "La variable de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error17 <- "La variable d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error18 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error19 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,msg_error17,msg_error18,msg_error19)))
    }
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    # Analyse
    
    data <- data[data$CODE1!=data$CODE2,]
    
    analyse<-k_oursins(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,decalageAllerRetour,decalageCentroid)
    
    analyse_WGS84 <- st_transform(analyse[[1]],"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    if(filtreMajeurs>nrow(analyse_WGS84))
    {
      nb_flux_majeur <- nrow(analyse_WGS84)
    }else
    {
      nb_flux_majeur <- filtreMajeurs
      if(nb_flux_majeur<1) nb_flux_majeur <- 1
    }
    
    analyse_WGS84_list <- split(analyse_WGS84,factor(analyse_WGS84$CODE1))
    analyse_WGS84_1 <- data.frame()
    aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_1 <<- rbind(analyse_WGS84_1,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
    analyse_WGS84_1 <- analyse_WGS84_1[!is.na(analyse_WGS84_1[,varFlux]),]
    
    analyse_WGS84_list <- split(analyse_WGS84,factor(analyse_WGS84$CODE2))
    analyse_WGS84_2 <- data.frame()
    aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_2 <<- rbind(analyse_WGS84_2,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
    analyse_WGS84_2 <- analyse_WGS84_2[!is.na(analyse_WGS84_2[,varFlux]),]
    
    analyse_WGS84 <- unique(rbind(analyse_WGS84_1,analyse_WGS84_2))
    
    analyse_WGS84 <- st_as_sf(analyse_WGS84)
    
    analyse_WGS84 <- analyse_WGS84[as.vector(st_length(analyse_WGS84))<=filtreDist*1000,]
    
    analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=filtreVol,]
    
    analyse_WGS84 <- analyse_WGS84[rev(order(as.data.frame(analyse_WGS84)[,varFlux])),]
    
    donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
    
    donnees <- donnees[rev(order(donnees[,varFlux])),]
    
    # Construction de la map par defaut
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_oursins",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_oursins",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_oursins",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    # AFFICHAGE DE LA MAILLE
    
    map <- addPolygons(map = map, data = maille_WGS84, opacity = 1,
                       stroke = TRUE, color = "grey",
                       weight = 0.5,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                       fill = T, fillColor = "white", fillOpacity = 0.001,
                       group = list(nom_couche="carte_oursins",code_epsg=code_epsg,nom_fond="fond_maille")
    )
    
    # AFFICHAGE DE L'ANALYSE
    
    map <- addPolylines(map = map,
                        data = analyse_WGS84,
                        stroke = TRUE, color = "#303030",
                        opacity = 1,
                        weight = 2,
                        options = pathOptions(clickable = T),
                        popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                        group = list(nom_couche="carte_oursins",code_epsg=code_epsg,dom=dom,nom_fond="fond_flux",var_flux=varFlux)
    )
    
    return(map)
  }


leaflet_joignantes <-
  function(data,fondMaille,typeMaille,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax=NULL,filtreVol=0,filtreDist=100,filtreMajeurs=10,decalageAllerRetour=0,decalageCentroid=0,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error3 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(typeMaille)!="character")) msg_error4 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'COM') / "
    if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(largeurFlecheMax)) if(any(class(largeurFlecheMax)!="numeric")) msg_error8 <- "La largeur de la fleche max doit etre de type numerique (en km) / "
    if(any(class(filtreVol)!="numeric")) msg_error9 <- "Le filtre doit etre de type numerique / "
    if(any(class(filtreDist)!="numeric")) msg_error10 <- "Le filtre doit etre de type numerique / "
    if(any(class(filtreMajeurs)!="numeric")) msg_error11 <- "Le filtre doit etre de type numerique / "
    if(any(class(decalageAllerRetour)!="numeric")) msg_error12 <- "La variable decalageAllerRetour doit etre de type numerique / "
    if(any(class(decalageCentroid)!="numeric")) msg_error13 <- "La variable decalageCentroid doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error14 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<3) msg_error15 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable de depart, une variable d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error16 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error17 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "COM")) msg_error18 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'COM' / "
    if(!any(names(data) %in% idDataDepart))  msg_error19 <- "La variable de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error20 <- "La variable d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error21 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error22 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),!is.null(msg_error21),!is.null(msg_error22)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                              msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22)))
    }
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
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
      if (typeMaille=="COM") largeurFlecheMax<-2
    }
    
    data <- data[data$CODE1!=data$CODE2,]
    
    analyse<-k_joignantes(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeurFlecheMax*1000,decalageAllerRetour,decalageCentroid)
    
    analyse_WGS84 <- st_transform(analyse[[1]],"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    if(filtreMajeurs>nrow(analyse_WGS84))
    {
      nb_flux_majeur <- nrow(analyse_WGS84)
    }else
    {
      nb_flux_majeur <- filtreMajeurs
      if(nb_flux_majeur<1) nb_flux_majeur <- 1
    }
    
    analyse_WGS84_list <- split(analyse_WGS84,factor(analyse_WGS84$CODE1))
    analyse_WGS84_1 <- data.frame()
    aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_1 <<- rbind(analyse_WGS84_1,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
    analyse_WGS84_1 <- analyse_WGS84_1[!is.na(analyse_WGS84_1[,varFlux]),]
    
    analyse_WGS84_list <- split(analyse_WGS84,factor(analyse_WGS84$CODE2))
    analyse_WGS84_2 <- data.frame()
    aa <- lapply(1:length(analyse_WGS84_list), function(x) analyse_WGS84_2 <<- rbind(analyse_WGS84_2,as.data.frame(analyse_WGS84_list[[x]])[rev(order(as.data.frame(analyse_WGS84_list[[x]])[,varFlux]))[c(1:nb_flux_majeur)],]))
    analyse_WGS84_2 <- analyse_WGS84_2[!is.na(analyse_WGS84_2[,varFlux]),]
    
    analyse_WGS84 <- unique(rbind(analyse_WGS84_1,analyse_WGS84_2))
    
    analyse_WGS84 <- st_as_sf(analyse_WGS84)
    
    analyse_WGS84 <- analyse_WGS84[as.vector(st_length(analyse_WGS84)/2.2)<=filtreDist*1000,]
    
    analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=filtreVol,]
    
    analyse_WGS84 <- analyse_WGS84[rev(order(as.data.frame(analyse_WGS84)[,varFlux])),]
    
    donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
    
    donnees <- donnees[rev(order(donnees[,varFlux])),]
    
    coord_fleche_max_pl <- st_coordinates(analyse[[1]][abs(data.frame(analyse[[1]])[,varFlux])==max(donnees[,varFlux]),])
    large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[5,1],coord_fleche_max_pl[5,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
    
    # Construction de la map par defaut
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_joignantes",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_joignantes",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_joignantes",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    # AFFICHAGE DE LA MAILLE
    
    map <- addPolygons(map = map, data = maille_WGS84, opacity = 1,
                       stroke = TRUE, color = "grey",
                       weight = 0.5,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                       fill = T, fillColor = "white", fillOpacity = 0.001,
                       group = list(nom_couche="carte_joignantes",code_epsg=code_epsg,nom_fond="fond_maille")
    )
    
    # AFFICHAGE DE L'ANALYSE
    
    map <- addPolygons(map = map,
                       data = analyse_WGS84,
                       stroke = TRUE, color = "#303030",
                       opacity = 1,
                       weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                       fill = T, fillColor = "#CD853F", fillOpacity = 1,
                       group = list(nom_couche="carte_joignantes",code_epsg=code_epsg,dom=dom,nom_fond="fond_flux",var_flux=varFlux,max_var=max(donnees[,varFlux]),largeur=largeurFlecheMax,distance=large_pl)
    )
    
    return(map)
  }


leaflet_saphirs <-
  function(data,fondMaille,typeMaille,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax=NULL,direction="Ent",filtreVol=0,dom="0")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error3 <- "Le fond supplementaire doit etre un objet sf / "
    if(any(class(typeMaille)!="character")) msg_error4 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'COM') / "
    if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(largeurFlecheMax)) if(any(class(largeurFlecheMax)!="numeric")) msg_error8 <- "La largeur de la fleche max doit etre de type numerique (en km) / "
    if(any(class(direction)!="character")) msg_error9 <- "La direction des fleches doit etre de type caractere / "
    if(any(class(filtreVol)!="numeric")) msg_error10 <- "Le filtre doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error11 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(length(names(data))<3) msg_error12 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable de depart, une variable d'arrivee et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error13 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error14 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error15 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'COM' / "
    if(!any(names(data) %in% idDataDepart))  msg_error16 <- "La variable de depart n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% idDataArrivee))  msg_error17 <- "La variable d'arrivee n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varFlux))  msg_error18 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error19 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                              msg_error16,msg_error17,msg_error18,msg_error19)))
    }
    
    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSuppl)) 
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
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
      if (typeMaille=="COM") largeurFlecheMax<-2
    }
    
    if(typeMaille=="REG") longueur<-100000
    if(typeMaille=="DEP") longueur<-30000
    if(!typeMaille %in% c("REG","DEP")) longueur<-10000
    
    data <- data[data$CODE1!=data$CODE2,]
    
    analyse<-k_saphir(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,largeurFlecheMax*1000,longueur,direction)
    
    analyse_WGS84 <- st_transform(analyse[[1]],"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    # Fonds habillages
    
    if(dom=="0")
    {
      pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fra <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else
    {
      if(dom=="971")
      {
        fra <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="972")
      {
        fra <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="973")
      {
        fra <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(dom=="974")
      {
        fra <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
      if(dom=="976")
      {
        fra <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        pays <- fra
      }
    }
    fond_france <- fra
    fond_pays <- pays
    
    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    
    if(!is.null(fondSuppl))
    {
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    if(direction!="Sol")
    {
      analyse_WGS84 <- analyse_WGS84[as.data.frame(analyse_WGS84)[,varFlux]>=100,]
      donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
      donnees <- donnees[rev(order(donnees[,varFlux])),]
    }else
    {
      donnees <- as.data.frame(analyse_WGS84)
    }
    
    coord_fleche_max_pl <- st_coordinates(analyse[[1]][abs(data.frame(analyse[[1]])[,varFlux])==max(donnees[,varFlux]),])
    large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[5,1],coord_fleche_max_pl[5,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))
    
    # Construction de la map par defaut
    
    map <- leaflet(padding = 0,
                   options = leafletOptions(
                     preferCanvas = TRUE,
                     transition = 2,
                     minZoom = 6,
                     maxZoom = 10
                   )) %>%
      
      setMapWidgetStyle(list(background = "#AFC9E0")) %>%
      
      addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">\u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
      
      fitBounds(lng1 = min(list_bbox[[1]]),
                lat1 = min(list_bbox[[2]]),
                lng2 = max(list_bbox[[1]]),
                lat2 = max(list_bbox[[2]])
      ) %>%
      
      # On ajoute une barre d'echelle
      addScaleBar(position = 'bottomright',
                  options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      )
    
    # AFFICHAGE DES FONDS D'HABILLAGE
    if(dom %in% c("0","973"))
    {
      map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1, # fond_pays sauf la France
                         stroke = TRUE, color = "white",
                         weight = 1,
                         popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                         group = list(nom_couche="carte_saphirs",code_epsg=code_epsg,nom_fond="fond_pays")
                         
      )
    }
    
    map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                       stroke = TRUE, color = "black",
                       weight = 1.5,
                       popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                       options = pathOptions(clickable = F),
                       fill = T, fillColor = "white", fillOpacity = 1,
                       group = list(nom_couche="carte_saphirs",code_epsg=code_epsg,nom_fond="fond_france")
    )
    
    # AFFICHAGE DU FOND TERRITOIRE
    
    if(!is.null(fondSuppl))
    {
      map <- addPolygons(map = map, data = fond_territoire,
                         stroke = TRUE, color = "#BFBFBF", opacity = 1,
                         weight = 0.5,
                         options = pathOptions(clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = list(nom_couche="carte_saphirs",code_epsg=code_epsg,nom_fond="fond_territoire")
      )
    }
    
    # AFFICHAGE DE LA MAILLE
    
    map <- addPolygons(map = map, data = maille_WGS84, opacity = 1,
                       stroke = TRUE, color = "grey",
                       weight = 0.5,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                       fill = T, fillColor = "white", fillOpacity = 0.001,
                       group = list(nom_couche="carte_saphirs",code_epsg=code_epsg,nom_fond="fond_maille")
    )
    
    # AFFICHAGE DE L'ANALYSE
    
    map <- addPolygons(map = map,
                       data = analyse_WGS84,
                       stroke = TRUE, color = "#303030",
                       opacity = 1,
                       weight = 1,
                       options = pathOptions(clickable = T),
                       popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                       fill = T, fillColor = sapply(donnees[,varFlux], function(x) if(x>0){"#CD853F"}else{"#6495ED"}), fillOpacity = 1,
                       group = list(nom_couche="carte_saphirs",code_epsg=code_epsg,dom=dom,nom_fond="fond_flux",var_flux=varFlux,max_var=max(donnees[,varFlux]),largeur=largeurFlecheMax,distance=large_pl)
    )
    
    return(map)
  }
