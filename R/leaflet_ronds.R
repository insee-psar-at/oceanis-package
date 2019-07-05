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
