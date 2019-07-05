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
