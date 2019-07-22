leaflet_typo <-
function(data,fondMaille,fondSuppl=NULL,idData,varTypo,dom="0",map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13 <- NULL
    
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
    
    if (!is.null(map_proxy)) if(!any(class(map_proxy) %in% "leaflet_proxy")) msg_error13 <- "La carte doit etre un objet leaflet_proxy / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),!is.null(msg_error13)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13)))
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
    
    if(!is.null(map_proxy))
    {
      map <- map_proxy
      clearGroup(map, group = "carte_typo")
    }
    
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
    
    if(is.null(map_proxy))
    {
      map <- leaflet(padding = 0,
                     options = leafletOptions(
                       preferCanvas = TRUE,
                       transition = 2
                     )) %>%
        
        setMapWidgetStyle(list(background = "#AFC9E0")) %>%
        
        addTiles_insee(attribution = paste0("<a href=\"http://www.insee.fr\">OCEANIS - \u00A9 IGN - INSEE ",format(Sys.time(), format = "%Y"),"</a>")) %>%
        
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
                           group = list(nom_couche="carte_typo_init",code_epsg=code_epsg,nom_fond="fond_pays")
                           
        )
      }
      
      map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                         stroke = TRUE, color = "black",
                         weight = 1.5,
                         popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                         options = pathOptions(clickable = F),
                         fill = T, fillColor = "white", fillOpacity = 1,
                         group = list(nom_couche="carte_typo_init",code_epsg=code_epsg,nom_fond="fond_france")
      )
    }
    
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
