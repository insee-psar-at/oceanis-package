leaflet_typo <-
function(data,fondMaille,fondSuppl=NULL,idData,varTypo,emprise="FRM",fondEtranger=NULL,zoomMaille=NULL,map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    leafletVerifParamTypo(data,fondMaille,fondSuppl,idData,varTypo,emprise,fondEtranger,map_proxy)

    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSuppl))
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","utf8")
    }
    epsg_etranger <- NULL
    if(!is.null(fondEtranger))
    {
      names(fondEtranger)[1] <- "CODE"
      names(fondEtranger)[2] <- "LIBELLE"
      fondEtranger$LIBELLE<-iconv(fondEtranger$LIBELLE,"latin1","utf8")

      epsg_etranger <- st_crs(fondEtranger)$epsg
      if(is.na(epsg_etranger) | epsg_etranger=="4326")
      {
        epsg_etranger <- "3395" # Mercator
      }
    }
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")

    if(!is.null(map_proxy))
    {
      if(any(class(map_proxy) %in% "leaflet_proxy"))
      {
        clearGroup(map_proxy, group = "carte_typo")
      }
    }

    code_epsg <- switch(emprise,
                        "FRM"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471",# UTM 38 S
                        "999"=epsg_etranger)

    # Analyse

    analyse<-k_typo(fondMaille,names(fondMaille)[!sapply(fondMaille[-length(names(fondMaille))],is.numeric)][1],data,"CODE",varTypo)

    analyse <- analyse[[1]]
    analyse[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(as.data.frame(analyse)[,varTypo], big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    analyse_WGS84 <- st_transform(analyse,"+init=epsg:4326 +proj=longlat +ellps=WGS84")

    # Fonds habillages

    if(emprise=="FRM")
    {
      fond_pays <- st_transform(sf_paysm(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fond_france <- st_transform(sf_fram(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }else if(emprise!="999")
    {
      if(emprise=="971")
      {
        fond_france <- st_transform(sf_reg01(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        fond_pays <- fond_france
      }
      if(emprise=="972")
      {
        fond_france <- st_transform(sf_reg02(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        fond_pays <- fond_france
      }
      if(emprise=="973")
      {
        fond_france <- st_transform(sf_reg03(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        fond_pays <- st_transform(sf_pays973(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      }
      if(emprise=="974")
      {
        fond_france <- st_transform(sf_reg04(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        fond_pays <- fond_france
      }
      if(emprise=="976")
      {
        fond_france <- st_transform(sf_reg06(),"+init=epsg:4326 +proj=longlat +ellps=WGS84")
        fond_pays <- fond_france
      }
    }else if(emprise=="999")
    {
      fond_etranger <- st_transform(fondEtranger,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      fond_pays <- fond_etranger
    }else{}

    maille_WGS84 <- st_transform(fondMaille,"+init=epsg:4326 +proj=longlat +ellps=WGS84")

    if(!is.null(zoomMaille))
    {
      zoom_maille_WGS84 <- maille_WGS84[maille_WGS84$CODE %in% zoomMaille,]
      if(nrow(zoom_maille_WGS84)>0)
      {
        list_bbox <- list(c(st_bbox(zoom_maille_WGS84)[1],st_bbox(zoom_maille_WGS84)[3]),c(st_bbox(zoom_maille_WGS84)[2],st_bbox(zoom_maille_WGS84)[4]))
      }else
      {
        list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
      }
    }else
    {
      list_bbox <- list(c(st_bbox(maille_WGS84)[1],st_bbox(maille_WGS84)[3]),c(st_bbox(maille_WGS84)[2],st_bbox(maille_WGS84)[4]))
    }

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

    if(is.null(map_proxy) | (!is.null(map_proxy) & class(map_proxy)=="character"))
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

        # Pour gerer l'ordre des calques
        addMapPane(name = "fond_pays", zIndex = 401) %>%
        addMapPane(name = "fond_france", zIndex = 402) %>%
        addMapPane(name = "fond_etranger", zIndex = 403) %>%
        addMapPane(name = "fond_territoire", zIndex = 404) %>%
        addMapPane(name = "fond_typo", zIndex = 405) %>%
        addMapPane(name = "fond_legende", zIndex = 406) %>%

        # On ajoute une barre d'echelle
        addScaleBar(position = 'bottomright',
                    options = scaleBarOptions(metric = TRUE, imperial = FALSE)
        )

      # AFFICHAGE DES FONDS D'HABILLAGE

      if(emprise %in% c("FRM","973")) # France metro ou Guyane
      {
        map <- addPolygons(map = map, data = fond_pays[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "white",
                           weight = 1,
                           popup = as.data.frame(fond_pays[,"LIBGEO"])[,-ncol(as.data.frame(fond_pays[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_pays", clickable = T),
                           fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                           group = "carte_typo_init",
                           layerId = list(fond_pays=fond_pays,code_epsg=code_epsg,nom_fond="fond_pays")
        )

        map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_france", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_typo_init",
                           layerId = list(fond_france=fond_france,code_epsg=code_epsg,nom_fond="fond_france")
        )
      }else if(!emprise %in% c("999")) # 971, 972, 974 ou 976
      {
        map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_france", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_typo_init",
                           layerId = list(fond_france=fond_france,code_epsg=code_epsg,nom_fond="fond_france")
        )
      }else if(emprise %in% c("999")) # Etranger
      {
        map <- addPolygons(map = map, data = fond_etranger[,"LIBELLE"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1,
                           popup = as.data.frame(fond_etranger[,"LIBELLE"])[,-ncol(as.data.frame(fond_etranger[,"LIBELLE"]))],
                           options = pathOptions(pane = "fond_etranger", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_typo_init",
                           layerId = list(fond_etranger=fond_etranger,code_epsg=code_epsg,nom_fond="fond_etranger")
        )
      }

      # AFFICHAGE DU FOND TERRITOIRE

      if(!is.null(fondSuppl))
      {
        map <- addPolygons(map = map, data = fond_territoire,
                           stroke = TRUE, color = "#BFBFBF", opacity = 1,
                           weight = 0.5,
                           options = pathOptions(pane = "fond_territoire", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fond_territoire)[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "carte_typo_init",
                           layerId = list(fond_territoire=fond_territoire,code_epsg=code_epsg,nom_fond="fond_territoire")
        )
      }
    }else # Contexte shiny/proxy
    {
      map <- map_proxy
    }

    # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE

    map <- addPolygons(map = map, data = analyse_WGS84, opacity = 1,
                       stroke = TRUE, color = "white", weight = 1,
                       options = pathOptions(pane = "fond_typo", clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_WGS84)[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_WGS84$TXT1,"<br>"),
                       fill = T,
                       fillColor = analyse$col,
                       fillOpacity = 1,
                       group = "carte_typo",
                       layerId = list(analyse_WGS84=analyse_WGS84,analyse=analyse,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_maille_typo_carte",var_typo=varTypo)
    )

    return(map)
  }
