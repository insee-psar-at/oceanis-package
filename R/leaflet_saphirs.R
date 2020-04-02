leaflet_saphirs <-
function(data,fondMaille,typeMaille,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax=NULL,direction="Ent",filtreVol=0,colEntree="#CD853F",colSortie="#6495ED",colBorder="#303030",emprise="FRM",fondEtranger=NULL,zoomMaille=NULL,map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    leafletVerifParamSaphirs(data,fondMaille,typeMaille,fondSuppl,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax,direction,filtreVol,colEntree,colSortie,colBorder,emprise,fondEtranger,map_proxy)

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
    epsg_etranger <- NULL
    if(!is.null(fondEtranger))
    {
      names(fondEtranger)[1] <- "CODE"
      names(fondEtranger)[2] <- "LIBELLE"
      fondEtranger$LIBELLE<-iconv(fondEtranger$LIBELLE,"latin1","utf8")

      if(substr(st_crs(fondEtranger)[1]$input,1,5) == "EPSG:")
      {
        epsg_etranger <- substr(st_crs(fondEtranger)[1]$input,6,9)
      }else
      {
        epsg_etranger <- st_crs(fondEtranger)[1]$input
      }

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
        clearGroup(map_proxy, group = "carte_saphirs")
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

    if(direction!="Sol")
    {
      analyse_WGS84 <- analyse_WGS84[abs(as.data.frame(analyse_WGS84)[,varFlux])>=100,]
      donnees <- merge(as.data.frame(analyse_WGS84)[,c("CODE1","CODE2")],data,by=c("CODE1","CODE2"),all.x=T)
      donnees <- donnees[rev(order(donnees[,varFlux])),]
    }else
    {
      donnees <- as.data.frame(analyse_WGS84)
    }

    coord_fleche_max_pl <- st_coordinates(analyse[[1]][abs(data.frame(analyse[[1]])[,varFlux])==max(abs(donnees[,varFlux])),])
    large_pl <- max(st_distance(st_sfc(st_point(c(coord_fleche_max_pl[2,1],coord_fleche_max_pl[2,2])),st_point(c(coord_fleche_max_pl[6,1],coord_fleche_max_pl[6,2])))))

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
        addMapPane(name = "fond_maille", zIndex = 405) %>%
        addMapPane(name = "fond_saphirs", zIndex = 406) %>%
        addMapPane(name = "fond_legende", zIndex = 407) %>%

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
                           group = "carte_saphirs_init",
                           layerId = list(fond_pays=fond_pays,code_epsg=code_epsg,nom_fond="fond_pays")
        )

        map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_france", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_saphirs_init",
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
                           group = "carte_saphirs_init",
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
                           group = "carte_saphirs_init",
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
                           group = "carte_saphirs_init",
                           layerId = list(fond_territoire=fond_territoire,code_epsg=code_epsg,nom_fond="fond_territoire")
        )
      }

      # AFFICHAGE DE LA MAILLE

      map <- addPolygons(map = map, data = maille_WGS84, opacity = 1,
                         stroke = TRUE, color = "grey",
                         weight = 0.5,
                         options = pathOptions(pane = "fond_maille", clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = "carte_saphirs_init",
                         layerId = list(maille_WGS84=maille_WGS84,code_epsg=code_epsg,nom_fond="fond_maille")
      )
    }else # Contexte shiny/proxy
    {
      map <- map_proxy
    }

    # AFFICHAGE DE L'ANALYSE

    if(max(donnees[,varFlux])<0) colEntree <- NULL
    if(min(donnees[,varFlux])>=0) colSortie <- NULL

    map <- addPolygons(map = map,
                       data = analyse_WGS84,
                       stroke = TRUE, color = colBorder,
                       opacity = 1,
                       weight = 1,
                       options = pathOptions(pane = "fond_saphirs", clickable = T),
                       popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                       fill = T, fillColor = sapply(donnees[,varFlux], function(x) if(x>0){colEntree}else{colSortie}), fillOpacity = 1,
                       group = "carte_saphirs",
                       layerId = list(analyse_WGS84=analyse_WGS84,donnees=donnees,colEntree=colEntree,colSortie=colSortie,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_flux",var_flux=varFlux,max_var=max(abs(donnees[,varFlux])),largeur=largeurFlecheMax,distance=large_pl)
    )

    return(map)
  }
