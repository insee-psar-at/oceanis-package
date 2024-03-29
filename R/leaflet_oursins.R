leaflet_oursins <-
function(data,fondMaille,fondSuppl=NULL,idDataDepart,idDataArrivee,varFlux,filtreVol=0,filtreDist=100,filtreMajeurs=10,decalageAllerRetour=0,decalageCentroid=0,emprise="FRM",fondEtranger=NULL,zoomMaille=NULL,map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    leafletVerifParamOursins(data,fondMaille,fondSuppl,idDataDepart,idDataArrivee,varFlux,filtreVol,filtreDist,filtreMajeurs,decalageAllerRetour,decalageCentroid,emprise,fondEtranger,map_proxy)

    names(data)[names(data)==idDataDepart] <- "CODE1"
    names(data)[names(data)==idDataArrivee] <- "CODE2"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSuppl))
    {
      names(fondSuppl)[1] <- "CODE"
      names(fondSuppl)[2] <- "LIBELLE"
      if(any(Encoding(fondSuppl$LIBELLE) %in% "latin1")){
        fondSuppl$LIBELLE<-iconv(fondSuppl$LIBELLE,"latin1","UTF-8")
      }
    }
    epsg_etranger <- NULL
    if(!is.null(fondEtranger))
    {
      names(fondEtranger)[1] <- "CODE"
      names(fondEtranger)[2] <- "LIBELLE"
      if(any(Encoding(fondEtranger$LIBELLE) %in% "latin1")){
        fondEtranger$LIBELLE<-iconv(fondEtranger$LIBELLE,"latin1","UTF-8")
      }

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
    if(any(Encoding(fondMaille$LIBELLE) %in% "latin1")){
      fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","UTF-8")
    }

    if(!is.null(map_proxy))
    {
      if(any(class(map_proxy) %in% "leaflet_proxy"))
      {
        clearGroup(map_proxy, group = "carte_oursins")
      }
    }

    code_epsg <- switch(emprise,
                        "FRM"="2154",# Lambert 93
                        "971"="5490",# UTM 20 N
                        "972"="5490",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471",# UTM 38 S
                        "999"=epsg_etranger)

    # Analyse

    data <- data[data$CODE1!=data$CODE2,]

    analyse<-k_oursins(fondMaille,names(fondMaille)[1],data,"CODE1","CODE2",varFlux,decalageAllerRetour,decalageCentroid)

    analyse_WGS84 <- st_transform(analyse[[1]],crs=4326)

    # Fonds habillages

    if(emprise=="FRM")
    {
      fond_pays <- st_transform(sf_paysm(),crs=4326)
      fond_france <- st_transform(sf_fram(),crs=4326)
    }else if(emprise!="999")
    {
      if(emprise=="971")
      {
        fond_france <- st_transform(sf_reg01(),crs=4326)
        fond_pays <- fond_france
      }
      if(emprise=="972")
      {
        fond_france <- st_transform(sf_reg02(),crs=4326)
        fond_pays <- fond_france
      }
      if(emprise=="973")
      {
        fond_france <- st_transform(sf_reg03(),crs=4326)
        fond_pays <- st_transform(sf_pays973(),crs=4326)
      }
      if(emprise=="974")
      {
        fond_france <- st_transform(sf_reg04(),crs=4326)
        fond_pays <- fond_france
      }
      if(emprise=="976")
      {
        fond_france <- st_transform(sf_reg06(),crs=4326)
        fond_pays <- fond_france
      }
    }else if(emprise=="999")
    {
      fond_etranger <- st_transform(fondEtranger,crs=4326)
      fond_pays <- fond_etranger
    }else{}

    maille_WGS84 <- st_transform(fondMaille,crs=4326)

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
      fond_territoire <- st_transform(fondSuppl,crs=4326)
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

    if(is.null(map_proxy) | (!is.null(map_proxy) & inherits(map_proxy,"character")))
    {
      if(is.null(fondEtranger))
      {
        proj4 <- st_crs(fondMaille)$proj4string
      }else{
        proj4 <- st_crs(fondEtranger)$proj4string
      }
      
      map <- leaflet(padding = 0,
                     options = leafletOptions(
                       preferCanvas = TRUE,
                       transition = 2,
                       crs = leafletCRS(crsClass = "L.Proj.CRS",
                                        code = paste0("EPSG:", code_epsg),
                                        proj4def = proj4,
                                        resolutions = 2^(16:1)
                       )
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
        addMapPane(name = "fond_oursins", zIndex = 406) %>%

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
                           group = "carte_oursins_init",
                           layerId = list(fond_pays=fond_pays,code_epsg=code_epsg,nom_fond="fond_pays")
        )

        map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_france", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_oursins_init",
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
                           group = "carte_oursins_init",
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
                           group = "carte_oursins_init",
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
                           group = "carte_oursins_init",
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
                         group = "carte_oursins_init",
                         layerId = list(maille_WGS84=maille_WGS84,code_epsg=code_epsg,nom_fond="fond_maille")
      )
    }else # Contexte shiny/proxy
    {
      map <- map_proxy
    }

    # AFFICHAGE DE L'ANALYSE
    map <- addPolylines(map = map,
                        data = analyse_WGS84,
                        stroke = TRUE, color = "#303030",
                        opacity = 1,
                        weight = 2,
                        options = pathOptions(pane = "fond_oursins", clickable = T),
                        popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                        group = "carte_oursins",
                        layerId = list(analyse_WGS84=analyse_WGS84,donnees=donnees,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_flux",var_flux=varFlux)
    )

    return(map)
  }
