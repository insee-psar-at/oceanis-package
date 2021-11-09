leaflet_ronds <-
function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varVolume,rayonRond=NULL,rapportRond=NULL,emprise="FRM",fondEtranger=NULL,fondChx=NULL,colPos="#EB617F",colNeg="#286AC7",colBorderPos="white",colBorderNeg="white",epaisseurBorder=1,opacityElargi=0.6,zoomMaille=NULL,map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    leafletVerifParamRonds(data,fondMaille,fondMailleElargi,fondSuppl,idData,varVolume,rayonRond,rapportRond,emprise,fondEtranger,fondChx,colPos,colNeg,colBorderPos,colBorderNeg,epaisseurBorder,opacityElargi,map_proxy)

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
    if(!is.null(fondChx))
    {
      names(fondChx)[1] <- "CODE"
    }

    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")

    if(!is.null(map_proxy))
    {
      if(any(class(map_proxy) %in% "leaflet_proxy")) # Contexte shiny/proxy
      {
        clearGroup(map_proxy, group = "carte_ronds")
        clearGroup(map_proxy, group = "carte_ronds_elargi")
      }
    }

    if(is.null(fondMailleElargi))
    {
      elargi <- FALSE
    }else
    {
      elargi <- TRUE
    }

    code_epsg <- switch(emprise,
                        "FRM"="2154",# Lambert 93
                        "971"="5490",# UTM 20 N
                        "972"="5490",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471",# UTM 38 S
                        "999"=epsg_etranger)

    # Calcul du rayon du rond max

    #Aire totale du territoire d'etude
    aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
    #valeur max de la serie de donnees
    suppressWarnings(max_var <- max(abs(data[data[,"CODE"] %in% fondMaille$CODE,varVolume]), na.rm = TRUE))

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
      if(length(rayonRond)!=0)
      {
        if(!is.na(rayonRond))
        {
          if(rayonRond>max_rayon_metres)
          {
            if(!is.null(map_proxy))
            {
              showModal(modalDialog(HTML(paste0("Le rayon du rond le plus grand est trop \u00e9lev\u00e9 et ne permet pas de respecter la r\u00e8gle s\u00e9miologique des 1/7\u00e8me. Le rayon max conseill\u00e9 est ",round(max_rayon_metres,2)," m\u00e8tres.")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            }else
            {
              message(simpleMessage(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon conseille max est ",round(max_rayon_metres,2)," metres.")))
            }
          }
        }else
        {
          rayonRond <- max_rayon_metres
        }
      }else
      {
        rayonRond <- max_rayon_metres
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
      if(!is.null(map_proxy))
      {
        showModal(modalDialog(HTML(paste0("<font size=+1>La maille ne correspond pas au niveau g\u00e9ographique du fichier de donn","\u00e9","es.<br><br>Veuillez svp choisir une maille adapt","\u00e9","e ou modifier le fichier de donn","\u00e9","es.</font>")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
        return(map_proxy)
      }else
      {
        stop(simpleError("La maille ne correspond pas au niveau geographique du fichier de donnees. Veuillez svp choisir une maille adaptee ou modifier le fichier de donnees"))
      }
    }

    analyse$donnees[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    if(elargi)
    {
      analyse$donnees_elargi[,"TXT1"] <- paste0("<b> <font color=#2B3E50>",format(analyse$donnees_elargi$save, big.mark=" ",decimal.mark=",",nsmall=0),"</font></b>")
    }

    analyse_WGS84 <- st_transform(analyse$analyse_points,crs=4326)

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

    if(elargi)
    {
      analyse_WGS84_elargi <- st_transform(analyse$analyse_points_elargi,crs=4326)
      maille_WGS84_elargi <- st_transform(fondMailleElargi,crs=4326)
    }

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

    if(!is.null(fondMailleElargi))
    {
      fondMailleElargi <- st_transform(fondMailleElargi,crs=4326)
    }

    # CONSTRUCTION DE LA MAP EN LEAFLET

    if(is.null(map_proxy) | (!is.null(map_proxy) & class(map_proxy)=="character"))
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
        addMapPane(name = "fond_etranger", zIndex = 402) %>%
        addMapPane(name = "fond_france", zIndex = 403) %>%
        addMapPane(name = "fond_territoire", zIndex = 404) %>%
        addMapPane(name = "fond_maille_elargi", zIndex = 405) %>%
        addMapPane(name = "fond_maille", zIndex = 406) %>%
        addMapPane(name = "fond_ronds_elargi", zIndex = 407) %>%
        addMapPane(name = "fond_ronds", zIndex = 408) %>%
        addMapPane(name = "fond_legende", zIndex = 409) %>%

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
                           group = "carte_ronds_init",
                           layerId = list(fond_pays=fond_pays,code_epsg=code_epsg,nom_fond="fond_pays")
        )

        map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_france", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_ronds_init",
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
                           group = "carte_ronds_init",
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
                           group = "carte_ronds_init",
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
                           fill = F,
                           group = "carte_ronds_init",
                           layerId = list(fond_territoire=fond_territoire,code_epsg=code_epsg,nom_fond="fond_territoire")
        )
      }

      # AFFICHAGE DE LA MAILLE

      map <- addPolygons(map = map, data = maille_WGS84, opacity = 1, #maille_WGS84
                         stroke = TRUE, color = "grey", weight = 1,
                         options = pathOptions(pane = "fond_maille", clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = "carte_ronds_init",
                         layerId = list(maille_WGS84=maille_WGS84,code_epsg=code_epsg,nom_fond="fond_maille")
      )

    }else # Contexte shiny/proxy
    {
      map <- map_proxy
    }

    if(!is.null(fondMailleElargi))
    {
      map <- addPolygons(map = map,
                         data = maille_WGS84_elargi,
                         stroke = TRUE, color = "grey", opacity = opacityElargi,
                         weight = 0.5,
                         options = pathOptions(pane = "fond_maille_elargi", clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fondMailleElargi)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = "carte_ronds_elargi",
                         layerId = list(maille_WGS84_elargi=maille_WGS84_elargi,code_epsg=code_epsg,nom_fond="fond_maille_elargi")
      )
      
      # AFFICHAGE DE L'ANALYSE ELARGIE

      if(max(analyse$donnees_elargi$save) > 0)
      {
        idx <- which(analyse$donnees_elargi$save > 0)
        analyse_pos <- list()
        analyse_pos$analyse_points <- analyse$analyse_points[idx]
        analyse_pos$donnees_elargi <- analyse$donnees_elargi[idx,]
        analyse_WGS84_elargi_pos <- analyse_WGS84_elargi[idx]
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84_elargi_pos)[,1],
                          lat = st_coordinates(analyse_WGS84_elargi_pos)[,2],
                          stroke = TRUE,
                          color = colBorderPos,
                          opacity = opacityElargi,
                          weight = epaisseurBorder,
                          radius = rayonRond*sqrt(analyse_pos$donnees_elargi[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_pos$donnees_elargi[,"TXT1"]),
                          fill = T,
                          fillColor = colPos,
                          fillOpacity = opacityElargi,
                          group = "carte_ronds",
                          layerId = list(analyse=analyse_pos,analyse_WGS84_elargi=analyse_WGS84_elargi_pos,rayonRond=rayonRond,code_epsg=code_epsg,emprise=emprise,
                                         nom_fond="fond_ronds_pos_elargi_carte",
                                         max_var=max_var,var_volume=varVolume,colPos=colPos,colBorderPos=colBorderPos,epaisseurBorder=epaisseurBorder)
        )
      }
      
      if(min(analyse$donnees_elargi$save) < 0)
      {
        idx <- which(analyse$donnees_elargi$save < 0)
        analyse_neg <- list()
        analyse_neg$analyse_points <- analyse$analyse_points[idx]
        analyse_neg$donnees_elargi <- analyse$donnees_elargi[idx,]
        analyse_WGS84_elargi_neg <- analyse_WGS84_elargi[idx]
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84_elargi_neg)[,1],
                          lat = st_coordinates(analyse_WGS84_elargi_neg)[,2],
                          stroke = TRUE,
                          color = colBorderNeg,
                          opacity = opacityElargi,
                          weight = epaisseurBorder,
                          radius = rayonRond*sqrt(analyse_neg$donnees_elargi[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_neg$donnees_elargi[,"TXT1"]),
                          fill = T,
                          fillColor = colNeg,
                          fillOpacity = opacityElargi,
                          group = "carte_ronds",
                          layerId = list(analyse=analyse_neg,analyse_WGS84_elargi=analyse_WGS84_elargi_pos,rayonRond=rayonRond,code_epsg=code_epsg,emprise=emprise,
                                         nom_fond="fond_ronds_neg_elargi_carte",
                                         max_var=max_var,var_volume=varVolume,colNeg=colNeg,colBorderNeg=colBorderNeg,epaisseurBorder=epaisseurBorder)
        )
      }
    }
    
    # AFFICHAGE DE L'ANALYSE

    if(max(analyse$donnees$save) > 0)
    {
      idx <- which(analyse$donnees$save > 0)
      analyse_pos <- list()
      analyse_pos$analyse_points <- analyse$analyse_points[idx]
      analyse_pos$donnees <- analyse$donnees[idx,]
      analyse_WGS84_pos <- analyse_WGS84[idx]
      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_pos)[,1],
                        lat = st_coordinates(analyse_WGS84_pos)[,2],
                        stroke = TRUE,
                        color = colBorderPos,
                        opacity = 1,
                        weight = epaisseurBorder,
                        radius = rayonRond*sqrt(analyse_pos$donnees[,varVolume]/max_var),
                        options = pathOptions(pane = "fond_ronds", clickable = T),
                        popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_pos$donnees[,"TXT1"]),
                        fill = T,
                        fillColor = colPos,
                        fillOpacity = 1,
                        group = "carte_ronds",
                        layerId = list(analyse=analyse_pos,analyse_WGS84=analyse_WGS84_pos,rayonRond=rayonRond,code_epsg=code_epsg,emprise=emprise,
                                       nom_fond="fond_ronds_pos_carte",
                                       max_var=max_var,var_volume=varVolume,colPos=colPos,colBorderPos=colBorderPos,epaisseurBorder=epaisseurBorder)
      )
    }

    if(min(analyse$donnees$save) < 0)
    {
      idx <- which(analyse$donnees$save < 0)
      analyse_neg <- list()
      analyse_neg$analyse_points <- analyse$analyse_points[idx]
      analyse_neg$donnees <- analyse$donnees[idx,]
      analyse_WGS84_neg <- analyse_WGS84[idx]
      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_neg)[,1],
                        lat = st_coordinates(analyse_WGS84_neg)[,2],
                        stroke = TRUE,
                        color = colBorderNeg,
                        opacity = 1,
                        weight = epaisseurBorder,
                        radius = rayonRond*sqrt(analyse_neg$donnees[,varVolume]/max_var),
                        options = pathOptions(pane = "fond_ronds", clickable = T),
                        popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse_neg$donnees[,"TXT1"]),
                        fill = T,
                        fillColor = colNeg,
                        fillOpacity = 1,
                        group = "carte_ronds",
                        layerId = list(analyse=analyse_neg,analyse_WGS84=analyse_WGS84_neg,rayonRond=rayonRond,code_epsg=code_epsg,emprise=emprise,
                                       nom_fond="fond_ronds_neg_carte",
                                       max_var=max_var,var_volume=varVolume,colNeg=colNeg,colBorderNeg=colBorderNeg,epaisseurBorder=epaisseurBorder)
      )
    }

    return(map)
  }
