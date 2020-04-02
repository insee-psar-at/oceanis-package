leaflet_classes_ronds <-
function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varVolume,varRatio,rayonRond=NULL,rapportRond=NULL,methode="kmeans",nbClasses=3,bornes=NULL,stylePalette="defaut",opacityElargi=0.6,colBorderClasses="white",colBorderRondsPos="#303030",colBorderRondsNeg="#303030",epaisseurBorder=1.5,precision=1,emprise="FRM",fondEtranger=NULL,fondChx=NULL,zoomMaille=NULL,map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres
    leafletVerifParamClassesRonds(data,fondMaille,fondMailleElargi,fondSuppl,idData,varVolume,varRatio,rayonRond,rapportRond,methode,nbClasses,bornes,stylePalette,opacityElargi,colBorderClasses,colBorderRondsPos,colBorderRondsNeg,epaisseurBorder,precision,emprise,fondEtranger,fondChx,map_proxy)

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

      epsg_etranger <- st_crs(fondEtranger)$epsg
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
        clearGroup(map_proxy, group = "carte_classes")
        clearGroup(map_proxy, group = "carte_ronds")
        clearGroup(map_proxy, group = "carte_ronds_elargi")
        clearGroup(map_proxy, group = "carte_classes_elargi")
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
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
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
      if(!is.na(rayonRond))
      {
        if(rayonRond>max_rayon_metres)
        {
          if(!is.null(map_proxy))
          {
            showModal(modalDialog(HTML(paste0("Le rayon du rond le plus grand est trop \u00e9lev\u00e9 et ne permet pas de respecter la r\u00e8gle s\u00e9miologique des 1/7\u00e8me. Le rayon max conseill\u00e9 est ",round(max_rayon_metres,2)," m\u00e8tres.")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          }else
          {
            message(simpleMessage(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon max conseille est ",round(max_rayon_metres,2)," metres.")))
          }
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

    # Analyse ronds

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
      suppressWarnings(test_bornes_analyse <- try(classIntervals(as.numeric(analyse$donnees[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"),silent=TRUE))

      if(!class(test_bornes_analyse) %in% "try-error")
      {
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(analyse$donnees[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"))
      }else
      {
        if(!is.null(map_proxy))
        {
          showModal(modalDialog(HTML("<font size=+1>Le nombre de classes n'est pas adapt\u00e9 \u00e0 l'analyse.</font>"), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
          return(map_proxy)
        }else
        {
          stop(simpleError("Le nombre de classes n'est pas adapte a l'analyse."))
        }
      }

      suppressWarnings(test_calcul_bornes <- try(calcul_bornes(analyse$donnees,bornes_analyse,varRatio,nbClasses,methode,stylePalette),silent=TRUE))

      if(!class(test_calcul_bornes) %in% "try-error")
      {
        carac_bornes <- calcul_bornes(analyse$donnees,bornes_analyse,varRatio,nbClasses,methode,stylePalette)
      }else
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

      pal_classes_pos <- recup_palette(stylePalette)[[1]]
      pal_classes_neg <- recup_palette(stylePalette)[[2]]

      nb_col_pos <- length(pal_classes_pos)

      if(min<0 & max>=0) # Si - et +
      {
        nb_col <- length(pal_classes_pos)
        pal_classes_pos <- pal_classes_pos[(nb_col_pos-length(bornes[bornes>0])+1):nb_col_pos]
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
          pal_classes <- pal_classes_pos[-c(1:(nb_col_pos-length(bornes)+1))] # On enleve les couleurs fonces inutiles
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

    if(elargi)
    {
      analyse_WGS84_elargi <- st_transform(analyse$analyse_points_elargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
      maille_WGS84_elargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
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
      fond_territoire <- st_transform(fondSuppl,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }

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
        addMapPane(name = "fond_classes_elargi", zIndex = 405) %>%
        addMapPane(name = "fond_classes", zIndex = 406) %>%
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
                           group = "carte_classes_ronds_init",
                           layerId = list(fond_pays=fond_pays,code_epsg=code_epsg,nom_fond="fond_pays")
        )

        map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1,
                           stroke = TRUE, color = "black",
                           weight = 1.5,
                           popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                           options = pathOptions(pane = "fond_france", clickable = T),
                           fill = T, fillColor = "white", fillOpacity = 1,
                           group = "carte_classes_ronds_init",
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
                           group = "carte_classes_ronds_init",
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
                           group = "carte_classes_ronds_init",
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
                           group = "carte_classes_ronds_init",
                           layerId = list(fond_territoire=fond_territoire,code_epsg=code_epsg,nom_fond="fond_territoire")
        )
      }
    }else # Contexte shiny/proxy
    {
      map <- map_proxy
    }

    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE EN CLASSES ELARGIE

      analyse_maille_classe_elargi <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varRatio])),varRatio]

      analyse_maille_elargi <- merge(maille_WGS84_elargi[,c("CODE","geometry")],analyse$donnees_elargi,by="CODE")
      analyse_maille_elargi <- analyse_maille_elargi[rev(order(as.data.frame(analyse_maille_elargi)[,varRatio])),c("CODE","LIBELLE",varRatio,"TXT1","TXT2","geometry")]
      analyse_maille_elargi <- st_sf(analyse_maille_elargi,stringsAsFactors = FALSE)

      map <- addPolygons(map = map, data = analyse_maille_elargi, opacity = opacityElargi,
                         stroke = TRUE, color = colBorderClasses, weight = 1,
                         options = pathOptions(pane = "fond_classes_elargi", clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT2),
                         fill = T,
                         fillColor = palette(analyse_maille_classe_elargi),
                         fillOpacity = opacityElargi,
                         group = "carte_classes_elargi",
                         layerId = list(analyse_maille_elargi=analyse_maille_elargi,analyse_maille_classe_elargi=analyse_maille_classe_elargi,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_maille_elargi_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style=stylePalette,palette=pal_classes,col_border_classes=colBorderClasses)
      )
    }

    # AFFICHAGE DE LA MAILLE ET DE L'ANALYSE EN CLASSES

    analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varRatio])),varRatio]

    analyse_maille <- merge(maille_WGS84[,c("CODE","geometry")],analyse$donnees,by="CODE")
    analyse_maille <- analyse_maille[rev(order(as.data.frame(analyse_maille)[,varRatio])),c("CODE","LIBELLE",varRatio,"TXT1","TXT2","geometry")]
    analyse_maille <- st_sf(analyse_maille,stringsAsFactors = FALSE)

    map <- addPolygons(map = map, data = analyse_maille, opacity = 1,
                       stroke = TRUE, color = colBorderClasses, weight = 1,
                       options = pathOptions(pane = "fond_classes", clickable = T),
                       popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille)$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT2),
                       fill = T,
                       fillColor = palette(analyse_maille_classe),
                       fillOpacity = 1,
                       group = "carte_classes",
                       layerId = list(analyse_maille=analyse_maille,analyse_maille_classe=analyse_maille_classe,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_maille_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style=stylePalette,palette=pal_classes,col_border_classes=colBorderClasses)
    )

    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE L'ANALYSE EN RONDS ELARGIE

      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_elargi)[,1],
                        lat = st_coordinates(analyse_WGS84_elargi)[,2],
                        stroke = TRUE, color = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colBorderRondsPos}else{colBorderRondsNeg}),
                        opacity = opacityElargi,
                        weight = epaisseurBorder,
                        radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                        options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                        popup = paste0("<b> <font color=#2B3E50>",analyse$donnees_elargi$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1),
                        fill = F,
                        group = "carte_ronds_elargi",
                        layerId = list(analyse_WGS84_elargi=analyse_WGS84_elargi,analyse=analyse,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_classes_ronds_elargi_carte",max_var=max_var,var_volume=varVolume,rayonRond=rayonRond,col_border_ronds_pos=colBorderRondsPos,col_border_ronds_neg=colBorderRondsNeg,epaisseurBorder=epaisseurBorder)
      )
    }

    # AFFICHAGE DE L'ANALYSE EN RONDS

    map <- addCircles(map = map,
                      lng = st_coordinates(analyse_WGS84)[,1],
                      lat = st_coordinates(analyse_WGS84)[,2],
                      stroke = TRUE, color = sapply(analyse$donnees$save, function(x) if(x>0){colBorderRondsPos}else{colBorderRondsNeg}),
                      opacity = 1,
                      weight = epaisseurBorder,
                      radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                      options = pathOptions(pane = "fond_ronds", clickable = T),
                      popup = paste0("<b> <font color=#2B3E50>",analyse$donnees$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1),
                      fill = F,
                      group = "carte_ronds",
                      layerId = list(analyse_WGS84=analyse_WGS84,analyse=analyse,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_classes_ronds_carte",max_var=max_var,var_volume=varVolume,rayonRond=rayonRond,col_border_ronds_pos=colBorderRondsPos,col_border_ronds_neg=colBorderRondsNeg,epaisseurBorder=epaisseurBorder)
    )

    return(map)
  }
