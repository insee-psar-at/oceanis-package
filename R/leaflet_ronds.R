leaflet_ronds <-
function(data,fondMaille,fondMailleElargi=NULL,fondSuppl=NULL,idData,varVolume,rayonRond=NULL,rapportRond=NULL,dom="0",fondChx=NULL,colPos="#CD853F",colNeg="#6495ED",colBorder="white",opacityElargi=0.6,zoomMaille=NULL,map_proxy=NULL)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    leafletVerifParamRonds(data,fondMaille,fondMailleElargi,fondSuppl,idData,varVolume,rayonRond,rapportRond,dom,fondChx,colPos,colNeg,colBorder,opacityElargi,map_proxy)
    
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
      if(length(rayonRond)!=0)
      {
        if(!is.na(rayonRond))
        {
          if(rayonRond>max_rayon_metres)
          {
            if(!is.null(map_proxy))
            {
              showModal(modalDialog(HTML(paste0("Le rayon du rond le plus grand est trop \u00e9lev\u00e9 et ne permet pas de respecter la r\u00e8gle s\u00e9miologique des 1/7\u00e8me. Le rayon max est ",round(max_rayon_metres,2)," m\u00e8tres.")), size="l", footer=NULL, easyClose = TRUE, style = "color: #fff; background-color: #DF691A; border-color: #2e6da4")) #337ab7
            }else
            {
              stop(simpleError(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon max est ",round(max_rayon_metres,2)," metres.")))
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
    
    if(!is.null(fondMailleElargi))
    {
      fondMailleElargi <- st_transform(fondMailleElargi,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    }
    
    # CONSTRUCTION DE LA MAP EN LEAFLET
    
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
        addMapPane(name = "fond_territoire", zIndex = 403) %>%
        addMapPane(name = "fond_maille_elargi", zIndex = 404) %>%
        addMapPane(name = "fond_maille", zIndex = 405) %>%
        addMapPane(name = "fond_ronds_elargi", zIndex = 406) %>%
        addMapPane(name = "fond_ronds", zIndex = 407) %>%
        addMapPane(name = "fond_legende", zIndex = 408) %>%
        
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
                           options = pathOptions(pane = "fond_pays", clickable = F),
                           fill = T, fillColor = "#CCCCCC", fillOpacity = 1,
                           group = "carte_ronds_init",
                           layerId = list(code_epsg=code_epsg,nom_fond="fond_pays")
                           
        )
      }
      
      map <- addPolygons(map = map, data = fond_france[,"LIBGEO"], opacity = 1, # fond_france
                         stroke = TRUE, color = "black",
                         weight = 1.5,
                         popup = as.data.frame(fond_france[,"LIBGEO"])[,-ncol(as.data.frame(fond_france[,"LIBGEO"]))],
                         options = pathOptions(pane = "fond_france", clickable = F),
                         fill = T, fillColor = "white", fillOpacity = 1,
                         group = "carte_ronds_init",
                         layerId = list(code_epsg=code_epsg,nom_fond="fond_france")
      )
      
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
                           layerId = list(code_epsg=code_epsg,nom_fond="fond_territoire")
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
      map <- addPolygons(map = map, data = maille_WGS84_elargi,
                         stroke = TRUE, color = "grey", opacity = opacityElargi,
                         weight = 0.5,
                         options = pathOptions(pane = "fond_maille_elargi", clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(fondMailleElargi)[,"LIBELLE"], "</font> </b>"),
                         fill = T, fillColor = "white", fillOpacity = 0.001,
                         group = "carte_ronds_elargi",
                         layerId = list(maille_WGS84_elargi=maille_WGS84_elargi,code_epsg=code_epsg,nom_fond="fond_maille_elargi")
      )
    }
    
    if(!is.null(fondMailleElargi))
    {
      # AFFICHAGE DE L'ANALYSE ELARGIE
      
      map <- addCircles(map = map,
                        lng = st_coordinates(analyse_WGS84_elargi)[,1],
                        lat = st_coordinates(analyse_WGS84_elargi)[,2],
                        stroke = TRUE, color = colBorder,
                        opacity = opacityElargi,
                        weight = 1,
                        radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                        options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                        popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1),
                        fill = T,
                        fillColor = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colPos}else{colNeg}),
                        fillOpacity = opacityElargi,
                        group = "carte_ronds_elargi",
                        layerId = list(analyse=analyse,analyse_WGS84_elargi=analyse_WGS84_elargi,rayonRond=rayonRond,code_epsg=code_epsg,dom=dom,
                                       nom_fond=c(if(max(analyse$donnees_elargi$save)>0){"fond_ronds_pos_elargi_carte"}else{" "},
                                                  if(min(analyse$donnees_elargi$save)<0){"fond_ronds_neg_elargi_carte"}else{" "}),
                                      max_var=max_var,var_volume=varVolume,colPos=colPos,colNeg=colNeg,colBorder=colBorder)
      )
    }
    
    # AFFICHAGE DE L'ANALYSE
    
    map <- addCircles(map = map,
                      lng = st_coordinates(analyse_WGS84)[,1],
                      lat = st_coordinates(analyse_WGS84)[,2],
                      stroke = TRUE, color = colBorder,
                      opacity = 1,
                      weight = 1,
                      radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                      options = pathOptions(pane = "fond_ronds", clickable = T),
                      popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1),
                      fill = T,
                      fillColor = sapply(analyse$donnees$save, function(x) if(x>0){colPos}else{colNeg}),
                      fillOpacity = 1,
                      group = "carte_ronds",
                      layerId = list(analyse=analyse,analyse_WGS84=analyse_WGS84,rayonRond=rayonRond,code_epsg=code_epsg,dom=dom,
                                     nom_fond=c(if(max(analyse$donnees$save)>0){"fond_ronds_pos_carte"}else{" "},
                                                if(min(analyse$donnees$save)<0){"fond_ronds_neg_carte"}else{" "}),
                                   max_var=max_var,var_volume=varVolume,colPos=colPos,colNeg=colNeg,colBorder=colBorder)
    )
    
    return(map)
  }
