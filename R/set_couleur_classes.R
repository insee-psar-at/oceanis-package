set_couleur_classes <-
function(map,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL,colBorder="white",map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6 <- NULL

    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(stylePalette)!="character")) msg_error2 <- "Le style de la palette doit etre de type caractere ('defaut', 'Insee_Rouge', 'Insee_Jaune', 'Insee_Bleu', 'Insee_Turquoise', 'Insee_Vert', 'Insee_Violet' ou 'Insee_Gris') / "
    if(!is.null(palettePos)) if(any(class(palettePos)!="character")) msg_error3 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(!is.null(paletteNeg)) if(any(class(paletteNeg)!="character")) msg_error4 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error5 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error6 <- "La carte doit etre un objet leaflet / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),!is.null(msg_error6)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6)))
    }

    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }

    idx_carte <- NULL
    idx_legende <- NULL
    ronds <- F
    legende <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[[3]] %in% c("carte_classes","carte_classes_elargi")))
        {
          if(any(map$x$calls[[i]]$args[[2]]$nom_fond %in% c("fond_maille_carte","fond_maille_elargi_carte")))
            idx_carte <- c(idx_carte,i)
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_ronds_classes_carte","fond_ronds_classes_elargi_carte"))
        {
          idx_carte <- c(idx_carte,i)
          ronds <- T
        }
      }
     
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_classes") idx_legende <- c(idx_legende,i)
      }
    }

    for(i in 1:length(idx_carte))
    {
      if(ronds) arg <- 4 else arg <- 2

      bornes <- map$x$calls[[idx_carte[i]]]$args[[arg]]$bornes

      if(min(bornes) < 0 & max(bornes) > 0)
      {
        if(0 %in% bornes)
        {
          nb_col_pos <- length(bornes[bornes>0])
          nb_col_neg <- length(bornes[bornes<0])
          col_classe_zero <- c()
        }else
        {
          nb_col_pos <- length(bornes[bornes>0])-1
          nb_col_neg <- length(bornes[bornes<0])-1
          col_classe_zero <- recup_palette(stylePalette = "Gris", nbPos = 6)[[1]][1]
        }
      }else if(min(bornes) >= 0)
      {
        nb_col_pos <- length(bornes[bornes>0])-1
        nb_col_neg <- 0
        col_classe_zero <- c()
      }else if(max(bornes) <= 0)
      {
        nb_col_pos <- 0
        nb_col_neg <- length(bornes[bornes<0])-1
        col_classe_zero <- c()
      }
      
      if(is.null(palettePos) & is.null(paletteNeg))
      {
        pal_new <- recup_palette(stylePalette = stylePalette, nbNeg = nb_col_neg, nbPos = nb_col_pos)[[1]]
        pal_new <- c(pal_new[0:nb_col_neg], col_classe_zero, pal_new[(nb_col_neg + 1):(length(pal_new) + 1)])
        pal_new <- pal_new[!is.na(pal_new)]
      }else
      {
        couleur_pos <- NULL
        couleur_neg <- NULL
        if(!is.null(palettePos) & nb_col_pos > 0) couleur_pos <- palettePos[c((length(palettePos) - nb_col_pos + 1):length(palettePos))]
        if(!is.null(paletteNeg) & nb_col_neg > 0) couleur_neg <- paletteNeg[c(1:nb_col_neg)]
        pal_new <- c(couleur_neg,couleur_pos)
      }

      style_anc <- map$x$calls[[idx_carte[i]]]$args[[arg]]$style
      map$x$calls[[idx_carte[i]]]$args[[arg]]$style <- stylePalette

      if(ronds) arg <- 6 else arg <- 4

      pal_anc <- data.frame(col = recup_palette(stylePalette = style_anc, nbNeg = nb_col_neg, nbPos = nb_col_pos)[[1]])
      
      couleur_analyse <- data.frame(col=map$x$calls[[idx_carte[i]]]$args[[arg]]$fillColor)
      couleur_analyse$id1 <- c(1:nrow(couleur_analyse))

      pal_anc$id2 <- c(1:nrow(pal_anc))
      couleur_analyse <- merge(couleur_analyse,pal_anc,by="col")

      aa <- sapply(1:(length(pal_new)), function(x) couleur_analyse[couleur_analyse$id2==x,"col"] <<- pal_new[x])
      rm(aa)
      couleur_analyse <- couleur_analyse[order(couleur_analyse$id1),]
      couleur_analyse <- couleur_analyse$col

      map$x$calls[[idx_carte[i]]]$args[[arg]]$fillColor <- couleur_analyse

      if(!ronds)
      {
        map$x$calls[[idx_carte[i]]]$args[[arg]]$color <- colBorder
      }
    }

    if(!is.null(idx_legende))
    {
      for(i in 1:length(idx_legende))
      {
        map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor <- rev(pal_new)[i]
      }
    }

    if(!is.null(map_leaflet)) # Contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy
      if(ronds) arg <- 4 else arg <- 2

      if(!is.null(idx_legende)) clearGroup(map, group = "legende_classes")

      for(i in 1:length(idx_carte))
      {
        varRatio <- map_leaflet$x$calls[[idx_carte[i]]]$args[[arg]]$var_ratio
        code_epsg <- map_leaflet$x$calls[[idx_carte[i]]]$args[[arg]]$code_epsg
        emprise <- map_leaflet$x$calls[[idx_carte[i]]]$args[[arg]]$emprise
        precision <- map_leaflet$x$calls[[idx_carte[i]]]$args[[arg]]$precision

        pal_new[is.na(pal_new)] <- "grey"
        palette<-colorBin(palette=rev(pal_new), domain=0:100, bins=bornes, na.color="grey")

        if(!ronds) # analyses en classes et en classes_ronds
        {
          if(map_leaflet$x$calls[[idx_carte[i]]]$args[[2]]$nom_fond %in% "fond_maille_elargi_carte") # analyse elargie
          {
            clearGroup(map, group = "carte_classes_elargi")

            analyse_maille_elargi <- map_leaflet$x$calls[[idx_carte[i]]]$args[[2]]$analyse_maille_elargi
            analyse_maille_classe_elargi <- map_leaflet$x$calls[[idx_carte[i]]]$args[[2]]$analyse_maille_classe_elargi
            opacityElargi <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$fillOpacity

            map <- addPolygons(map = map, data = analyse_maille_elargi, opacity = opacityElargi,
                               stroke = TRUE, color = colBorder, weight = 1,
                               options = pathOptions(pane = "fond_classes_elargi", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                               fill = T,
                               fillColor = palette(analyse_maille_classe_elargi),
                               fillOpacity = opacityElargi,
                               group = "carte_classes_elargi",
                               layerId = list(analyse_maille_elargi=analyse_maille_elargi,analyse_maille_classe_elargi=analyse_maille_classe_elargi,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_maille_elargi_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style=stylePalette,palette=pal_new,col_border_classes=colBorder)
            )
          }else
          {
            clearGroup(map, group = "carte_classes")

            analyse_maille <- map_leaflet$x$calls[[idx_carte[i]]]$args[[2]]$analyse_maille
            analyse_maille_classe <- map_leaflet$x$calls[[idx_carte[i]]]$args[[2]]$analyse_maille_classe

            map <- addPolygons(map = map, data = analyse_maille, opacity = 1,
                               stroke = TRUE, color = colBorder, weight = 1,
                               options = pathOptions(pane = "fond_classes", clickable = T),
                               popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                               fill = T,
                               fillColor = palette(analyse_maille_classe),
                               fillOpacity = 1,
                               group = "carte_classes",
                               layerId = list(analyse_maille=analyse_maille,analyse_maille_classe=analyse_maille_classe,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_maille_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style=stylePalette,palette=pal_new,col_border_classes=colBorder)
            )
          }
        }else if(ronds) #analyse en ronds_classes
        {
          analyse <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$analyse
          analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$analyse_WGS84_elargi
          varVolume <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$var_volume
          rayonRond <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$rayonRond
          max_var <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$max_var
          col_border_ronds_pos <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$col_border_ronds_pos
          col_border_ronds_neg <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$col_border_ronds_neg
          epaisseurBorder <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$epaisseurBorder

          if(map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$nom_fond %in% "fond_ronds_classes_elargi_carte")
          {
            clearGroup(map, group = "carte_ronds_elargi")

            analyse_maille_classe_elargi <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varVolume])),varRatio]

            opacityElargi <- map_leaflet$x$calls[[idx_carte[i]]]$args[[6]]$fillOpacity

            map <- addCircles(map = map,
                              lng = st_coordinates(analyse_WGS84_elargi)[,1],
                              lat = st_coordinates(analyse_WGS84_elargi)[,2],
                              stroke = TRUE, color = sapply(analyse$donnees_elargi$save, function(x) if(x>0){col_border_ronds_pos}else{col_border_ronds_neg}),
                              opacity = opacityElargi,
                              weight = epaisseurBorder,
                              radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                              options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",analyse$donnees_elargi$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees_elargi$TXT2),
                              fill = T,
                              fillColor = palette(analyse_maille_classe_elargi),
                              fillOpacity = opacityElargi,
                              group = "carte_ronds_elargi",
                              layerId = list(analyse_WGS84_elargi=analyse_WGS84_elargi,analyse_maille_classe_elargi=analyse_maille_classe_elargi,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_ronds_classes_elargi_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,rayonRond=rayonRond,precision=precision,style=stylePalette,palette=pal_new,col_border_ronds_pos=col_border_ronds_pos,col_border_ronds_neg=col_border_ronds_neg,epaisseurBorder=epaisseurBorder)
            )
          }else
          {
            clearGroup(map, group = "carte_ronds")

            analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio]
            analyse_WGS84 <- map_leaflet$x$calls[[idx_carte[i]]]$args[[4]]$analyse_WGS84

            map <- addCircles(map = map,
                              lng = st_coordinates(analyse_WGS84)[,1],
                              lat = st_coordinates(analyse_WGS84)[,2],
                              stroke = TRUE, color = sapply(analyse$donnees$save, function(x) if(x>0){col_border_ronds_pos}else{col_border_ronds_neg}),
                              opacity = 1,
                              weight = epaisseurBorder,
                              radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                              options = pathOptions(pane = "fond_ronds", clickable = T),
                              popup = paste0("<b> <font color=#2B3E50>",analyse$donnees$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees$TXT2),
                              fill = T,
                              fillColor = palette(analyse_maille_classe),
                              fillOpacity = 1,
                              group = "carte_ronds",
                              layerId = list(analyse_WGS84=analyse_WGS84,analyse_maille_classe=analyse_maille_classe,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_ronds_classes_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,rayonRond=rayonRond,precision=precision,style=stylePalette,palette=pal_new,col_border_ronds_pos=col_border_ronds_pos,col_border_ronds_neg=col_border_ronds_neg,epaisseurBorder=epaisseurBorder)
            )
          }
        }else
        {}
      }

      idx_titre <- NULL
      for(i in 1:length(map_leaflet$x$calls))
      {
        if(map_leaflet$x$calls[[i]]$method %in% "addControl")
        {
          if(any(map_leaflet$x$calls[[i]]$args[[4]]=="map-title")) idx_titre <- i
        }
      }

      if(!is.null(idx_titre)) titre <- map_leaflet$x$calls[[idx_titre]]$args[[1]] else titre = NULL

      if(!is.null(idx_legende))
      {
        for(i in 1:length(map_leaflet$x$calls))
        {
          if(map_leaflet$x$calls[[i]]$method %in% "addRectangles")
          {
            lat <- map_leaflet$x$calls[[i]]$args[[1]]
            lng <- map_leaflet$x$calls[[i]]$args[[2]]
          }
        }

        typeLegende <- map_leaflet$x$calls[[idx_legende[length(idx_legende)]]]$args[[2]]$typeLegende
        zoom <- map_leaflet$x$calls[[idx_legende[length(idx_legende)]]]$args[[2]]$zoom

        coeff <- ((360/(2^zoom))/7.2)
        lat <- lat-coeff*0.5
        lng <- lng+coeff*0.5

        map <- add_legende_classes(map,titre=titre,lng=lng,lat=lat,typeLegende=typeLegende,zoom=zoom,map_leaflet=map_leaflet)
      }
    }

    return(map)
  }
