set_opacite_elargi <-
function(map,opacite=0.6,map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(opacite)!="numeric")) msg_error2 <- "L'opacite doit etre de type numerique (entre 0 et 1) / "
    if(opacite<0 | opacite>1) msg_error3 <- "L'opacite doit etre compris entre 0 (transparent) et 1 (opaque) / "
    if (!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error6 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }
    
    idx_maille <- NULL
    idx_carte_ronds <- NULL
    ronds <- F
    classes <- F
    ronds_classes <- F
    classes_ronds <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons") # maille et/ou analyse en classes
      {
        if(any(map$x$calls[[i]]$args[[3]] %in% c("carte_ronds_elargi","carte_classes_elargi")))
        {
          if(any(map$x$calls[[i]]$args[[3]] %in% c("carte_classes_elargi"))) classes <- T
          if(map$x$calls[[i]]$args[[2]]$nom_fond %in% c("fond_maille_elargi","fond_maille_elargi_carte")) idx_maille <- i
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles") # ronds
      {
        if(any(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds_elargi")))
        {
          ronds <- T
          if(any(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_ronds_classes_elargi_carte"))) ronds_classes <- T
          if(any(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_classes_ronds_elargi_carte"))) classes_ronds <- T
          if(any(map$x$calls[[i]]$args[[4]]$nom_fond %in% c("fond_ronds_pos_elargi_carte","fond_ronds_neg_elargi_carte","fond_ronds_classes_elargi_carte","fond_classes_ronds_elargi_carte"))) idx_carte_ronds <- i
        }
      }
    }
    
    if(is.null(map_leaflet)) # contexte leaflet
    {
      if(!is.null(idx_maille))
      {
        for(i in 1:length(idx_maille))
        {
          map$x$calls[[idx_maille]]$args[[4]]$opacity <- opacite
          map$x$calls[[idx_maille]]$args[[4]]$fillOpacity <- opacite
        }
      }
      if(!is.null(idx_carte_ronds))
      {
        for(i in 1:length(idx_carte_ronds))
        {
          map$x$calls[[idx_carte_ronds]]$args[[6]]$opacity <- opacite
          map$x$calls[[idx_carte_ronds]]$args[[6]]$fillOpacity <- opacite
        }
      }
    }else # contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy

      if(classes==T & ronds==F)
      {
        clearGroup(map, group = "carte_classes_elargi")
        
        analyse_maille_elargi <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$analyse_maille_elargi
        analyse_maille_classe_elargi <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$analyse_maille_classe_elargi
        code_epsg <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$code_epsg
        dom <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$dom
        varRatio <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$var_ratio
        bornes <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$bornes
        precision <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$precision
        stylePalette <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$style
        pal <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$palette
        col_border_classes <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$col_border_classes
        
        palette<-colorBin(palette=rev(pal), domain=0:100, bins=bornes, na.color="grey")
        
        map <- addPolygons(map = map, data = analyse_maille_elargi, opacity = opacite,
                           stroke = TRUE, color = col_border_classes, weight = 1,
                           options = pathOptions(pane = "fond_classes_elargi", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                           fill = T,
                           fillColor = palette(analyse_maille_classe_elargi),
                           fillOpacity = opacite,
                           group = "carte_classes_elargi",
                           layerId = list(analyse_maille_elargi=analyse_maille_elargi,analyse_maille_classe_elargi=analyse_maille_classe_elargi,code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_elargi_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style=stylePalette,palette=pal,col_border_classes=col_border_classes)
        )
      }else if(ronds==T & classes==F)
      {
        clearGroup(map, group = "carte_ronds_elargi")
        
        maille_WGS84_elargi <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$maille_WGS84_elargi
        analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$analyse_WGS84_elargi
        analyse <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$analyse
        code_epsg <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$code_epsg
        dom <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$dom
        max_var <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$max_var
        varVolume <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$var_volume
        rayonRond <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$rayonRond
        colPos <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$colPos
        colNeg <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$colNeg
        colBorder <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$colBorder
        
        map <- addPolygons(map = map, data = maille_WGS84_elargi,
                           stroke = TRUE, color = "grey", opacity = opacite,
                           weight = 0.5,
                           options = pathOptions(pane = "fond_maille_elargi", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84_elargi)[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "carte_ronds_elargi",
                           layerId = list(maille_WGS84_elargi=maille_WGS84_elargi,code_epsg=code_epsg,nom_fond="fond_maille_elargi")
        )
        
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84_elargi)[,1],
                          lat = st_coordinates(analyse_WGS84_elargi)[,2],
                          stroke = TRUE, color = colBorder,
                          opacity = opacite,
                          weight = 1,
                          radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1),
                          fill = T,
                          fillColor = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colPos}else{colNeg}),
                          fillOpacity = opacite,
                          group = "carte_ronds_elargi",
                          layerId = list(analyse=analyse,analyse_WGS84_elargi=analyse_WGS84_elargi,rayonRond=rayonRond,code_epsg=code_epsg,dom=dom,
                                         nom_fond=c(if(max(analyse$donnees_elargi$save)>0){"fond_ronds_pos_elargi_carte"}else{" "},
                                                    if(min(analyse$donnees_elargi$save)<0){"fond_ronds_neg_elargi_carte"}else{" "}),
                                         max_var=max_var,var_volume=varVolume,colPos=colPos,colNeg=colNeg,colBorder=colBorder)
        )
        
      }else if(ronds_classes==T)
      {
        clearGroup(map, group = "carte_classes_elargi")
        clearGroup(map, group = "carte_ronds_elargi")
        
        maille_WGS84_elargi <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$maille_WGS84_elargi
        analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$analyse_WGS84_elargi
        analyse <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$analyse
        code_epsg <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$code_epsg
        dom <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$dom
        varRatio <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$var_ratio
        bornes <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$bornes
        precision <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$precision
        stylePalette <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$style
        pal <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$palette
        max_var <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$max_var
        varVolume <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$var_volume
        rayonRond <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$rayonRond
        col_border_classes <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$col_border_classes
        
        palette<-colorBin(palette=rev(pal), domain=0:100, bins=bornes, na.color="grey")
        
        map <- addPolygons(map = map, data = maille_WGS84_elargi, opacity = opacite, #maille_WGS84
                           stroke = TRUE, color = "grey", weight = 1,
                           options = pathOptions(pane = "fond_classes_elargi", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(maille_WGS84_elargi)[,"LIBELLE"], "</font> </b>"),
                           fill = T, fillColor = "white", fillOpacity = 0.001,
                           group = "carte_classes_elargi",
                           layerId = list(maille_WGS84_elargi=maille_WGS84_elargi,code_epsg=code_epsg,nom_fond="fond_maille_elargi")
        )
        
        analyse_maille_classe_elargi <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varVolume])),varRatio]
        
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84_elargi)[,1],
                          lat = st_coordinates(analyse_WGS84_elargi)[,2],
                          stroke = TRUE, color = col_border_classes,
                          opacity = opacite,
                          weight = 1,
                          radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",analyse$donnees_elargi$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees_elargi$TXT2),
                          fill = T,
                          fillColor = palette(analyse_maille_classe_elargi),
                          fillOpacity = opacite,
                          group = "carte_ronds_elargi",
                          layerId = list(analyse_WGS84_elargi=analyse_WGS84_elargi,analyse=analyse,code_epsg=code_epsg,dom=dom,nom_fond="fond_ronds_classes_elargi_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,rayonRond=rayonRond,precision=precision,style=stylePalette,palette=pal,col_border_classes=col_border_classes)
        )
      }else if(classes_ronds==T)
      {
        clearGroup(map, group = "carte_classes_elargi")
        clearGroup(map, group = "carte_ronds_elargi")
        
        analyse_maille_elargi <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$analyse_maille_elargi
        analyse_maille_classe_elargi <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$analyse_maille_classe_elargi
        analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$analyse_WGS84_elargi
        analyse <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$analyse
        code_epsg <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$code_epsg
        dom <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$dom
        varRatio <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$var_ratio
        bornes <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$bornes
        precision <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$precision
        stylePalette <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$style
        pal <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$palette
        max_var <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$max_var
        varVolume <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$var_volume
        rayonRond <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$rayonRond
        col_border_classes <- map_leaflet$x$calls[[idx_maille]]$args[[2]]$col_border_classes
        col_border_ronds <- map_leaflet$x$calls[[idx_carte_ronds]]$args[[4]]$col_border_ronds
        
        palette<-colorBin(palette=rev(pal), domain=0:100, bins=bornes, na.color="grey")
        
        map <- addPolygons(map = map, data = analyse_maille_elargi, opacity = opacite,
                           stroke = TRUE, color = col_border_classes, weight = 1,
                           options = pathOptions(pane = "fond_classes_elargi", clickable = T),
                           popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT2),
                           fill = T,
                           fillColor = palette(analyse_maille_classe_elargi),
                           fillOpacity = opacite,
                           group = "carte_classes_elargi",
                           layerId = list(analyse_maille_elargi=analyse_maille_elargi,analyse_maille_classe_elargi=analyse_maille_classe_elargi,code_epsg=code_epsg,dom=dom,nom_fond="fond_maille_elargi_carte",bornes=bornes,var_ratio=varRatio,precision=precision,style=stylePalette,palette=pal,col_border_classes=col_border_classes)
        )
        
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84_elargi)[,1],
                          lat = st_coordinates(analyse_WGS84_elargi)[,2],
                          stroke = TRUE, color = col_border_ronds,
                          opacity = opacite,
                          weight = 1.5,
                          radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds_elargi", clickable = F),
                          popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                          fill = F,
                          group = "carte_ronds_elargi",
                          layerId = list(analyse_WGS84_elargi=analyse_WGS84_elargi,analyse=analyse,code_epsg=code_epsg,dom=dom,nom_fond="fond_classes_ronds_elargi_carte",max_var=max_var,var_volume=varVolume,rayonRond=rayonRond,col_border_ronds=col_border_ronds)
        )
      }else
      {}
    }
    
    return(map)
  }
