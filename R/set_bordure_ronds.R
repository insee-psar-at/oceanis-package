set_bordure_ronds <-
function(map,colBorderPos="white",colBorderNeg="white",epaisseurBorder=1,map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(colBorderPos)!="character")) msg_error2 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorderNeg)!="character")) msg_error3 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(epaisseurBorder)!="numeric")) msg_error4 <- "L'epaiseeur de la bordure doit etre de type numerique / "
    if(!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error5 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5)))
    }
    
    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }
    
    idx_maille <- NULL
    idx_carte <- NULL
    classes <- F
    ronds_classes <- F
    classes_ronds <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_ronds","carte_ronds_elargi","carte_classes","carte_classes_elargi"))
        {
          idx_maille <- c(idx_maille,i)
        }
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes_init"))
        {
          classes <- T
        }
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_ronds_classes_init"))
        {
          classes <- T
          ronds_classes <- T
        }
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes_ronds_init"))
        {
          classes <- T
          classes_ronds <- T
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds","carte_ronds_elargi"))
        {
          idx_carte <- c(idx_carte,i)
        }
      }
    }
    
    if(is.null(map_leaflet)) # contexte leaflet
    {
      for(i in 1:length(idx_carte))
      {
        varVolume <- map$x$calls[[idx_carte[i]]]$args[[4]]$var_volume
        valeurs <- map$x$calls[[idx_carte[i]]]$args[[4]]$analyse$donnees[,varVolume]
        
        val_pos <- which(valeurs>=0)
        
        if(length(val_pos)>0)
        {
          map$x$calls[[idx_carte[i]]]$args[[6]]$color <- colBorderPos
        }
        
        val_neg <- which(valeurs<0)
        
        if(length(val_neg)>0)
        {
          map$x$calls[[idx_carte[i]]]$args[[6]]$color <- colBorderNeg
        }
        
        map$x$calls[[idx_carte[i]]]$args[[6]]$weight <- epaisseurBorder
      }
    }else # contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy
      
      clearGroup(map, group = "carte_ronds")
      clearGroup(map, group = "carte_ronds_elargi")
      
      analyse_WGS84 <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$analyse_WGS84
      analyse <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$analyse
      code_epsg <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$code_epsg
      emprise <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$emprise
      max_var <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$max_var
      varVolume <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$var_volume
      rayonRond <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$rayonRond
      
      if(!classes) # Analyse en ronds
      {
        if(any(map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$nom_fond %in% c("fond_ronds_pos_elargi_carte","fond_ronds_neg_elargi_carte"))) # analyse elargie
        {
          maille_WGS84_elargi <- map_leaflet$x$calls[[idx_maille[1]]]$args[[2]]$maille_WGS84_elargi
          analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$analyse_WGS84_elargi
          opacityElargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[6]]$fillOpacity
          
          colorPos <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$colPos
          colorNeg <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$colNeg
          
          map <- addPolygons(map = map, data = maille_WGS84_elargi,
                             stroke = TRUE, color = "grey", opacity = opacityElargi,
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
                            stroke = TRUE, color = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colBorderPos}else{colBorderNeg}),
                            opacity = opacityElargi,
                            weight = epaisseurBorder,
                            radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                            options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1),
                            fill = T,
                            fillColor = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colorPos}else{colorNeg}),
                            fillOpacity = opacityElargi,
                            group = "carte_ronds_elargi",
                            layerId = list(analyse=analyse,analyse_WGS84_elargi=analyse_WGS84_elargi,rayonRond=rayonRond,code_epsg=code_epsg,emprise=emprise,
                                           nom_fond=c(if(max(analyse$donnees_elargi$save)>0){"fond_ronds_pos_elargi_carte"}else{" "},
                                                      if(min(analyse$donnees_elargi$save)<0){"fond_ronds_neg_elargi_carte"}else{" "}),
                                           max_var=max_var,var_volume=varVolume,colPos=colorPos,colNeg=colorNeg,colBorderPos=colBorderPos,colBorderNeg=colBorderNeg,epaisseurBorder=epaisseurBorder)
          )
        }
        
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = sapply(analyse$donnees$save, function(x) if(x>0){colBorderPos}else{colBorderNeg}),
                          opacity = 1,
                          weight = epaisseurBorder,
                          radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1),
                          fill = T,
                          fillColor = sapply(analyse$donnees$save, function(x) if(x>0){colorPos}else{colorNeg}),
                          fillOpacity = 1,
                          group = "carte_ronds",
                          layerId = list(analyse=analyse,analyse_WGS84=analyse_WGS84,rayonRond=rayonRond,code_epsg=code_epsg,emprise=emprise,
                                         nom_fond=c(if(max(analyse$donnees$save)>0){"fond_ronds_pos_carte"}else{" "},
                                                    if(min(analyse$donnees$save)<0){"fond_ronds_neg_carte"}else{" "}),
                                         max_var=max_var,var_volume=varVolume,colPos=colorPos,colNeg=colorNeg,colBorderPos=colBorderPos,colBorderNeg=colBorderNeg,epaisseurBorder=epaisseurBorder)
        )
        
      }else if(classes_ronds) # Analyse classes_ronds
      {
        if(any(map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$nom_fond %in% "fond_classes_ronds_elargi_carte")) # analyse elargie
        {
          analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$analyse_WGS84_elargi
          analyse_maille_elargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$analyse_maille_elargi
          opacityElargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[6]]$fillOpacity
          
          map <- addCircles(map = map,
                            lng = st_coordinates(analyse_WGS84_elargi)[,1],
                            lat = st_coordinates(analyse_WGS84_elargi)[,2],
                            stroke = TRUE, color = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colBorderPos}else{colBorderNeg}),
                            opacity = opacityElargi,
                            weight = epaisseurBorder,
                            radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                            options = pathOptions(pane = "fond_ronds_elargi", clickable = F),
                            popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille_elargi)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille_elargi)$TXT1),
                            fill = F,
                            group = "carte_ronds_elargi",
                            layerId = list(analyse_WGS84_elargi=analyse_WGS84_elargi,analyse=analyse,analyse_maille_elargi=analyse_maille_elargi,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_classes_ronds_elargi_carte",max_var=max_var,var_volume=varVolume,rayonRond=rayonRond,col_border_ronds_pos=colBorderPos,col_border_ronds_neg=colBorderNeg,epaisseurBorder=epaisseurBorder)
          )
        }
        
        analyse_maille <- map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$analyse_maille
        
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = sapply(analyse$donnees$save, function(x) if(x>0){colBorderPos}else{colBorderNeg}),
                          opacity = 1,
                          weight = epaisseurBorder,
                          radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds", clickable = F),
                          popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_maille)$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",as.data.frame(analyse_maille)$TXT1),
                          fill = F,
                          group = "carte_ronds",
                          layerId = list(analyse_WGS84=analyse_WGS84,analyse=analyse,analyse_maille=analyse_maille,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_classes_ronds_carte",max_var=max_var,var_volume=varVolume,rayonRond=rayonRond,col_border_ronds_pos=colBorderPos,col_border_ronds_neg=colBorderNeg,epaisseurBorder=epaisseurBorder)
        )
      }else if(ronds_classes) # Analyse ronds_classes
      {
        varRatio <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$var_ratio
        bornes <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$bornes
        precision <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$precision
        stylePalette <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$stylePalette
        pal_classes <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$pal_classes
        
        if(any(map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$nom_fond %in% "fond_classes_ronds_elargi_carte")) # analyse elargie
        {
          analyse_WGS84_elargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[4]]$analyse_WGS84_elargi
          opacityElargi <- map_leaflet$x$calls[[idx_carte[1]]]$args[[6]]$fillOpacity
          
          analyse_maille_classe_elargi <- analyse$donnees_elargi[rev(order(analyse$donnees_elargi[,varVolume])),varRatio]
          
          map <- addCircles(map = map,
                            lng = st_coordinates(analyse_WGS84_elargi)[,1],
                            lat = st_coordinates(analyse_WGS84_elargi)[,2],
                            stroke = TRUE, color = sapply(analyse$donnees_elargi$save, function(x) if(x>0){colBorderPos}else{colBorderNeg}),
                            opacity = opacityElargi,
                            weight = epaisseurBorder,
                            radius = rayonRond*sqrt(analyse$donnees_elargi[,varVolume]/max_var),
                            options = pathOptions(pane = "fond_ronds_elargi", clickable = T),
                            popup = paste0("<b> <font color=#2B3E50>",analyse$donnees_elargi$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees_elargi$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees_elargi$TXT2),
                            fill = T,
                            fillColor = palette(analyse_maille_classe_elargi),
                            fillOpacity = opacityElargi,
                            group = "carte_ronds_elargi",
                            layerId = list(analyse_WGS84_elargi=analyse_WGS84_elargi,analyse=analyse,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_ronds_classes_elargi_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,rayonRond=rayonRond,precision=precision,style=stylePalette,palette=pal_classes,col_border_ronds_pos=colBorderPos,col_border_ronds_neg=colBorderNeg,epaisseurBorder=epaisseurBorder)
          )
        }
        
        analyse_maille_classe <- analyse$donnees[rev(order(analyse$donnees[,varVolume])),varRatio]
        
        map <- addCircles(map = map,
                          lng = st_coordinates(analyse_WGS84)[,1],
                          lat = st_coordinates(analyse_WGS84)[,2],
                          stroke = TRUE, color = sapply(analyse$donnees$save, function(x) if(x>0){colBorderPos}else{colBorderNeg}),
                          opacity = 1,
                          weight = epaisseurBorder,
                          radius = rayonRond*sqrt(analyse$donnees[,varVolume]/max_var),
                          options = pathOptions(pane = "fond_ronds", clickable = T),
                          popup = paste0("<b> <font color=#2B3E50>",analyse$donnees$LIBELLE, "</font> </b><br><b> <font color=#2B3E50>",varVolume," : </font></b>",analyse$donnees$TXT1,"<br><b><font color=#2B3E50>",varRatio," : </font></b>",analyse$donnees$TXT2),
                          fill = T,
                          fillColor = palette(analyse_maille_classe),
                          fillOpacity = 1,
                          group = "carte_ronds",
                          layerId = list(analyse_WGS84=analyse_WGS84,analyse=analyse,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_ronds_classes_carte",bornes=bornes,max_var=max_var,var_ratio=varRatio,var_volume=varVolume,rayonRond=rayonRond,precision=precision,style=stylePalette,palette=pal_classes,col_border_ronds_pos=colBorderPos,col_border_ronds_neg=colBorderNeg,epaisseurBorder=epaisseurBorder)
        )
      }else
      {
        stop(simpleWarning("L'apparence de la bordure des ronds ne s'applique que pour les analyses en ronds, en classes dans les ronds (ronds_classes) ou en ronds sur les classes (classes_ronds)"))
      }
    }
    
    return(map)
  }
