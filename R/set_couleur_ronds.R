set_couleur_ronds <-
function(map,colorPos="#CD853F",colorNeg="#6495ED",map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(colorPos)) if(any(class(colorPos)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(colorNeg)) if(any(class(colorNeg)!="character")) msg_error3 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error4 <- "La carte doit etre un objet leaflet / "
    
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
    idx_carte <- NULL
    classes <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_ronds","carte_ronds_elargi","carte_classes","carte_classes_elargi"))
        {
          idx_maille <- c(idx_maille,i)
        }
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes","carte_classes_elargi"))
        {
          classes <- T
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
        if(!classes)
        {
          valeurs <- map$x$calls[[idx_carte[i]]]$args[[4]]$analyse$donnees$save
          
          val_pos <- which(valeurs>=0)
          
          if(length(val_pos)>0)
          {
            map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor[val_pos] <- colorPos
          }
          
          val_neg <- which(valeurs<0)
          
          if(length(val_neg)>0)
          {
            map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor[val_neg] <- colorNeg
          }
        }
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
          
          colBorderPos <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$colBorderPos
          colBorderNeg <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$colBorderNeg
          epaisseurBorder <- map_leaflet$x$calls[[idx_carte[2]]]$args[[4]]$epaisseurBorder
          
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
                                           max_var=max_var,var_volume=varVolume,colBorderPos=colBorderPos,colBorderNeg=colBorderNeg,epaisseurBorder=epaisseurBorder)
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
                                         max_var=max_var,var_volume=varVolume,colBorderPos=colBorderPos,colBorderNeg=colBorderNeg,epaisseurBorder=epaisseurBorder)
        )
      }else
      {
        stop(simpleWarning("L'apparence de la couleur de remplissage des ronds ne s'applique que pour les analyses en ronds. Pour les analyses en classes dans les ronds (ronds_classes) ou en ronds sur les classes (classes_ronds), passez par la fonction set_couleur_classes()."))
      }
    }
    
    return(map)
  }
