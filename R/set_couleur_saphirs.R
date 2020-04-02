set_couleur_saphirs <-
function(map,colEntree="#CD853F",colSortie="#6495ED",colBorder="black",map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL

    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(colEntree)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colSortie)!="character")) msg_error3 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error4 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
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

    idx_fleche <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[2]]$nom_fond=="fond_flux") idx_fleche <- i
        if(map$x$calls[[i]]$args[[2]]$nom_fond=="fond_flux_leg") idx_legende <- i
      }
    }

    if(is.null(map_leaflet)) # contexte leaflet
    {
      val_ent <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_fleche]]$args[[5]],str_locate(map$x$calls[[idx_fleche]]$args[[5]],":")[[1]]+2,nchar(map$x$calls[[idx_fleche]]$args[[5]])-11)," ",""))>=0)

      if(length(val_ent)>0)
      {
        map$x$calls[[idx_fleche]]$args[[4]]$fillColor[1:length(val_ent)] <- colEntree
      }

      val_sor <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_fleche]]$args[[5]],str_locate(map$x$calls[[idx_fleche]]$args[[5]],":")[[1]]+2,nchar(map$x$calls[[idx_fleche]]$args[[5]])-11)," ",""))<0)

      if(length(val_sor)>0)
      {
        map$x$calls[[idx_fleche]]$args[[4]]$fillColor[length(val_ent)+1:length(val_sor)] <- colSortie
      }

      map$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder

      if(!is.null(idx_legende))
      {
        if(length(val_ent)>0 & length(val_sor)==0)
        {
          map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colEntree
        }else if(length(val_ent)==0 & length(val_sor)>0)
        {
          map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colSortie
        }else
        {
          map$x$calls[[idx_legende]]$args[[4]]$fillColor <- "transparent"
        }
        map$x$calls[[idx_legende]]$args[[4]]$color <- colBorder
      }

    }else # contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy

      clearGroup(map, group = "carte_saphirs")

      analyse_WGS84 <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$analyse_WGS84
      donnees <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$donnees
      code_epsg <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$code_epsg
      emprise <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$emprise
      varFlux <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$var_flux
      max_var <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$max_var
      largeurFlecheMax <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$largeur
      large_pl <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$distance

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

      if(!is.null(idx_legende))
      {
        if(max(as.data.frame(donnees)[,varFlux])<0)
        {
          map_leaflet$x$calls[[idx_fleche]]$args[[2]]$colEntree <- NULL
          map_leaflet$x$calls[[idx_fleche]]$args[[2]]$colSortie <- colSortie
        }else if(min(as.data.frame(donnees)[,varFlux])>=0)
        {
          map_leaflet$x$calls[[idx_fleche]]$args[[2]]$colEntree <- colEntree
          map_leaflet$x$calls[[idx_fleche]]$args[[2]]$colSortie <- NULL
        }else
        {
          map_leaflet$x$calls[[idx_fleche]]$args[[2]]$colEntree <- NULL
          map_leaflet$x$calls[[idx_fleche]]$args[[2]]$colSortie <- NULL
        }

        map_leaflet$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder

        titre <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$titre
        lng <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$lng
        lat <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$lat
        zoom <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$zoom

        map <- add_legende_saphirs(map, titre = titre, lng = lng, lat = lat, zoom = zoom, map_leaflet = map_leaflet)
      }
    }

    return(map)
  }
