set_couleur_joignantes <-
function(map,colFleche="#FFC300",colBorder="black",map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL

    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(colFleche)) if(any(class(colFleche)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error3 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
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
      if(!is.null(idx_fleche))
      {
        map$x$calls[[idx_fleche]]$args[[4]]$fillColor <- colFleche
        map$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder
      }

      if(!is.null(idx_legende))
      {
        map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colFleche
        map$x$calls[[idx_legende]]$args[[4]]$color <- colBorder
      }

    }else # contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy

      clearGroup(map, group = "carte_joignantes")

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
                         options = pathOptions(pane = "fond_joignantes", clickable = T),
                         popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                         fill = T, fillColor = colFleche, fillOpacity = 1,
                         group = "carte_joignantes",
                         layerId = list(analyse_WGS84=analyse_WGS84,donnees=donnees,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_flux",var_flux=varFlux,max_var=max_var,largeur=largeurFlecheMax,distance=large_pl)
      )

      if(!is.null(idx_legende))
      {
        map_leaflet$x$calls[[idx_fleche]]$args[[4]]$fillColor <- colFleche
        map_leaflet$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder
        map_leaflet$x$calls[[idx_legende]]$args[[4]]$fillColor <- colFleche
        map_leaflet$x$calls[[idx_legende]]$args[[4]]$color <- colBorder

        titre <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$titre
        lng <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$lng
        lat <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$lat
        zoom <- map_leaflet$x$calls[[idx_legende]]$args[[2]]$zoom

        map <- add_legende_joignantes(map, titre = titre, lng = lng, lat = lat, zoom = zoom, map_leaflet = map_leaflet)
      }
    }

    return(map)
  }
