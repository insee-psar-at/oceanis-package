set_couleur_typo <-
function(map,paletteTypo=NULL,colBorder="white",map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL

    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(paletteTypo)) if(any(class(paletteTypo)!="character")) msg_error2 <- "La palette de la typologie doit etre un vecteur de type caractere / "
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

    if(is.null(paletteTypo))
    {
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="carte_typo") idx_carte <- c(idx_carte,i)
        }
      }
      nb_col <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
      paletteTypo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
    }

    idx_carte <- NULL
    idx_legende <- NULL
    legende <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="carte_typo")
        {
          if(map$x$calls[[i]]$args[[2]]$nom_fond=="fond_maille_typo_carte") idx_carte <- i
        }
      }

      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]=="legende_typo") legende <- T
      }
      if(legende) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }

    if(is.null(map_leaflet)) # Contexte leaflet
    {
      if(!is.null(idx_carte))
      {
        couleur_analyse <- data.frame(col=map$x$calls[[idx_carte]]$args[[4]]$fillColor)
        couleur_analyse$id1 <- c(1:nrow(couleur_analyse))
        pal_anc <- data.frame(col=unique(couleur_analyse$col))
        pal_anc$id2 <- c(1:nrow(pal_anc))
        couleur_analyse <- merge(couleur_analyse,pal_anc,by="col")

        aa <- sapply(1:(length(paletteTypo)), function(x) couleur_analyse[couleur_analyse$id2==x,"col"] <<- paletteTypo[x])
        rm(aa)
        couleur_analyse <- couleur_analyse[order(couleur_analyse$id1),]
        couleur_analyse <- couleur_analyse$col

        map$x$calls[[idx_carte]]$args[[4]]$fillColor <- couleur_analyse

        map$x$calls[[idx_carte]]$args[[4]]$color <- colBorder
      }

      if(legende)
      {
        for(i in 1:length(idx_legende))
        {
          map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor <- paletteTypo[i]
        }
      }
    }else # Contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy

      clearGroup(map, group = "carte_typo")

      analyse_WGS84 <- map_leaflet$x$calls[[idx_carte]]$args[[2]]$analyse_WGS84
      analyse <- map_leaflet$x$calls[[idx_carte]]$args[[2]]$analyse
      code_epsg <- map_leaflet$x$calls[[idx_carte]]$args[[2]]$code_epsg
      emprise <- map_leaflet$x$calls[[idx_carte]]$args[[2]]$emprise
      varTypo <- map_leaflet$x$calls[[idx_carte]]$args[[2]]$var_typo

      map <- addPolygons(map = map, data = analyse_WGS84, opacity = 1,
                         stroke = TRUE, color = colBorder, weight = 1,
                         options = pathOptions(pane = "fond_typo", clickable = T),
                         popup = paste0("<b> <font color=#2B3E50>",as.data.frame(analyse_WGS84)[,"LIBELLE"], "</font> </b><br><b><font color=#2B3E50>",varTypo," : </font></b>",analyse_WGS84$TXT1,"<br>"),
                         fill = T,
                         fillColor = analyse$col,
                         fillOpacity = 1,
                         group = "carte_typo",
                         layerId = list(analyse_WGS84=analyse_WGS84,analyse=analyse,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_maille_typo_carte",var_typo=varTypo)
      )
    }
    return(map)
  }
