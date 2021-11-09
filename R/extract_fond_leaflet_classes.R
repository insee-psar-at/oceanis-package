extract_fond_leaflet_classes <-
function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL

    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes_init","carte_classes","carte_classes_elargi")) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }

      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_classes_rectangle") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
    }

    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      var_classes <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$var_ratio

      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$code_epsg
      emprise <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$emprise

      list_fonds <- list()
      nom_fonds <- c()
      l <- 1

      for(i in 1:length(idx_carte))
      {
        fond <- map$x$calls[[idx_carte[i]]]$args[[2]][1][[1]]
        nom_col <- names(fond)[-which(names(fond)=="geometry")]
        aa <- map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
        fond <- cbind(fond,classe=aa)
        fond <- fond[,c(nom_col,"classe","geometry")]

        bb <- lapply(1:length(unique(fond$classe)), function(x) fond[fond$classe %in% rev(unique(fond$classe))[x],"classe"] <<- x)
        rm(aa,bb)

        fond <- st_transform(fond,crs=as.numeric(code_epsg))

        list_fonds[[l]] <- fond

        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[2]]$nom_fond)

        l <- l+1
      }

      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }

      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }

      if(!is.null(idx_legende))
      {
        label <- NULL
        palette <- NULL
        for(i in 1:length(idx_legende))
        {
          if(i==length(idx_legende))
          {
            titre_leg <- map$x$calls[[idx_legende[i]]]$args[11]
          }else
          {
            if(map$x$calls[[idx_legende[i]]]$method %in% "addMarkers")
            {
              label <- c(label,map$x$calls[[idx_legende[i]]]$args[11][[1]])
            }
            if(map$x$calls[[idx_legende[i]]]$method %in% "addPolygons")
            {
              if(map$x$calls[[idx_legende[i]]]$args[[3]] != "legende_classes_rectangle")
              {
                palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[4][[1]]$fillColor)
              }
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }

      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_classes,emprise))
    }
  }
