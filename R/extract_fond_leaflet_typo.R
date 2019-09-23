extract_fond_leaflet_typo <-
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
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_typo_init","carte_typo")) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]=="legende_typo") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_typo") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      var_typo <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$var_typo
      
      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$code_epsg
      dom <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        fond <- map$x$calls[[idx_carte[i]]]$args[[2]][1][[1]]
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
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
              palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[4][[1]]$fillColor)
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }
      
      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_typo,dom))
    }
  }
