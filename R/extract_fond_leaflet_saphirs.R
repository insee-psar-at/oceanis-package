extract_fond_leaflet_saphirs <-
function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_fleche <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]] %in% c("carte_saphirs_init","carte_saphirs"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]] %in% c("legende_saphirs"))) idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(any(map$x$calls[[i]]$args[5][[1]] %in% c("legende_saphirs"))) idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      idx_fleche <- idx_carte[length(idx_carte)]
      idx_carte <- idx_carte[-length(idx_carte)]
      
      var_flux <- map$x$calls[[idx_fleche]]$args[[2]]$var_flux
      
      code_epsg <- map$x$calls[[idx_fleche]]$args[[2]]$code_epsg
      dom <- map$x$calls[[idx_fleche]]$args[[2]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      for(i in 1:length(idx_carte))
      {
        fond <- map$x$calls[[idx_carte[i]]]$args[[2]][1][[1]]
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        rm(fond)
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[2]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_fleche))
      {
        fond <- map$x$calls[[idx_fleche]]$args[[2]]$analyse_WGS84
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        fond_entree <- fond[data.frame(fond)[,var_flux]>=0,]
        fond_sortie <- fond[data.frame(fond)[,var_flux]<0,]
        
        if(nrow(fond_entree)>0)
        {
          list_fonds[[l]] <- fond_entree
          nom_fonds <- c(nom_fonds,paste0(map$x$calls[[idx_fleche]]$args[[2]]$nom_fond,"_entree"))
          l <- l+1
        }
        if(nrow(fond_sortie)>0)
        {
          list_fonds[[l]] <- fond_sortie
          nom_fonds <- c(nom_fonds,paste0(map$x$calls[[idx_fleche]]$args[[2]]$nom_fond,"_sortie"))
          l <- l+1
        }
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
        large <- map$x$calls[[idx_fleche[1]]]$args[[2]]$distance
        long <- large
        
        gf <- st_sf(geometry=st_sfc(st_polygon(list(as.matrix(map$x$calls[[idx_legende[1]]]$args[[1]][[2]][[1]][[1]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        x <- st_bbox(gf)$xmin
        y <- st_bbox(gf)$ymin
        flux_leg_pl <- flux_legende_saphirs_pl(x,y,long,large,code_epsg)
        
        max_var <- map$x$calls[[idx_fleche]]$args[[2]]$max_var
        flux_leg_pl <- cbind(VAR=c(max_var,max_var/3),flux_leg_pl)
        names(flux_leg_pl) <- c(var_flux,"geometry")
        
        list_fonds[[l]] <- flux_leg_pl
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende[[1]]]]$args[[2]]$nom_fond)
        
        l <- l+1
      }
      
      colFleche <- unique(map$x$calls[[idx_fleche]]$args[4][[1]]$fillColor)
      
      if(length(colFleche)>1)
      {
        colEntree <- colFleche[1]
        colSortie <- colFleche[2]
      }else
      {
        colEntree <- colFleche
        colSortie <- colFleche
      }
      colBorder <- map$x$calls[[idx_fleche]]$args[4][[1]]$color
      
      return(list(list_fonds,nom_fonds,titre,source,colEntree,colSortie,colBorder,dom))
    }
  }
