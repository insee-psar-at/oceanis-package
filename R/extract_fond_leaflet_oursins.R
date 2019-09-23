extract_fond_leaflet_oursins <-
function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_fleche <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_oursins_init","carte_oursins")) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addPolylines")
      {
        if(map$x$calls[[i]]$args[[3]] == "carte_oursins") idx_fleche <- i
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
    }
    
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
      
      list_fonds[[l]] <- fond
      rm(fond)
      
      nom_fonds <- c(nom_fonds,map$x$calls[[idx_fleche]]$args[[2]]$nom_fond)
      
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
    
    epaisseur <- map$x$calls[[idx_fleche]]$args[4][[1]]$weight
    colTrait <- map$x$calls[[idx_fleche]]$args[4][[1]]$color
    
    return(list(list_fonds,nom_fonds,titre,source,epaisseur,colTrait,dom))
  }
