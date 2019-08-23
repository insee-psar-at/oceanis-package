extract_fond_leaflet_joignantes <-
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
        if(any(map$x$calls[[i]]$args[3][[1]] %in% c("carte_joignantes_init","carte_joignantes"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]] %in% c("legende_joignantes"))) idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(any(map$x$calls[[i]]$args[5][[1]] %in% c("legende_joignantes"))) idx_legende <- c(idx_legende,i)
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
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[2]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_fleche))
      {
        aa <- lapply(1:length(map$x$calls[[idx_fleche]]$args[[1]]), function(x) st_polygon(list(as.matrix(map$x$calls[[idx_fleche]]$args[[1]][[x]][[1]][[1]]))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(i in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(list(aa[[i]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_fleche]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],":")[[1]][1]+2,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][4]-1))
          cc <- as.numeric(str_replace_all(str_replace_all(cc,",",".")," ",""))
          fond <- cbind(var=cc,bb)
          
          dd <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]+5,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(CODE2=dd,fond)
          ee <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],">")[[1]][2]+1,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]-1))
          fond <- cbind(CODE1=ee,fond)
          
          names(fond) <- c("CODE1","CODE2",var_flux,"geometry")
          rm(aa,bb,cc,dd,ee)
        }
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
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
      
      if(!is.null(idx_legende))
      {
        large <- map$x$calls[[idx_fleche[1]]]$args[[2]]$distance
        long <- large*2
        
        gf <- st_sf(geometry=st_sfc(st_polygon(list(as.matrix(map$x$calls[[idx_legende[1]]]$args[[1]][[1]][[1]][[1]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        x <- st_bbox(gf)$xmin
        y <- st_bbox(gf)$ymin
        flux_leg_pl <- flux_legende_joignantes_pl(x,y,long,large,code_epsg)
        
        max_var <- map$x$calls[[idx_fleche]]$args[[2]]$max_var
        flux_leg_pl <- cbind(VAR=c(max_var,max_var/3),flux_leg_pl)
        names(flux_leg_pl) <- c(var_flux,"geometry")
        
        list_fonds[[l]] <- flux_leg_pl
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende[[1]]]]$args[[2]]$nom_fond)
        
        l <- l+1
      }
      
      colFleche <- map$x$calls[[idx_fleche]]$args[4][[1]]$fillColor
      colBorder <- map$x$calls[[idx_fleche]]$args[4][[1]]$color
      
      return(list(list_fonds,nom_fonds,titre,source,colFleche,colBorder,dom))
    }
  }
