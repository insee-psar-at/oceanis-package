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
      if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_classes") idx_carte <- c(idx_carte,i)
    }
    if(map$x$calls[[i]]$method %in% "addControl")
    {
      if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
      if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
    }

    if(map$x$calls[[i]]$method %in% "addRectangles")
    {
      if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
    }
    if(!is.null(idx_legende)) # la legende existe
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
      }
      if(map$x$calls[[i]]$method %in% "addMarkers")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
      }
    }
  }
  
  if(is.null(idx_legende))
  {
    return(NULL)
  }else
  {
    var_classes <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$var_ratio
    
    code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
    dom <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$dom
    
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
      
      if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
      {
        cc <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],25,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][3]-1))
        fond <- cbind(LIBELLE=cc,bb)
        dd <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],">")[[1]][11]+1,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][12]-1))
        dd <- as.numeric(str_replace_all(str_replace_all(dd,",",".")," ",""))
        fond <- cbind(val=dd,fond)
        ee <- map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
        fond <- cbind(classe=ee,fond)
        fond$var <- fond$val
        fond <- fond[,c("LIBELLE","var","val","classe","geometry")]
        names(fond) <- c("LIBELLE",var_classes,"val","classe","geometry")
        
        ff <- lapply(1:length(unique(fond$classe)), function(x) fond[fond$classe %in% rev(unique(fond$classe))[x],"classe"] <<- x)
        rm(cc,dd,ee,ff)
      }else
      {
        fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
      }
      rm(aa,bb)
      
      fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
      
      list_fonds[[l]] <- fond
      
      nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
      
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
    
    return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_classes,dom))
  }
}
