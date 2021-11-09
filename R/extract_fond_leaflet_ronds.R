extract_fond_leaflet_ronds <-
function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_carte_ronds <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende_ronds <- NULL

    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_ronds_init","carte_ronds","carte_ronds_elargi")) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds","carte_ronds_elargi")) idx_carte_ronds <- c(idx_carte_ronds,i)
      }

      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
      }

      if(!is.null(idx_legende_ronds)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }

        if(map$x$calls[[i]]$method %in% "addPolylines")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
      }
    }

    if(is.null(idx_legende_ronds))
    {
      return(NULL)
    }else
    {
      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$code_epsg

      list_fonds <- list()
      nom_fonds <- c()
      l <- 1

      for(i in 1:length(idx_carte))
      {
        fond <- map$x$calls[[idx_carte[i]]]$args[[2]][1][[1]]

        fond <- st_transform(fond,crs=as.numeric(code_epsg))

        list_fonds[[l]] <- fond

        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[2]]$nom_fond)

        l <- l+1
      }

      for(i in 1:length(idx_carte_ronds))
      {
        emprise <- map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$emprise

        centres_ronds <- data.frame(lng=map$x$calls[[idx_carte_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_carte_ronds[i]]]$args[[1]])
        aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=4326)))
        bb <- do.call("rbind",aa)
        cc <- st_transform(bb,crs=as.numeric(map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$code_epsg))
        ronds_pl <- st_buffer(cc, map$x$calls[[idx_carte_ronds[i]]]$args[[3]])

        col_bor <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$color
        ronds_pl <- cbind(COL_BOR=col_bor,ronds_pl)
        col <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$fillColor
        ronds_pl <- cbind(COL=col,ronds_pl)
        varVolume <- map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$var_volume
        val <- map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$analyse$donnees[,varVolume]
        ronds_pl <- cbind(ETI_VAL=val,ronds_pl)

        fond_pos <- NULL
        fond_neg <- NULL

        if(nrow(ronds_pl[ronds_pl$ETI_VAL>0,])>0)
        {
          fond_pos <- ronds_pl[ronds_pl$ETI_VAL>0,]
        }
        if(nrow(ronds_pl[ronds_pl$ETI_VAL<0,])>0)
        {
          fond_neg <- ronds_pl[ronds_pl$ETI_VAL<0,]
        }

        if(!is.null(fond_pos))
        {
          list_fonds[[l]] <- fond_pos
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$nom_fond[1])
          l <- l+1
        }
        if(!is.null(fond_neg))
        {
          list_fonds[[l]] <- fond_neg
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$nom_fond[2])
          l <- l+1
        }
      }

      for(i in 1:length(idx_legende_ronds))
      {
        if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addCircles")
        {
          centres_ronds <- data.frame(lng=map$x$calls[[idx_legende_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_legende_ronds[i]]]$args[[1]])
          aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=4326)))
          bb <- do.call("rbind",aa)
          cc <- st_transform(bb,crs=as.numeric(map$x$calls[[idx_legende_ronds[i]]]$args[[4]]$code_epsg))
          ronds_pl <- st_buffer(cc, map$x$calls[[idx_legende_ronds[i]]]$args[[3]])

          ronds_pl_leg <- ronds_pl
          val <- c(map$x$calls[[idx_legende_ronds[i]]]$args[[7]][1],map$x$calls[[idx_legende_ronds[i]]]$args[[7]][2])
          ronds_pl_leg <- cbind(ETI_VAL=val,ronds_pl_leg)

          list_fonds[[l]] <- ronds_pl_leg
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[4]]$nom_fond)
          l <- l+1
        }
      }
    }

    return(list(list_fonds,nom_fonds,emprise))
  }
