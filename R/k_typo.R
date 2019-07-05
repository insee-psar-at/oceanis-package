k_typo <-
function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,variable_jointure_donnees_k,variable_a_representer_k)
  {
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
    
    if(is.numeric(donnees_k[,variable_a_representer_k])==T)
    {
      donnees_k[is.na(donnees_k)] <- 9999
      donnees_k <- donnees_k[match(as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],donnees_k[,variable_jointure_donnees_k]),]
      
      fond_carto_k$valeur <- donnees_k[,variable_a_representer_k]
      fond_carto_k[,variable_a_representer_k] <- as.numeric(donnees_k[,variable_a_representer_k])
      fond_carto_k$classe <- fond_carto_k$valeur
      fond_carto_k <- fond_carto_k[order(as.data.frame(fond_carto_k)[,variable_a_representer_k]),]
      
      return(list(analyse=fond_carto_k))
    }else
    {
      donnees_k[is.na(donnees_k)] <- "Non classe"
      
      donnees_k <- donnees_k[match(as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],donnees_k[,variable_jointure_donnees_k]),]
      valeurs <- cbind(var=as.character(sort(unique(donnees_k[,variable_a_representer_k]))), classe=seq(1,length(unique(donnees_k[,variable_a_representer_k])),1))
      donnees_k <- merge(donnees_k,valeurs,by.x=variable_a_representer_k,by.y="var")
      donnees_k <- donnees_k[order(donnees_k[,variable_a_representer_k]),]
      
      fond_carto_k_data <- unique(merge(donnees_k[,c(variable_jointure_donnees_k,variable_a_representer_k,"classe")],fond_carto_k,by.x=variable_jointure_donnees_k,by.y=variable_jointure_fond_carto_k))
      nb_y <- length(names(fond_carto_k_data)[grep("..y",names(fond_carto_k_data)[-grep("geom",names(fond_carto_k_data))])])
      if(nb_y>0)
      {
        idx_y <- grep("..y",names(fond_carto_k_data))
        idx_geom <- grep("geom",names(fond_carto_k_data))
        idx_y <- idx_y[-which(idx_y==idx_geom)]
        
        fond_carto_k_data <- fond_carto_k_data[,-idx_y[length(idx_y)]]
        names(fond_carto_k_data)[grep("..x",names(fond_carto_k_data))]<-sub(".x","",names(fond_carto_k_data)[grep("..x",names(fond_carto_k_data))])
      }
      fond_carto_k_data$valeur <- fond_carto_k_data[,variable_a_representer_k]
      fond_carto_k <- st_sf(fond_carto_k_data, crs=st_crs(fond_carto_k))
      fond_carto_k <- fond_carto_k[order(fond_carto_k$valeur),]
      
      return(list(analyse=fond_carto_k))
    }
  }
