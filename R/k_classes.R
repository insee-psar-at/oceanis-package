k_classes <-
function(fond_carto_k,fond_carto_elargi_k,variable_jointure_fond_carto_k,donnees_k,variable_jointure_donnees_k,variable_a_representer_k,elargi)
  {
    donnees_k[,variable_a_representer_k] <- as.numeric(donnees_k[,variable_a_representer_k])
    suppressWarnings(donnees_k<-donnees_k[!is.na(donnees_k[,variable_a_representer_k]),])
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
    
    base <- merge(as.data.frame(fond_carto_k)[,-length(names(fond_carto_k))],donnees_k,by.x=variable_jointure_fond_carto_k,by.y=variable_jointure_donnees_k)
    if (length(names(base)[grep("..y",names(base))])>0)
    {
      base <- base[,-grep("..y",names(base))]
      names(base)[grep("..x",names(base))]<-sub(".x","",names(base)[grep("..x",names(base))])
    }
    
    base <- base[abs(base[,variable_a_representer_k])>=0,]
    base <- base[order(base[,variable_a_representer_k],decreasing = T),]
    if(nrow(base)>0)
    {
      rownames(base) <- as.character(1:dim(base)[1])
    }else
    {
      return(NULL)
    }
    base$id <- c(1:nrow(base))
    base <- base[order(base[,variable_jointure_fond_carto_k]),]
    fond_carto_k <- merge(base[,c(variable_jointure_fond_carto_k,"id")],fond_carto_k,by=variable_jointure_fond_carto_k)
    fond_carto_k <- fond_carto_k[order(fond_carto_k$id),]
    fond_carto_k <- fond_carto_k[,-2]
    base <- base[order(base$id),]
    base <- base[,-(ncol(base))]
    
    if(elargi)
    {
      fond_carto_elargi_k <- fond_carto_elargi_k[as.data.frame(fond_carto_elargi_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
      
      base_elargi <- merge(as.data.frame(fond_carto_elargi_k)[,-length(names(fond_carto_elargi_k))],donnees_k,by.x=variable_jointure_fond_carto_k,by.y=variable_jointure_donnees_k)
      if (length(names(base_elargi)[grep("..y",names(base_elargi))])>0)
      {
        base_elargi <- base_elargi[,-grep("..y",names(base_elargi))]
        names(base_elargi)[grep("..x",names(base_elargi))]<-sub(".x","",names(base_elargi)[grep("..x",names(base_elargi))])
      }
      
      base_elargi <- base_elargi[abs(base_elargi[,variable_a_representer_k])>=0,]
      base_elargi <- base_elargi[order(base_elargi[,variable_a_representer_k],decreasing = T),]
      rownames(base_elargi) <- as.character(1:dim(base_elargi)[1])
      
      base_elargi$id <- c(1:nrow(base_elargi))
      base_elargi <- base_elargi[order(base_elargi[,variable_jointure_fond_carto_k]),]
      fond_carto_elargi_k <- merge(base_elargi[,c(variable_jointure_fond_carto_k,"id")],fond_carto_elargi_k,by=variable_jointure_fond_carto_k)
      fond_carto_elargi_k <- fond_carto_elargi_k[order(fond_carto_elargi_k$id),]
      fond_carto_elargi_k <- fond_carto_elargi_k[,-2]
      base_elargi <- base_elargi[order(base_elargi$id),]
      base_elargi <- base_elargi[,-(ncol(base_elargi))]
      
      return(list(donnees=base,donnees_elargi=base_elargi))
    }else
    {
      return(list(donnees=base))
    }
  }
