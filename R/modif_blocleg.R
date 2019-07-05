modif_blocleg <-
function(nomcouche,idcouche)
  {
    blocleg<-balises_qgis()[[3]]
    
    modifs=data.frame(c("NOMDELACOUCHE","ID_COUCHE"),stringsAsFactors=F)
    modifs[1,2]=nomcouche
    modifs[2,2]=idcouche
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocleg,modifs)
    return(qgs)
  }
