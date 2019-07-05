modif_bloclayeritem <-
function(nomcouche,idcouche,styl)
  {
    bloclayerItem<-balises_qgis()[[9]]
    
    modifs=data.frame(c("NOMDELACOUCHE","ID_COUCHE","STYLELEG","NOMDELACOUCHE"),stringsAsFactors=F)
    modifs[1,2]=nomcouche
    modifs[2,2]=idcouche
    modifs[3,2]=styl
    modifs[4,2]=nomcouche
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(bloclayerItem,modifs)
    return(qgs)
  }
