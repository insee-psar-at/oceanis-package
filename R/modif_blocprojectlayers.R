modif_blocprojectlayers <-
function(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
  {
    blocprojectlayers<-balises_qgis()[[4]]
    
    modifs=data.frame(c("GEOMETRIE","IDCOUCHE","CHEMINCOUCHE","NOMDELACOUCHE","PROJECTIONDELACOUCHE","ATTR","TYPEANALYSE"),stringsAsFactors=F)
    modifs[1,2]=geometrie
    modifs[2,2]=idcouche
    modifs[3,2]=chemincouche
    modifs[4,2]=nomcouche
    modifs[5,2]=projcouche
    modifs[6,2]=attr
    modifs[7,2]=typeanalyse
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocprojectlayers,modifs)
    return(qgs)
  }
