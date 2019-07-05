modif_blocsymbolsPolygon <-
function(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
  {
    blocsymbolsPolygon<-balises_qgis()[[5]]
    
    modifs=data.frame(c("COULEURFOND","COULEURBORDURE","REMPLISSAGEFOND","STYLEBORDURE","EPAISSEURBORDURE","NAME"),stringsAsFactors=F)
    modifs[1,2]=couleurfond
    modifs[2,2]=couleurbordure
    modifs[3,2]=remplissagefond
    modifs[4,2]=stylebordure
    modifs[5,2]=epaisseurbordure
    modifs[6,2]=name
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocsymbolsPolygon,modifs)
    return(qgs)
  }
