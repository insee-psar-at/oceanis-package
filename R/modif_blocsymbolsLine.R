modif_blocsymbolsLine <-
function(couleurbordure,stylebordure,epaisseurbordure,name)
{
  blocsymbolsLine<-balises_qgis()[[7]]
  
  modifs=data.frame(c("COULEURBORDURE","STYLEBORDURE","EPAISSEURBORDURE","NAME"),stringsAsFactors=F)
  modifs[1,2]=couleurbordure
  modifs[2,2]=stylebordure
  modifs[3,2]=epaisseurbordure
  modifs[4,2]=name
  modifs[,2]=as.character(modifs[,2])
  qgs=modification(blocsymbolsLine,modifs)
  return(qgs)
}
