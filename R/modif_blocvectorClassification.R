modif_blocvectorClassification <-
function(text)
{
  blocvectorClassification<-balises_qgis()[[10]]
  
  modifs=data.frame(c("TEXT"),stringsAsFactors=F)
  modifs[1,2]=text
  modifs[,2]=as.character(modifs[,2])
  qgs=modification(blocvectorClassification,modifs)
  return(qgs)
}
