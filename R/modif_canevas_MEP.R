modif_canevas_MEP <-
function(nommep,tit,sstit,an,sourc,xmin,xmax,ymin,ymax)
{
  canevas_MEP<-balises_qgis()[[2]]
  
  modifs=data.frame(c("NOMMEP","TITRE","SSTIT","AN","SOURCE","XXXMIN","XXXMAX","YYYMIN","YYYMAX"),stringsAsFactors=F)
  modifs[1,2]=nommep
  modifs[2,2]=tit
  modifs[3,2]=sstit
  modifs[4,2]=an
  modifs[5,2]=sourc
  modifs[6,2]=xmin
  modifs[7,2]=xmax
  modifs[8,2]=ymin
  modifs[9,2]=ymax
  modifs[,2]=as.character(modifs[,2])
  qgs=modification(canevas_MEP,modifs)
  return(qgs)
}
