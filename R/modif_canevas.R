modif_canevas <-
function(xmin,xmax,ymin,ymax,projproj,nbfonds)
  {
    canevas<-balises_qgis()[[1]]
    
    modifs=data.frame(c("XXXMIN","XXXMAX","YYYMIN","YYYMAX","PROJECTIONPROJET","NOMBREDEFONDS"),stringsAsFactors=F)
    modifs[1,2]=xmin
    modifs[2,2]=xmax
    modifs[3,2]=ymin
    modifs[4,2]=ymax
    modifs[5,2]=projproj
    modifs[6,2]=nbfonds
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(canevas,modifs)
    return(qgs)
  }
