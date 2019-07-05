modif_bloccategories <-
function(symbol,value,label)
  {
    bloccategories<-balises_qgis()[[8]]
    
    modifs=data.frame(c("SYMBOL","VALUE","LABEL"),stringsAsFactors=F)
    modifs[1,2]=symbol
    modifs[2,2]=value
    modifs[3,2]=label
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(bloccategories,modifs)
    return(qgs)
  }
