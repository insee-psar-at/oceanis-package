coordonnees_etiquettes <-
function(fondMaille,listeCode)
{
  options("stringsAsFactors"=FALSE)
  
  # Verification des parametres
  
  msg_error1<-msg_error2 <- NULL
  
  if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error1 <- "Le fond de maille doit etre un objet sf / "
  if(any(class(listeCode)!="character")) msg_error2 <- "La liste de code doit etre un vecteur de type caractere / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2)))
  }
  
  names(fondMaille)[1] <- "CODE"
  names(fondMaille)[2] <- "LIBELLE"
  fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
  
  fondEtiquettes <- fondMaille[fondMaille$CODE %in% listeCode,c("CODE","LIBELLE")]
  if(nrow(fondEtiquettes)==0) stop(simpleError("Aucun CODE a etiqueter n'a ete trouve dans fondMaille."))
  suppressWarnings(centroid <- st_coordinates(st_centroid(st_geometry(fondEtiquettes))))
  tableEtiquettes <- as.data.frame(fondEtiquettes)[,c("CODE","LIBELLE")]
  tableEtiquettes <- cbind(tableEtiquettes,centroid)
  tableEtiquettes$TAILLE <- 0.9
  tableEtiquettes$FONT <- 2
  tableEtiquettes$COL <- "black"
  
  return(tableEtiquettes)
}
