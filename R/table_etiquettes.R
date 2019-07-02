table_etiquettes <-
function(fondMaille,etiquettes)
{
  etiquettes <- data.frame(etiquettes, stringsAsFactors = FALSE)
  names(etiquettes)[1] <- "CODE"
  if(ncol(etiquettes)==1) # table avec "CODE"
  {
    tableEtiquettes <- coordonnees_etiquettes(fondMaille,etiquettes$CODE)
  }else if(ncol(etiquettes)==3) # table avec "CODE", "X", "Y"
  {
    message(simpleMessage(paste0("[INFO] La table d'\u00e9tiquettes ne contient que 3 colonnes, qui doivent \u00eatre : l'identifiant de la maille ('CODE') et les coordonn","\u00e9","es 'X' et 'Y' de l'\u00e9tiquette.")))
    names(etiquettes)[2] <- "X"
    names(etiquettes)[3] <- "Y"
    tableEtiquettes <- as.data.frame(fondMaille)[fondMaille$CODE %in% etiquettes$CODE,c("CODE","LIBELLE")]
    if(nrow(tableEtiquettes)==0) stop(simpleError("Aucun CODE a etiqueter n'a ete trouve dans fondMaille."))
    tableEtiquettes <- cbind(tableEtiquettes,etiquettes$X,etiquettes$Y)
    tableEtiquettes$TAILLE <- 0.9
    tableEtiquettes$FONT <- 2
    tableEtiquettes$COL <- "black"
  }else
  {
    message(simpleMessage(paste0("[INFO] La table d'\u00e9tiquettes fournie contient au moins 4 colonnes. Elles doivent \u00eatre : l'identifiant de la maille ('CODE'), le libell\u00e9 de l'\u00e9tiquette ('LIBELLE') et les coordonn","\u00e9","es 'X' et 'Y' de l'\u00e9tiquette. Peut etre ajout","\u00e9","es \u00e9ventuellement les colonnes 'TAILLE', 'FONT' et 'COL'.")))
    msg_error1<-msg_error2<-msg_error3 <- NULL
    if(!any(names(etiquettes) %in% "LIBELLE"))  msg_error1 <- "La table d'etiquettes doit contenir une colonne nommee LIBELLE / "
    if(!any(names(etiquettes) %in% "X"))  msg_error2 <- "La table d'etiquettes doit contenir une colonne nommee X / "
    if(!any(names(etiquettes) %in% "Y"))  msg_error3 <- "La table d'etiquettes doit contenir une colonne nommee Y / "
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    if(!any(names(etiquettes) %in% "TAILLE")) tableEtiquettes$TAILLE <- 0.9
    if(!any(names(etiquettes) %in% "FONT")) tableEtiquettes$FONT <- 2
    if(!any(names(etiquettes) %in% "COL")) tableEtiquettes$COL <- "black"
    
    tableEtiquettes <- etiquettes
  }

  return(tableEtiquettes)
}
