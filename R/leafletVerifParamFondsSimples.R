leafletVerifParamFondsSimples <-
function(listFonds,popup,add,map){
  msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
  
  if(any(class(listFonds)!="list")) msg_error1 <- "Le parametre listFonds doit etre une liste / "
  if(any(!any(class(listFonds[[1]]) %in% "sf"),!any(class(listFonds[[1]]) %in% "data.frame"))) msg_error2 <- "Le parametre de listFonds doit etre un objet sf / "
  if (!is.null(map)) if(!any(class(map) %in% "leaflet_proxy")) msg_error3 <- "La carte doit etre un objet leaflet_proxy / "
  if (!is.null(popup)) if(!any(class(popup) %in% "numeric")) msg_error4 <- "Le vecteur popup doit etre numeric / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
  }
}
