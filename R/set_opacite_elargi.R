set_opacite_elargi <-
function(map,opacite=0.6)
{
  msg_error1<-msg_error2<-msg_error3 <- NULL
  
  if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
  if(any(class(opacite)!="numeric")) msg_error2 <- "L'opacite doit etre de type numerique (entre 0 et 1) / "
  if(opacite<0 | opacite>1) msg_error3 <- "L'opacite doit etre compris entre 0 (transparent) et 1 (opaque) / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
  }
  
  idx_carte_classes_elargi <- NULL
  idx_carte_ronds_elargi <- NULL
  for(i in 1:length(map$x$calls))
  {
    if(map$x$calls[[i]]$method %in% "addPolygons") # maille et/ou analyse en classes
    {
      if(any(map$x$calls[[i]]$args[[3]]$nom_couche %in% c("carte_ronds","carte_classes","carte_ronds_classes","carte_classes_ronds")))
      {
        if(map$x$calls[[i]]$args[[3]]$nom_fond %in% c("fond_maille_elargi","fond_maille_elargi_carte")) idx_carte_classes_elargi <- c(idx_carte_classes_elargi,i)
      }
    }
    if(map$x$calls[[i]]$method %in% "addCircles") # ronds
    {
      if(any(map$x$calls[[i]]$args[[5]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds")))
      {
        if(any(map$x$calls[[i]]$args[[5]]$nom_fond %in% c("fond_ronds_pos_elargi_carte","fond_ronds_neg_elargi_carte","fond_ronds_elargi_carte"))) idx_carte_ronds_elargi <- c(idx_carte_ronds_elargi,i)
      }
    }
  }
  
  if(!is.null(idx_carte_classes_elargi))
  {
    for(i in 1:length(idx_carte_classes_elargi))
    {
      map$x$calls[[idx_carte_classes_elargi]]$args[[4]]$opacity <- opacite
      map$x$calls[[idx_carte_classes_elargi]]$args[[4]]$fillOpacity <- opacite
    }
  }
  if(!is.null(idx_carte_ronds_elargi))
  {
    for(i in 1:length(idx_carte_ronds_elargi))
    {
      map$x$calls[[idx_carte_ronds_elargi]]$args[[6]]$opacity <- opacite
      map$x$calls[[idx_carte_ronds_elargi]]$args[[6]]$fillOpacity <- opacite
    }
  }
  
  return(map)
}
