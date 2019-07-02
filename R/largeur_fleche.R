largeur_fleche <-
function(map)
{
  msg_error1 <- NULL
  
  if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
  
  if(any(!is.null(msg_error1)))
  {
    stop(simpleError(msg_error1))
  }
  
  idx_fleche <- NULL
  for(i in 1:length(map$x$calls))
  {
    if(map$x$calls[[i]]$method %in% "addPolygons")
    {
      if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux") idx_fleche <- i
    }
  }
  
  if(!is.null(idx_fleche))
  {
    largeur <- map$x$calls[[idx_fleche]]$args[[3]]$largeur
  }else
  {
    stop(simpleError("Il n'y a pas d'analyse en fleches joignantes ou saphirs dans la map"))
  }
  
  return(largeur)
}
