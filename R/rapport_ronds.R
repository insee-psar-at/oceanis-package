rapport_ronds <-
function(map)
{
  msg_error1 <- NULL
  
  if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
  
  if(any(!is.null(msg_error1)))
  {
    stop(simpleError(msg_error1))
  }
  
  idx_legende <- NULL
  j <- NULL
  for(i in 1:length(map$x$calls))
  {
    if(map$x$calls[[i]]$method %in% "addCircles")
    {
      j <- c(j,i)
      if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
    }
  }
  
  if(!is.null(j))
  {
    if(!is.null(idx_legende))
    {
      if(length(j)>2) idx <- 2 else idx <- 1
    }else
    {
      if(length(j)>1) idx <- 2 else idx <- 1
    }
    rayonRond <- max(map$x$calls[[j[idx]]]$args[[3]])
    max_var <- map$x$calls[[j[idx]]]$args[[5]]$max_var
    rapport <- (pi*(rayonRond)^2)/max_var
  }else
  {
    stop(simpleError("Il n'y a pas d'analyse en ronds dans la map"))
  }
  
  return(rapport)
}
