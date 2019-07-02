coord_legende <-
function(map)
{
  msg_error1 <- NULL
  
  if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
  
  if(any(!is.null(msg_error1)))
  {
    stop(simpleError(msg_error1))
  }
  
  ronds <- FALSE
  lng_ronds <- NULL
  lat_ronds <- NULL
  if(any(map$x$calls[[4]]$args[[3]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds")))
  {
    ronds <- TRUE
  }
  
  if(ronds) # La map comporte une une analyse en ronds
  {
    j <- NULL
    for(i in 1:length(map$x$calls)) # On compte le nombre d'objets circle dans le leaflet
    {
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        j <- c(j,i)
      }
    }
    
    if(length(j)==1) # La legende n'a pas encore ete creee, on recupere une position par defaut de la legende
    {
      lng_classes <- map$x$fitBounds[[4]]
      lat_classes <- map$x$fitBounds[[3]]
    }else # On recupere la derniere position connue de la legende
    {
      lng_ronds <- map$x$calls[[j[length(j)]]]$args[[2]][[1]]
      lat_ronds <- map$x$calls[[j[length(j)]]]$args[[1]][[1]]
    }
  }
  
  classes <- FALSE
  lng_classes <- NULL
  lat_classes <- NULL
  if(any(map$x$calls[[4]]$args[[3]]$nom_couche %in% c("carte_classes","carte_ronds_classes","carte_classes_ronds","carte_typo")))
  {
    classes <- TRUE
  }
  
  if(classes) # La map comporte une une analyse en classes
  {
    j <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        j <- c(j,i)
      }
    }
    
    if(length(j)<5) # La legende n'a pas encore ete creee, on recupere une position par defaut de la legende
    {
      lng_classes <- map$x$fitBounds[[4]]
      lat_classes <- map$x$fitBounds[[1]]
    }else # On recupere la derniere position connue de la legende
    {
      lng_classes <- max(map$x$calls[[j[length(j)]]]$args[[1]][[1]][[1]][[1]]$lng)
      lat_classes <- max(map$x$calls[[j[length(j)]]]$args[[1]][[1]][[1]][[1]]$lat)
    }
  }
  
  fleches <- FALSE
  lng_fleches <- NULL
  lat_fleches <- NULL
  if(any(map$x$calls[[4]]$args[[3]]$nom_couche %in% c("carte_joignantes","carte_saphirs")))
  {
    fleches <- TRUE
  }
  
  if(fleches) # La map comporte une une analyse en ronds
  {
    j <- NULL
    for(i in 1:length(map$x$calls)) # On compte le nombre d'objets circle dans le leaflet
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        j <- c(j,i)
      }
    }
    
    if(length(j)<5) # La legende n'a pas encore ete creee, on recupere une position par defaut de la legende
    {
      lng_fleches <- map$x$fitBounds[[4]]
      lat_fleches <- map$x$fitBounds[[3]]
    }else # On recupere la derniere position connue de la legende
    {
      lng_fleches <- min(map$x$calls[[j[length(j)]]]$args[[1]][[2]][[1]][[1]]$lng)
      lat_fleches <- min(map$x$calls[[j[length(j)]]]$args[[1]][[2]][[1]][[1]]$lat)
    }
  }
  
  coord_leg <- data.frame(X=c(lng_ronds,lng_classes,lng_fleches),Y=c(lat_ronds,lat_classes,lat_fleches))
  if(ronds & classes)
  {
    rownames(coord_leg) <- c("coord_leg_ronds","coord_leg_classes")
  }else if(ronds)
  {
    rownames(coord_leg) <- c("coord_leg_ronds")
  }else if(classes)
  {
    rownames(coord_leg) <- c("coord_leg_classes")
  }
  if(fleches) rownames(coord_leg) <- c("coord_leg_fleches")
  
  return(coord_leg)
}
