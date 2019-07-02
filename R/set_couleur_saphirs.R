set_couleur_saphirs <-
function(map,colEntree="#CD853F",colSortie="#6495ED",colBorder="black")
{
  msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
  
  if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
  if(any(class(colEntree)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
  if(any(class(colSortie)!="character")) msg_error3 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
  if(any(class(colBorder)!="character")) msg_error4 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
  }
  
  idx_fleche <- NULL
  idx_legende <- NULL
  for(i in 1:length(map$x$calls))
  {
    if(map$x$calls[[i]]$method %in% "addPolygons")
    {
      if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux") idx_fleche <- i
      if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux_leg") idx_legende <- i
    }
  }
  
  val_ent <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_fleche]]$args[[5]],str_locate(map$x$calls[[idx_fleche]]$args[[5]],":")[[1]]+2,nchar(map$x$calls[[idx_fleche]]$args[[5]])-11)," ",""))>=0)
  
  if(length(val_ent)>0)
  {
    map$x$calls[[idx_fleche]]$args[[4]]$fillColor[1:length(val_ent)] <- colEntree
  }
  
  val_sor <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_fleche]]$args[[5]],str_locate(map$x$calls[[idx_fleche]]$args[[5]],":")[[1]]+2,nchar(map$x$calls[[idx_fleche]]$args[[5]])-11)," ",""))<0)
  
  if(length(val_sor)>0)
  {
    map$x$calls[[idx_fleche]]$args[[4]]$fillColor[length(val_ent)+1:length(val_sor)] <- colSortie
  }
  
  map$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder
  
  if(!is.null(idx_legende))
  {
    if(length(val_ent)>0)
    {
      map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colEntree
    }else
    {
      map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colSortie
    }
    map$x$calls[[idx_legende]]$args[[4]]$color <- colBorder
  }
  
  return(map)
}
