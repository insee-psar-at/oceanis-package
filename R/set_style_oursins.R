set_style_oursins <-
function(map,epaisseur=2,colTrait="black",map_leaflet=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(epaisseur)!="numeric")) msg_error2 <- "L'epaisseur du trait doit etre de type numerique / "
    if(any(class(colTrait)!="character")) msg_error3 <- "La couleur du trait doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(map_leaflet)) if (any(!any(class(map_leaflet) %in% "leaflet"), !any(class(map_leaflet) %in% "htmlwidget"))) msg_error4 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(!is.null(map_leaflet))
    {
      map_proxy <- map
      map <- map_leaflet
    }
    
    idx_fleche <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolylines")
      {
        idx_fleche <- i
      }
    }
    
    if(is.null(map_leaflet))
    {
      map$x$calls[[idx_fleche]]$args[4][[1]]$weight <- epaisseur
      map$x$calls[[idx_fleche]]$args[4][[1]]$color <- colTrait
      
    }else # Contexte shiny/proxy
    {
      map_leaflet <- map
      map <- map_proxy
      
      clearGroup(map, group = "carte_oursins")
      
      analyse_WGS84 <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$analyse_WGS84
      donnees <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$donnees
      code_epsg <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$code_epsg
      dom <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$dom
      varFlux <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$var_flux
      
      map <- addPolylines(map = map,
                          data = analyse_WGS84,
                          stroke = TRUE, color = colTrait,
                          opacity = 1,
                          weight = epaisseur,
                          options = pathOptions(pane = "fond_oursins", clickable = T),
                          popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                          group = "carte_oursins",
                          layerId = list(analyse_WGS84=analyse_WGS84,donnees=donnees,code_epsg=code_epsg,dom=dom,nom_fond="fond_flux",var_flux=varFlux)
      )
    }
    
    return(map)
  }
