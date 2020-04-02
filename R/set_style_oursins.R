#' @title Modify the style of 'leaflet' map's urchins
#'
#' @description Modify the style of 'leaflet' map's urchins.
#'
#' @usage set_style_oursins(map, epaisseur = 2, colTrait = "black", map_leaflet
#' = NULL)
#'
#' @param map objet leaflet.
#' @param epaisseur valeur numerique (numeric). Par defaut a 2.
#' @param colTrait chaine de caracteres (character). Couleur nommee (par
#' exemple "black") ou hexadecimal (par exemple "#000000"). Par defaut "black".
#' @param map_leaflet objet leaflet. Pour l'integration des fonctions leaflet
#' dans les applications shiny (cf vignette). Par defaut a NULL.
#'
#' @return Retourne un objet leaflet.
#'
#' @seealso \code{\link{leaflet_fonds_simples}}
#'
#' \code{\link{leaflet_ronds} \link{leaflet_classes}
#' \link{leaflet_ronds_classes} \link{leaflet_classes_ronds}}
#'
#' \code{\link{leaflet_typo} \link{leaflet_oursins} \link{leaflet_joignantes}
#' \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_biloc")
#' data("regm")
#'
#' # Oursins
#' map <- leaflet_oursins(data = donnees_biloc, fondMaille = regm, idDataDepart = "REG_DEPART",
#' idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", filtreDist = 1000,
#' filtreMajeurs = 3)
#' map <- set_style_oursins(map = map, epaisseur = 3, colTrait = "grey")
#' \donttest{
#'  map
#' }
#'
#' @import leaflet
#'
#' @export set_style_oursins
#'
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
      emprise <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$emprise
      varFlux <- map_leaflet$x$calls[[idx_fleche]]$args[[2]]$var_flux

      map <- addPolylines(map = map,
                          data = analyse_WGS84,
                          stroke = TRUE, color = colTrait,
                          opacity = 1,
                          weight = epaisseur,
                          options = pathOptions(pane = "fond_oursins", clickable = T),
                          popup = paste0("<b><font color=#2B3E50>",donnees$CODE1," vers ",donnees$CODE2,"<br>",varFlux," : ",donnees[,varFlux],"</font></b>"),
                          group = "carte_oursins",
                          layerId = list(analyse_WGS84=analyse_WGS84,donnees=donnees,code_epsg=code_epsg,emprise=emprise,nom_fond="fond_flux",var_flux=varFlux)
      )
    }

    return(map)
  }
