#' @title Width of the arrow for 'leaflet' maps
#'
#' @description Width of the largest arrow of the map in kilometers for 'leaflet' maps.
#'
#' @details La largeur de la fleche est disponible uniquement pour les representations
#' de fleches joignantes et de fleches saphirs.
#'
#' @usage largeur_fleche(map)
#'
#' @param map objet leaflet.
#'
#' @return Retourne une valeur numerique.
#'
#' @seealso \code{\link{leaflet_joignantes} \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_biloc")
#' data("regm")
#'
#' # Fleches joignantes
#' map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
#' idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", filtreDist = 1000, filtreMajeurs = 3)
#' \donttest{
#'  map
#' }
#' largeur_fleche(map)
#' # [1] 100
#'
#' # Fleches joignantes
#' map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
#' idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", largeurFlecheMax = 200, filtreDist = 1000,
#' filtreMajeurs = 3)
#' \donttest{
#'  map
#' }
#' largeur_fleche(map)
#' # [1] 200
#'
#' @import leaflet
#'
#' @export largeur_fleche
#'
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
        if(map$x$calls[[i]]$args[[2]]$nom_fond=="fond_flux") idx_fleche <- i
      }
    }

    if(!is.null(idx_fleche))
    {
      largeur <- map$x$calls[[idx_fleche]]$args[[2]]$largeur
    }else
    {
      stop(simpleError("Il n'y a pas d'analyse en fleches joignantes ou saphirs dans la map"))
    }

    return(largeur)
  }
