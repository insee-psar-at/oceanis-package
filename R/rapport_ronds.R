#' @title Ratio between radius and value for 'leaflet' maps
#'
#' @description Returns the ratio between the area of the largest circle and the maximum
#' value of the volume data for 'leaflet' maps.
#'
#' @details Le rapport n'a pas d'unite. Il s'agit d'une valeur relative.
#'
#' Le calcul du rapport sert a comparer plusieurs cartes en ronds
#' proportionnels entre elles puisque la valeur du rapport est fonction du
#' volume a representer.
#'
#' La formule utilisee pour calculer le rapport est :
#' (pi*(rayonRond)^2)/max_var
#'
#' Il peut y avoir une legere difference entre la valeur du rapport passee en
#' parametre de la fonction \code{leaflet_ronds} et la valeur renvoyee par la
#' fonction \code{rapport_ronds}. Cette difference s'explique par l'arrondi de
#' pi a 6 chiffres apres la virgule.
#'
#' @usage rapport_ronds(map)
#'
#' @param map objet leaflet.
#'
#' @return Retourne un numerique.
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_ronds_classes},
#' \link{leaflet_classes_ronds}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#' data("depm")
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' 					 varVolume = "POP_2015")
#' \donttest{
#'  map
#' }
#' rapport_ronds(map)
#' # [1] 1924095
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varVolume = "POP_2015", rapportRond = 1900000)
#' \donttest{
#'  map
#' }
#' rapport_ronds(map)
#' # [1] 1900022
#'
#' @import leaflet
#'
#' @export rapport_ronds
#'
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
        if(map$x$calls[[i]]$args[5][[1]]=="legende_ronds") idx_legende <- c(idx_legende,i)
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
      max_var <- map$x$calls[[j[idx]]]$args[[4]]$max_var
      rapport <- (pi*(rayonRond)^2)/max_var
    }else
    {
      stop(simpleError("Il n'y a pas d'analyse en ronds dans la map"))
    }

    return(rapport)
  }
