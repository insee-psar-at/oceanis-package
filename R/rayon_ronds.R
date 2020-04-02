#' @title Radius of the largest circle for 'leaflet' maps
#'
#' @description Returns the radius of the largest circle for 'leaflet' maps.
#'
#' @details L'unite du rayon est le metre.
#'
#' A noter, que la taille du rayon est limitee afin d'eviter de trop masquer le
#' territoire d'etude en arriere-plan.
#'
#' En effet, la regle semiologique des 1/7eme s'applique a toutes les cartes
#' affichant des ronds proportionnels. Cette regle specifie que la somme des
#' aires des ronds ne doit pas depasser 1/7eme de l'aire du territoire d'etude.
#'
#' @usage rayon_ronds(map)
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
#' rayon_ronds(map)
#' # [1] 39944.67
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varVolume = "POP_2015", rayonRond = 30000)
#' \donttest{
#'  map
#' }
#' rayon_ronds(map)
#' # [1] 30000
#'
#' @import leaflet
#'
#' @export rayon_ronds
#'
rayon_ronds <-
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
    }else
    {
      stop(simpleError("Il n'y a pas d'analyse en ronds dans la map"))
    }

    return(rayonRond)
  }
