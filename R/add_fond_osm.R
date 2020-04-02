#' @title Add an OpenStreetMap background on a 'leaflet' map
#'
#' @description Add an OpenStreetMap background on a 'leaflet' map.
#'
#' @details L'ajout du fond OSM est definitif pour l'objet leaflet mis en parametre.
#'
#' Pour supprimer le fond OSM de la carte, l'objet leaflet doit etre regenere a
#' partir d'une des fonctions leaflet_.
#'
#' Il faut ajouter le fond OSM directement apres la fonction leaflet_ et avant
#' de creer eventuellement une legende.
#'
#' @usage add_fond_osm(map, opacityAnalyse = 1, colBordure = "white",
#' epaisseurBordure = 1)
#'
#' @param map objet leaflet.
#' @param opacityAnalyse valeur numerique (numeric). Chiffre entre 0
#' (transparent) et 1 (opaque). Par defaut a 1.
#' @param colBordure chaine de caracteres (character). Couleur nommee (par
#' exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
#' @param epaisseurBordure vecteur de numeriques (numeric). Par defaut a 1.
#'
#' @return Retourne un objet de type leaflet
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},
#' \link{leaflet_typo},}
#'
#' \code{\link{leaflet_oursins}, \link{leaflet_joignantes},
#' \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#' data("depm")
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015")
#' map <- add_fond_osm(map)
#' \donttest{
#'  map
#' }
#'
#' @import leaflet
#'
#' @export add_fond_osm
#'
add_fond_osm <-
function(map,opacityAnalyse=1,colBordure="white",epaisseurBordure=1)
  {
    msg_error1 <- msg_error2 <- msg_error3 <- msg_error4 <- NULL

    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(class(opacityAnalyse)!="numeric")) msg_error2 <- "La variable doit etre de type numerique / "
    if(any(class(colBordure)!="character")) msg_error3 <- "Le style de couleur doit etre de type caractere / "
    if(any(class(epaisseurBordure)!="numeric")) msg_error4 <- "L'epaisseur de bordure doit etre de type numerique / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }

    if(any(class(map) %in% "leaflet"))
    {
      j <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          j <- c(j,i)
        }
      }
      map$x$calls <- map$x$calls[-j[c(1,2)]]
      j <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          j <- c(j,i)
        }
      }
      map$x$calls[[j[length(j)]]]$args[[4]]$opacity <- opacityAnalyse
      map$x$calls[[j[length(j)]]]$args[[4]]$fillOpacity <- opacityAnalyse
      map$x$calls[[j[length(j)]]]$args[[4]]$color <- colBordure
      map$x$calls[[j[length(j)]]]$args[[4]]$weight <- epaisseurBordure

      map <- leaflet::addTiles(map,
                      urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                      attribution = NULL,
                      layerId = 4)
    }else if(any(class(map) %in% "leaflet_proxy"))
    {
      removeTiles(map, layerId = 4)
      map <- leaflet::addTiles(map,
                      urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                      attribution = NULL,
                      layerId = 4)
    }

    return(map)
  }
