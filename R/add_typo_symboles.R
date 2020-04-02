#' @name add_typo_symboles
#'
#' @title Add symbols on a 'leaflet' map
#'
#' @description Add a symbols on a 'leaflet' map for typology.
#'
#' @details Pour ajouter des symboles a la carte leaflet, il faut fournir un fond de
#' points et si besoin preciser le type des points (par defaut un carre plein -
#' voir l'aide de ?graphics::points), leurs couleurs (par defaut rouge), leurs
#' tailles (par defaut 30) et l'epaisseur de la bordure du symbole si ce
#' dernier n'est pas plein.
#'
#' Attention a l'ordre des elements des vecteurs qui doivent correspondre a
#' l'ordre du fond de points.
#'
#' @usage add_typo_symboles(map, fondPoints, types = NULL, couleurs = NULL,
#' tailles = NULL, epaisseurs = NULL)
#'
#' @param map objet leaflet.
#' @param fondPoints objets sf. Fond de points.
#' @param types vecteur de numeriques (numeric). Par defaut a NULL. Voir
#' details.
#' @param couleurs vecteur de caracteres (character). Par defaut a NULL. Voir
#' details.
#' @param tailles vecteur de numeriques (numeric). Par defaut a NULL. Voir
#' details.
#' @param epaisseurs vecteur de numeriques (numeric). Par defaut a NULL. Voir
#' details.
#'
#' @return Retourne un objet de type leaflet.
#'
#' @seealso \code{\link{coord_legende}}
#'
#' \code{\link{leaflet_fonds_simples}}
#'
#' \code{\link{leaflet_ronds} \link{leaflet_classes}
#' \link{leaflet_ronds_classes} \link{leaflet_classes_ronds}
#' \link{leaflet_typo}}
#'
#' \code{\link{leaflet_oursins} \link{leaflet_joignantes}
#' \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#' data("regm")
#'
#' # Typologie en symboles
#' map <- leaflet_fonds_simples(listFonds = list(depm))
#' points <- suppressWarnings(sf::st_centroid(depm[depm$CODE=="75",]))
#' points <- rbind(points,suppressWarnings(sf::st_centroid(depm[depm$CODE=="13",])))
#' points <- rbind(points,suppressWarnings(sf::st_centroid(depm[depm$CODE=="69",])))
#' types <- c(16,15,15)
#' couleurs <- c("blue","red","red")
#' tailles <- c(25,20,20)
#' map <- add_typo_symboles(map, fondPoints=points, types = types,
#' couleurs = couleurs, tailles = tailles)
#' \donttest{
#'  map
#' }
#'
#' @import leaflet sf grDevices graphics
#'
#' @export add_typo_symboles
#'
add_typo_symboles <-
function(map,fondPoints,types=NULL,couleurs=NULL,tailles=NULL,epaisseurs=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6 <- NULL

    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(any(!any(class(fondPoints) %in% "sf"),!any(class(fondPoints) %in% "data.frame"))) msg_error2 <- "Le fond de points doit etre un objet sf / "
    if(!is.null(types)) if(any(class(types)!="numeric")) msg_error3 <- "Le type des symboles doit etre un vecteur de valeurs numeriques (de 0 a 25 - voir aide) / "
    if(!is.null(couleurs)) if(any(class(couleurs)!="character")) msg_error4 <- "La couleur des symboles doit etre un vecteur de chaines de caracteres (nommee ou hexadecimal) / "
    if(!is.null(tailles)) if(any(class(tailles)!="numeric")) msg_error5 <- "La taille des symboles doit etre un vecteur de valeurs numeriques / "
    if(!is.null(epaisseurs)) if(any(class(epaisseurs)!="numeric")) msg_error6 <- "L'epaisseur des symboles doit etre un vecteur de valeurs numeriques / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),!is.null(msg_error6)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6)))
    }

    names(fondPoints)[1] <- "CODE"
    names(fondPoints)[2] <- "LIBELLE"
    fondPoints$LIBELLE<-iconv(fondPoints$LIBELLE,"latin1","utf8")

    fondPoints <- st_transform(fondPoints,"+init=epsg:4326 +proj=longlat +ellps=WGS84")

    lng_points <- st_coordinates(fondPoints)[,1]
    lat_points <- st_coordinates(fondPoints)[,2]

    if(is.null(types)) types <- rep(15,nrow(fondPoints))
    if(is.null(couleurs)) couleurs <- rep("red",nrow(fondPoints))
    if(is.null(tailles)) tailles <- rep(30,nrow(fondPoints))
    if(is.null(epaisseurs)) epaisseurs <- rep(2,nrow(fondPoints))

    symbLeg <- unique(data.frame(types,couleurs,tailles,epaisseurs))

    #voir aide de ?graphics::points
    pchIcons <- function(pch = 0:25, width = 30, height = 30, ...) {
      n <- length(pch)
      files <- character(n)
      # create a sequence of png images
      for (i in seq_len(n)) {
        f <- tempfile(fileext = ".png")
        suppressWarnings(png(f, width = width, height = height, bg = "transparent", units = "px"))
        par(mar = c(0, 0, 0, 0))
        plot.new()
        points(0.5, 0.5, pch = pch[i], cex = min(width, height)/8, ...)
        dev.off()
        files[i] <- f
      }
      files
    }

    for(i in 1:nrow(fondPoints))
    {
      iconFiles <- pchIcons(pch = types[i], width = tailles[i], height = tailles[i], col = couleurs[i], lwd = epaisseurs[i])

      map <- addMarkers(map = map, lng = lng_points[i], lat = lat_points[i], icon = icons(iconUrl = iconFiles, iconAnchorX = tailles[i]/2, iconAnchorY = tailles[i]/2),
                        popup = as.data.frame(fondPoints)[i,2],
                        group = list(nom_couche="carte_typo_symb",types=symbLeg$types,couleurs=symbLeg$couleurs,tailles=symbLeg$tailles,epaisseurs=symbLeg$epaisseurs))
    }

    unlink(iconFiles)

    return(map)
  }
