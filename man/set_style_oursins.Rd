% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_style_oursins.R
\name{set_style_oursins}
\alias{set_style_oursins}
\title{Modify the style of 'leaflet' map's urchins}
\usage{
set_style_oursins(map, epaisseur = 2, colTrait = "black", map_leaflet
= NULL)
}
\arguments{
\item{map}{objet leaflet.}

\item{epaisseur}{valeur numerique (numeric). Par defaut a 2.}

\item{colTrait}{chaine de caracteres (character). Couleur nommee (par
exemple "black") ou hexadecimal (par exemple "#000000"). Par defaut "black".}

\item{map_leaflet}{objet leaflet. Pour l'integration des fonctions leaflet
dans les applications shiny (cf vignette). Par defaut a NULL.}
}
\value{
Retourne un objet leaflet.
}
\description{
Modify the style of 'leaflet' map's urchins.
}
\examples{

data("donnees_biloc")
data("regm")

# Oursins
map <- leaflet_oursins(data = donnees_biloc, fondMaille = regm, idDataDepart = "REG_DEPART",
idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", filtreDist = 1000,
filtreMajeurs = 3)
map <- set_style_oursins(map = map, epaisseur = 3, colTrait = "grey")
\donttest{
 map
}

}
\seealso{
\code{\link{leaflet_fonds_simples}}

\code{\link{leaflet_ronds} \link{leaflet_classes}
\link{leaflet_ronds_classes} \link{leaflet_classes_ronds}}

\code{\link{leaflet_typo} \link{leaflet_oursins} \link{leaflet_joignantes}
\link{leaflet_saphirs}}
}
\keyword{documentation}
