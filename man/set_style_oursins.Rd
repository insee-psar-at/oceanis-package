\name{set_style_oursins}
\alias{set_style_oursins}
\title{
	Modify the style of 'leaflet' map's urchins
}
\description{
	Modify the style of 'leaflet' map's urchins.
}
\usage{
set_style_oursins(map, epaisseur = 2, colTrait = "black")
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{epaisseur}{
	valeur numerique (numeric). Par defaut a 2.
}
  \item{colTrait}{
	chaine de caracteres (character). Couleur nommee (par exemple "black") ou hexadecimal (par exemple "#000000"). Par defaut "black".
}
}
\value{
	Retourne un objet leaflet.
}
\seealso{
	\code{\link{leaflet_fonds_simples}}

	\code{\link{leaflet_ronds} \link{leaflet_classes} \link{leaflet_ronds_classes} \link{leaflet_classes_ronds}}
	
	\code{\link{leaflet_typo} \link{leaflet_oursins} \link{leaflet_joignantes} \link{leaflet_saphirs}}
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
\keyword{documentation}
