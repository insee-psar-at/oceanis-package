\name{coord_legende}
\alias{coord_legende}

\title{
	Returns the lon / lat coordinates of a 'leaflet' map's legend
}
\description{
	Returns the lon / lat coordinates of a 'leaflet' map's legend for proportional circles, chroropleth, typology, proportional and sapphire arrows.
}
\usage{
coord_legende(map)
}
\arguments{
  \item{map}{
	objet leaflet.
}
}
\details{
	Au moins une legende doit exister sur la carte avant de pouvoir connaitre sa position. Les cartes en ronds proportionnels avec une analyse en classes ont 2 legendes.
}
\value{
	Retourne un objet data.frame avec les coordonnees lon/lat de la (ou des) legende(s). Le type de legende est indique dans le row.names.
}
\seealso{
	\code{\link{add_legende_ronds}, \link{add_legende_classes}, \link{add_legende_typo},}
	
	\code{\link{add_legende_joignantes}, \link{add_legende_saphirs},}
	
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes},}
	
	\code{\link{leaflet_classes_ronds}, \link{leaflet_typo}, \link{leaflet_oursins}, \link{leaflet_joignantes},}
	
	\code{\link{leaflet_saphirs}}
}
\examples{
data("donnees_monoloc")
data("depm")

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015")
map <- add_legende_ronds(map = map, titre = "POP_2015")
\donttest{
 map
}

coord <- coord_legende(map)
map <- add_legende_ronds(map = map, titre = "POP_2015", lng = 8, lat = 50)
\donttest{
 map
}
}
\keyword{documentation}
