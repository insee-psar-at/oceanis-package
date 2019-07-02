\name{set_opacite_elargi}
\alias{set_opacite_elargi}
\title{
	Modify the opacity of the expanded representation of a 'leaflet' map
}
\description{
	Modify the opacity of the expanded representation of a 'leaflet' map for proportional circles and chroropleth.
}
\usage{
set_opacite_elargi(map, opacite = 0.6)
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{opacite}{
	valeur numerique (numeric). Chiffre entre 0 (transparent) et 1 (opaque). Par defaut a 0.6 (60 pour cent d'opacite ou 40 pour cent de transparence).
}
}
\details{
	Seule la representation elargie est concernee par la modification de l'opacite. La representation principale reste 100 pour cent opaque.
}
\value{
	Retourne un objet leaflet.
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}, \link{leaflet_typo},}
	
	\code{\link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs}}
}
\examples{
data("donnees_monoloc")
data("depm")

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm[depm$REG=="93",],
fondMailleElargi = depm, fondSuppl = depm, idData = "COD_DEP",
varVolume = "POP_2015")
map <- set_opacite_elargi(map = map, opacite = 0.8)
\donttest{
 map
}
}
\keyword{documentation}
