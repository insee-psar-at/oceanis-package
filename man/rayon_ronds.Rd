\name{rayon_ronds}
\alias{rayon_ronds}
\title{
	Radius of the largest circle for 'leaflet' maps
}
\description{
	Returns the radius of the largest circle for 'leaflet' maps.
}
\usage{
rayon_ronds(map)
}
\arguments{
  \item{map}{
	objet leaflet.
}
}
\details{
	L'unite du rayon est le metre.
	
	A noter, que la taille du rayon est limitee afin d'eviter de trop masquer le territoire d'etude en arriere-plan.
	
	En effet, la regle semiologique des 1/7eme s'applique a toutes les cartes affichant des ronds proportionnels. Cette regle specifie que la somme des aires des ronds ne doit pas depasser 1/7eme de l'aire du territoire d'etude.
}
\value{
	Retourne un numerique.
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}}
}
\examples{
data("donnees_monoloc")
data("depm")

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
					 varVolume = "POP_2015")
\donttest{
 map
}
rayon_ronds(map)
# [1] 39944.67

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
varVolume = "POP_2015", rayonRond = 30000)
\donttest{
 map
}
rayon_ronds(map)
# [1] 30000
}
\keyword{documentation}
