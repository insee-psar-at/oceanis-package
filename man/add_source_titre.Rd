\name{add_source_titre}
\alias{add_source}
\alias{add_titre}

\title{
	Add a source or title on a 'leaflet' map
}
\description{
	Add a source or title on a 'leaflet' map.
}
\usage{
add_source(map, source)

add_titre(map, titre, sousTitre = NULL)
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{source}{
	chaine de caracteres (character). Source de la carte.
}
\item{titre}{
	chaine de caracteres (character). Titre de la carte.
}
\item{sousTitre}{
	chaine de caracteres (character). Sous-titre de la carte. Par defaut a NULL.
}
}
\details{
	Pour supprimer la source, reexecutez la fonction add_source en laissant le parametre \code{source} a NULL.
	
	Pour supprimer le titre ou le sous-titre, reexecutez la fonction add_titre en laissant le parametre \code{titre} ou \code{sousTitre} a NULL.
}
\value{
	Retourne un objet de type leaflet.
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}, \link{leaflet_typo},}
	
	\code{\link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs}}
}
\examples{
data("donnees_monoloc")
data("depm")

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
varVolume = "POP_2015")
map <- add_titre(map = map,
titre = "Population des departements de France metropolitaine en 2015")
map <- add_source(map = map, source = "Source : INSEE - RP2016")
\donttest{
 map
}
}
\keyword{documentation}
