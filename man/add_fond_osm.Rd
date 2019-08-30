\name{add_fond_osm}
\alias{add_fond_osm}

\title{
	Add an OpenStreetMap background on a 'leaflet' map
}
\description{
	Add an OpenStreetMap background on a 'leaflet' map.
}
\usage{
add_fond_osm(map, opacityAnalyse = 1, colBordure = "white", epaisseurBordure = 1)
}

\arguments{
  \item{map}{
	objet leaflet.
}
\item{opacityAnalyse}{
	valeur numerique (numeric). Chiffre entre 0 (transparent) et 1 (opaque). Par defaut a 1.
}
  \item{colBordure}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
}
  \item{epaisseurBordure}{
	vecteur de numeriques (numeric). Par defaut a 1.
}
}
\details{
	L'ajout du fond OSM est definitif pour l'objet leaflet mis en parametre.
	
	Pour supprimer le fond OSM de la carte, l'objet leaflet doit etre regenere a partir d'une des fonctions leaflet_.
	
	Il faut ajouter le fond OSM directement apres la fonction leaflet_ et avant de creer eventuellement une legende.
}
\value{
	Retourne un objet de type leaflet
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}, \link{leaflet_typo},}
	
	\code{\link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs}}
}
\examples{
data("donnees_monoloc")
data("depm")

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015")
map <- add_fond_osm(map)
\donttest{
 map
}
}
\keyword{documentation}
