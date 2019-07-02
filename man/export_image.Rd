\name{export_image}
\alias{export_jpeg}
\alias{export_pdf}
\alias{export_png}
\title{
	Export a 'leaflet' map in image format
}
\description{
	Export a 'leaflet' map in image format (.jpeg, .pdf or .png).
}
\usage{
export_jpeg(map, chemin, nomFichier)

export_pdf(map, chemin, nomFichier)

export_png(map, chemin, nomFichier)
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{chemin}{
	chaine de caracteres (character). Chemin du dossier d'exportation de la carte.
}
  \item{nomFichier}{
	chaine de caracteres (character). Nom du fichier en sortie sans l'extension.
}
}
\details{
	Attention, l'export en format image peut durer quelques minutes.
}
\value{
	Ne retourne aucun objet.
	
	Exporte la carte dans le format .jpeg, .pdf ou .png a l'emplacement specifie.
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},}
	
	\code{\link{leaflet_typo}, \link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs}}
}
\examples{
data("depm")
data("regm")

data("donnees_monoloc")

# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015")

\donttest{
 ## Not run:
 export_jpeg(map = map, chemin = tempdir(), nomFichier = "map")
 export_pdf(map = map, chemin = tempdir(), nomFichier = "map")
 export_png(map = map, chemin = tempdir(), nomFichier = "map")
 ## End(Not run)
}
}
\keyword{documentation}
