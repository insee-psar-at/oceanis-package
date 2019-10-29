\name{set_bordure_ronds}
\alias{set_bordure_ronds}
\title{
	Modify the circles' borders of 'leaflet' map's analysis
}
\description{
	Modify the circles' borders of 'leaflet' map's analysis.
}
\usage{
set_bordure_ronds(map, colBorderPos = "white", colBorderNeg = "white",
epaisseurBorder = 1, map_leaflet = NULL)
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{colBorderPos}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
}
  \item{colBorderNeg}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
}
  \item{epaisseurBorder}{
	chaine de caracteres (numeric). Epaisseur de la bordure des ronds. Par defaut a 1".
}
  \item{map_leaflet}{
	objet leaflet. Pour l'integration des fonctions leaflet dans les applications shiny (cf vignette). Par defaut a NULL.
}
}
\details{
	Permet de modifier l'apparence des bordures des ronds, c'est-a-dire leur couleur et leur epaisseur.
	
	Il est possible de distinguer les valeurs en volume positives des valeurs en volume negatives.
	
	Applicable pour les analyses en ronds proportionnels, en classes dans les ronds et en ronds sur les classes.
}
\value{
	Retourne un objet de type leaflet.
}
\references{
	Un convertisseur de couleurs pour visualiser une couleur a partir de son nom, son code hexadecimal ou RGB : http://www.proftnj.com/RGB3.htm
}
\seealso{
	\code{\link{recup_palette},}
	
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}, \link{leaflet_typo},}
	
	\code{\link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs}}
}
\examples{
data("depm")

data("donnees_monoloc")

# Ronds proportionnels sur une analyse en classes
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
varVolume = "POP_2015")
map <- set_bordure_ronds(map = map, colBorderPos = "grey", epaisseurBorder = 3)
\donttest{
 map
}
}
\keyword{documentation}
