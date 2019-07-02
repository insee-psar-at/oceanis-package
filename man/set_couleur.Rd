\name{set_couleur}
\alias{set_couleur_ronds}
\alias{set_couleur_classes}
\alias{set_couleur_typo}
\alias{set_couleur_joignantes}
\alias{set_couleur_saphirs}
\title{
	Modify the colors of 'leaflet' map's analysis
}
\description{
	Modify the colors of 'leaflet' map's analysis.
}
\usage{
set_couleur_ronds(map, colorPos = "#CD853F", colorNeg = "#6495ED", colBorder = "white")

set_couleur_classes(map, stylePalette = "defaut", palettePos = NULL, paletteNeg = NULL,
colBorder = "white")

set_couleur_typo(map, paletteTypo = NULL, colBorder = "white")

set_couleur_joignantes(map, colFleche = "#CD853F", colBorder = "black")

set_couleur_saphirs(map, colEntree = "#CD853F", colSortie = "#6495ED",
colBorder = "black")
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{colorPos}{
	chaine de caracteres (character). Couleur nommee ou hexadecimal. Par defaut "#CD853F".
}
  \item{colorNeg}{
	chaine de caracteres (character). Couleur nommee ou hexadecimal. Par defaut "#6495ED".
}
  \item{stylePalette}{
	chaine de caracteres (character). A choisir parmi "InseeFlash", "InseeAnalyse", "InseeDossier", "InseePremiere" ou "defaut" (par defaut).
}
  \item{palettePos}{
	vecteur de caracteres (character). Specifier le vecteur de couleurs (nommees ou hexadecimal) des valeurs positives (au choix). Par defaut a NULL.
}
  \item{paletteNeg}{
	vecteur de caracteres (character). Specifier le vecteur de couleurs (nommees ou hexadecimal) des valeurs negatives (au choix). Par defaut a NULL.
}
  \item{paletteTypo}{
	vecteur de caracteres (character). Specifier le vecteur de couleurs (nommees ou hexadecimal) de la typologie (au choix). Par defaut a NULL.
}
  \item{colBorder}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
}
  \item{colFleche}{
	chaine de caracteres (character). Couleur nommee ou hexadecimal. Par defaut "#CD853F".
}
  \item{colEntree}{
	chaine de caracteres (character). Couleur nommee ou hexadecimal. Par defaut "#CD853F".
}
  \item{colSortie}{
	chaine de caracteres (character). Couleur nommee ou hexadecimal. Par defaut "#6495ED".
}
}
\details{
	Les palettes proposees pour l'analyse en classes sont celles utilisees dans les publications Insee. Seule la palette par defaut propose des couleurs supplementaires par rapport a la palette utilisee dans la publication InsesFlash.
	
	Les couleurs sont classees du plus fonce au plus clair pour les palettes des valeurs positives et du plus clair au plus fonce pour les palettes des valeurs negatives.
	
	Le code des couleurs est le code hexadecimal.
	
	\itemize{
		\item InseeFlash positives : "#9B231C", "#B24B1D", "#D47130", "#E4A75A", "#F2CE93"
		\item InseeFlash negatives : "#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289"
        \item InseeAnalyse positives : "#5E2057","#853567","#8E5981","#BA97B2","#D7C0CC"
		\item InseeAnalyse negatives : "#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289"
        \item InseeDossier positives : "#4F185E","#65317B","#9475A5","#BFA5C6","#E7D1E5"
		\item InseeDossier negatives : "#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289"
        \item InseePremiere positives : "#7F0029","#CC1543","#DE635B","#F79C85","#FDE3DE"
		\item InseePremiere negatives : "#ECF4D8","#CDD78C","#91B778","#549534","#005941"
        \item defaut positives : "#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93"
		\item defaut negatives : "#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050"
	}
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
map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
map <- set_couleur_ronds(map = map, colBorder = "grey")
map <- set_couleur_classes(map = map, stylePalette = "InseePremiere", colBorder = "black")
\donttest{
 map
}

data("donnees_biloc_saphirs")
data("regm")

# Fleches saphirs
map <- leaflet_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG",
idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
varFlux = "MIGR", largeurFlecheMax = 500, direction = "Ent")
map <- set_couleur_saphirs(map = map, colEntree = "#91B778", colBorder = "grey")
\donttest{
 map
}
}
\keyword{documentation}
