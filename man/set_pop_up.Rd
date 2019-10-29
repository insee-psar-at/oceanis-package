\name{set_pop_up}
\alias{set_pop_up}
\title{
	Modify the pop_up of 'leaflet' map's analysis
}
\description{
	Modify the pop_up of 'leaflet' map's analysis.
}
\usage{
set_pop_up(map, popupRonds = NULL, popupClasses = NULL)
}
\arguments{
  \item{map}{
	objet leaflet.
}
  \item{popupRonds}{
	vecteur de caracteres (character). Peut etre du HTML pour mettre en forme le texte. Par defaut a NULL.
}
  \item{popupClasses}{
	vecteur de caracteres (character). Peut etre du HTML pour mettre en forme le texte. Par defaut a NULL.
}
}
\details{
	Le nouveau texte pour le pop-up peut etre du contenu HTML.
	Par exemple : "<b> <font color=#2B3E50>Greater City of Paris</font> </b><br><b><font color=#2B3E50>PART : </font></b><b> <font color=#2B3E50>21,2</font></b>"
	Attention : conserver le meme ordre des pop-up dans le vecteur que celui des donnees pour que les valeurs correspondent bien a l'analyse.
	
	Pour supprimer les pop-up, laisser le parametre a NULL.
}
\value{
	Retourne un objet de type leaflet.
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}, \link{leaflet_typo},}
	
	\code{\link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs}}
}
\examples{
data("depm")

data("donnees_monoloc")

# Ronds proportionnels sur une analyse en classes
map <- leaflet_ronds_classes(data = donnees_monoloc[donnees_monoloc$REG=="93",],
fondMaille = depm[depm$REG=="93",], idData = "COD_DEP", varVolume = "POP_2015",
varRatio = "VAR_AN_MOY", nbClasses = 4)

new_popup <- c(paste0("<b><font color=#2B3E50>Bouches-du-Rhone</font></b><br>",
"<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>2 016,6</font><br>",
"<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,4</font>"),
paste0("<b><font color=#2B3E50>Alpes-Maritimes</font></b><br>",
"<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>1 082,4</font><br>",
"<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,1</font>"),
paste0("<b><font color=#2B3E50>Var</font></b><br>",
"<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>1 048,7</font><br>",
"<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,8</font>"),
paste0("<b><font color=#2B3E50>Vaucluse</font></b><br>",
"<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>  557,5</font><br>",
"<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,6</font>"),
paste0("<b><font color=#2B3E50>Alpes-de-Haute-Provence</font></b><br>",
"<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>  161,8</font><br>",
"<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,2</font>"),
paste0("<b><font color=#2B3E50>Hautes-Alpes</font></b><br>",
"<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>  140,9</font><br>",
"<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,6</font>"))
				
map <- set_pop_up(map = map, popupRonds = new_popup)
\donttest{
 map
}
}
\keyword{documentation}
