\name{add_legende}
\alias{add_legende_ronds}
\alias{add_legende_classes}
\alias{add_legende_typo}
\alias{add_legende_typo_symboles}
\alias{add_legende_joignantes}
\alias{add_legende_saphirs}

\title{
	Add a legend on a 'leaflet' map
}
\description{
	Add a legend on a 'leaflet' map for proportional circles, chroropleth, typology, proportional and sapphire arrows.
}
\usage{
add_legende_ronds(map, titre = NULL, lng = NULL, lat = NULL, dom = "0", precision = 0,
					zoom = 8, map_leaflet = NULL)

add_legende_classes(map, titre = NULL, lng = NULL, lat = NULL, typeLegende = 1,
					zoom = 8, map_leaflet = NULL)

add_legende_typo(map, titre = NULL, lng = NULL, lat = NULL, labels = NULL,
					zoom = 8)
					
add_legende_typo_symboles(map, titre = NULL, lng = NULL, lat = NULL, labels = NULL,
					zoom = 8, map_leaflet = NULL)

add_legende_joignantes(map, titre = NULL, lng = NULL, lat = NULL, dom = "0",
						precision = 0, zoom = 8)

add_legende_saphirs(map, titre = NULL, lng = NULL, lat = NULL, dom = "0", precision = 0,
					zoom = 8)
}

\arguments{
  \item{map}{
	objet leaflet.
}
  \item{titre}{
	chaine de caracteres (character). Titre de la legende des classes. Par defaut a NULL.
}
  \item{lng}{
	valeur numerique (numeric). Longitude (x) de la legende des classes dans le systeme de projection WGS84 (code EPSG 4326). Par defaut a NULL.
}
  \item{lat}{
	valeur numerique (numeric). Latitude (y) de la legende des classes dans le systeme de projection WGS84 (code EPSG 4326). Par defaut a NULL.
}
  \item{dom}{
	chaine de caracteres (character). "0" (par defaut pour la France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973" (Guyane), "974" (La Reunion) ou "976" (Mayotte). Ce parametre permet de definir le systeme de projection local de la carte.
}
  \item{typeLegende}{
	valeur numerique (numeric). 1 (par defaut) pour une legende litterale, 2 pour une legende en echelle.
}
  \item{labels}{
	vecteur de caracteres (character). Labels personnalises pour la legende de la carte en typologie. Par defaut a NULL.
}
  \item{precision}{
	valeur numerique (numeric). Arrondit les valeurs de la legende des ronds ou des fleches. 0 (par defaut) pour arrondir a l'unite, -1 pour arrondir a la dizaine, -2 a la centaine...
}
  \item{zoom}{
	valeur numerique (numeric). Valeur entre 6 et 10 (8 par defaut). Definit le niveau de zoom de la carte pour un affichage optimal.
}
  \item{map_leaflet}{
	objet leaflet. Pour l'integration des fonctions leaflet dans les applications shiny (cf vignette). Par defaut a NULL.
}
}
\details{
	Par defaut, \code{lng} et \code{lat} sont NULL ; la legende est alors positionnee en haut a droite de la carte sauf pour l'analyse en classes ou elle est positionnee en bas.
	
	Les valeurs \code{lng} et \code{lat} peuvent etre negatives. Une longitude a 0 correspond au meridien de Greenwich.
		
	Pour deplacer la legende, reexecutez la fonction add_legende_xxx en specifiant les parametres \code{lng} et \code{lat}.
	
	Pour supprimer la legende, reexecutez la fonction add_legende_xxx en laissant les parametres \code{lng} et \code{lat} a NULL.
	
	Pour supprimer le titre, reexecutez la fonction add_legende_xxx en laissant le parametre \code{titre} a NULL.
	
	Concernant la legende des ronds proportionnels, le grand cercle correspond a la valeur max en volume et le petit cercle au tiers de la valeur max.
	
	Idem pour la largeur des fleches joignantes et des fleches saphirs.
	
	Les valeurs de la legende des ronds et des fleches peuvent etre arrondies grace au parametre \code{precision}.
	
	Pour la légende des cartes en typologie, il est possible de specifier ses propres labels sans avoir besoin de modifier les noms de variables dans la table de donnees.
	
	Si le titre et les valeurs de legende sont trop decales, il faut modifier la valeur du zoom : entre 6 (zoom maximal, niveau commune) et 10 (zoom minimal, niveau France metro)
}
\value{
	Retourne un objet de type leaflet.
}
\seealso{
	\code{\link{coord_legende}}
	
	\code{\link{leaflet_fonds_simples}}
	
	\code{\link{leaflet_ronds} \link{leaflet_classes} \link{leaflet_ronds_classes} \link{leaflet_classes_ronds} \link{leaflet_typo}}
	
	\code{\link{leaflet_oursins} \link{leaflet_joignantes} \link{leaflet_saphirs}}
}
\examples{
data("depm")
data("regm")

data("donnees_monoloc")

# Ronds proportionnels sur une analyse en classes
map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
map <- add_legende_classes(map = map, titre = "VAR_AN_MOY", lng = 9, lat = 48, zoom = 6)
map <- add_legende_ronds(map = map, titre = "POP_2015", lng = 9, lat = 50, zoom = 6)
\donttest{
 map
}

data("donnees_biloc")

# Fleches joignantes
map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
varFlux = "MIGR", filtreDist = 1000, filtreMajeurs = 3)
map <- add_legende_joignantes(map = map, titre = "MIGR")
\donttest{
 map
}
}
\keyword{documentation}
