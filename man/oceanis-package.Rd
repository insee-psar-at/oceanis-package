\name{oceanis-package}
\alias{oceanis-package}
\alias{oceanis}
\docType{package}
\title{
\packageTitle{oceanis}
}
\description{
\packageDescription{oceanis}
}
\details{
	Package : oceanis
	
	Type : Package
	
	Version : 0.8.6
	
	Date : 2019-07-05
	
	License : GPL(>=2.0)
	
	Outil de cartographie permettant de realiser des cartes d'etudes et d'analyses (ronds proportionnels, analyse en classes, typologie, oursins, fleches joignantes et fleches saphirs).
	
	Le package \code{oceanis} repond a 3 types de besoins :
	
	- Creer des cartes fixes avec la possibilite d'ajouter des etiquettes. Ideal pour les impressions de documents (.pdf ou autres). Peut etre integre dans les applications.
	
	- Creer des cartes interactives grace au zoom et aux pop-up (technologie leaflet). Utile pour l'integration dans une application ou dans une presentation en direct.
	
	- Creer des cartes dans un environnement web ou les parametres sont modifiables a la volee (technologie R-Shiny + leaflet). Utile pour l'integration dans une application ou dans une presentation en direct. Ici, la carte est externalisee vers une nouvelle fenetre du navigateur.
	
	
	Les fonctions de creation de cartes sont classees en 3 categories, correspondant aux 3 besoins precedents. Elles sont respectivement prefixees par \code{plot_}, \code{leaflet_} et \code{shiny_}.
	
	D'autres fonctions existent pour completer l'offre :
	
	Des fonctions pour exporter une carte leaflet en projet Qgis, des fonctions pour modifier son apparence, des fonctions permettant de creer ses propres zonages (connus ou a facon) a partir de tableaux de donnees.
	
	
	Un exemple possible d'enchainement de fonctions :
	
	1- Chargement du package oceanis (\code{library(oceanis)})
	
	2- Chargement des donnees avec la fonction \code{import()} du package \code{rio}
	
	3- Chargement des fonds de carte avec la fonction \code{read_sf()} du package \code{sf}
	
	4- Appel a une fonction de creation de carte (shiny_(), leaflet_() ou plot_())
	
	5- Modification du parametrage (legende, fonds supplementaire, taille, couleur, habillage...)
	
	6- Export de la carte en projet Qgis (uniquement pour les fonctions shiny_() et leaflet_())
	
	ou 6-bis- Export de la carte en format image.
	
	
	Pour resumer, voici un comparatif des fonctionnalites proposees :
	
	                        PLOT
							
	Interactivite :          Non
	
	Personnalisation / Parametrages :      Fort
	
	Integration dans les applications :          Moyen
	
	Export Qgis :            Non
	
	Etiquettes :             Oui
	
	Habillage :              Fort
	
	Zonage a facon :         Oui
	
	Representation elargie : Non
	
	
		                    LEAFLET
							
	Interactivite :          Oui
	
	Personnalisation / Parametrages :       Moyen
	
	Integration dans les applications :          Fort
	
	Export Qgis :            Oui
	
	Etiquettes :             Non
	
	Habillage :              Fort
	
	Zonage a facon :         Oui
	
	Representation elargie : Oui
	
		                    R-SHINY
							
	Interactivite :          Oui
	
	Personnalisation / Parametrages :       Faible
	
	Integration dans les applications :          Moyen
	
	Export Qgis :            Oui
	
	Etiquettes :             Non
	
	Habillage :              Faible
	
	Zonage a facon :         Oui
	
	Representation elargie : Oui
}
\author{
\packageAuthor{oceanis}
Maintainer: \packageMaintainer{oceanis}
}
\keyword{package}