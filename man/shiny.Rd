\name{shiny}
\alias{shiny_ronds}
\alias{shiny_classes}
\alias{shiny_ronds_classes}
\alias{shiny_classes_ronds}
\alias{shiny_typo}
\alias{shiny_oursins}
\alias{shiny_joignantes}
\alias{shiny_saphirs}
\title{
	Creation 'leaflet' maps in a 'shiny' web environment
}
\description{
	Creation 'leaflet' maps in a 'shiny' web environment where the parameters are modifiable on the fly.
}
\usage{
shiny_ronds(data, fondMaille, fondMailleElargi = NULL, fondContour, fondSuppl = NULL,
idData, varVolume, dom = "0", fondChx = NULL)

shiny_classes(data, fondMaille, fondMailleElargi = NULL, fondContour, fondSuppl = NULL,
idData, varRatio, dom = "0")

shiny_ronds_classes(data, fondMaille, fondMailleElargi = NULL, fondContour,
fondSuppl = NULL, idData, varVolume, varRatio, dom = "0",
fondChx = NULL)

shiny_classes_ronds(data, fondMaille, fondMailleElargi = NULL, fondContour,
fondSuppl = NULL, idData, varVolume, varRatio, dom = "0",
fondChx = NULL)

shiny_typo(data, fondMaille, fondContour, fondSuppl = NULL, idData, varTypo, dom = "0")

shiny_oursins(data, fondMaille, fondContour, fondSuppl = NULL, idDataDepart,
idDataArrivee, varFlux, decalageAllerRetour = 0, decalageCentroid = 0,
dom = "0")

shiny_joignantes(data, fondMaille, typeMaille, fondContour, fondSuppl = NULL,
idDataDepart, idDataArrivee, varFlux, decalageAllerRetour = 0,
decalageCentroid = 0, dom = "0")

shiny_saphirs(data, fondMaille, typeMaille, fondContour, fondSuppl = NULL, idDataDepart,
idDataArrivee, varFlux, direction = "Ent", dom = "0")
}
\arguments{
  \item{data}{
	tableau de donnees (data.frame).
}
  \item{fondMaille}{
	objet sf. Fond de carte.
}
  \item{typeMaille}{
	chaine de caracteres (character). Type de maille issu d'un zonage administratif ou d'etudes ("REG", "DEP", "UU", "AU", "ZE", "BV", "EPCI" ou "COM").
}
  \item{fondMailleElargi}{
	objet sf. Fond de carte. Par defaut a NULL.
}
  \item{fondContour}{
	objet sf. Fond de carte.
}
  \item{fondSuppl}{
	objet sf. Fond de carte. Par defaut a NULL.
}
  \item{idData}{
	chaine de caracteres (character). Variable identifiant la maille.
}
  \item{idDataDepart}{
	chaine de caracteres (character). Variable identifiant le depart du flux.
}
  \item{idDataArrivee}{
	chaine de caracteres (character). Variable identifiant l'arrivee du flux.
}
  \item{varVolume}{
	chaine de caracteres (character). Variable en volume pour les ronds proportionnels.
}
  \item{varRatio}{
	chaine de caracteres (character). Variable en ratio pour l'analyse en classes.
}
  \item{varTypo}{
	chaine de caracteres (character). Variable de typologie.
}
  \item{varFlux}{
	chaine de caracteres (character). Variable de flux pour les oursins, fleches joignantes ou fleches saphirs.
}
  \item{direction}{
	chaine de caracteres (character). Type de flux. A choisir parmi "Ent" pour entrees (par defaut), "Sor" pour sorties et "Sol" pour soldes (entrees - sorties).
}
  \item{decalageAllerRetour}{
	valeur numerique (numeric). Decalage entre les fleches aller-retour (km).
}
  \item{decalageCentroid}{
	valeur numerique (numeric). Decalage des fleches depuis et vers les centroides des territoires (km).
}
  \item{dom}{
	chaine de caracteres (character). "0" (par defaut pour la France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973" (Guyane), "974" (La Reunion) ou "976" (Mayotte). Ce parametre permet de definir le systeme de projection de la carte.
}
  \item{fondChx}{
	objet sf. Fond des chefs-lieux. Pour les ronds proportionnels avec une maille communale uniquement, choix entre position du centre des ronds sur les centroides des communes (NULL par defaut) ou sur les chefs-lieux (chx) des communes.
}
}
\details{
	Les cartes sont externalisees de l'environnement R contrairement aux cartes plot et leaflet. Elles s'affichent dans le navigateur.
	
	Les cartes produites possedent les memes avantages que les cartes leaflet (deplacement de la carte, zoom/dezoom, pop-up d'informations, placement libre de la legende).
	
	L'environnement R-Shiny propose en plus la possibilite de modifier a la volee les parametres de la carte (ordre des fonds, ajout de fonds, representation elargie, taille et rapport des ronds, methode des classes, nombre de classes, bornes, filtre de flux, legende).
	
	Les cartes peuvent etre exportees en projet Qgis via un bouton de l'interface R-Shiny. La fonctionnalite cree un .zip a dezipper au meme emplacement.
	
	L'apparence des cartes (choix de la palette et des couleurs, etiquettes) n'est cependant pas modifiable. Le but des fonctions shiny est de proposer des cartes realisables et modifiables rapidement avec un minimum de parametrage. En effet, une mise en forme specifique pourra s'effecuer dans Qgis pour la diffusion par exemple.
	
	Il est obligatoire de renseigner le contour du territoire d'études dans le paramètre \code{fondContour}.
	
  \describe{
	\item{Ronds proportionnels}{
		L'interieur des cercles est de couleur orange (couleur par defaut) pour representer les valeurs positives de la variable en volume et bleu pour les valeurs negatives.
		
		Le parametre \code{fondChx} est a renseigner UNIQUEMENT si la maille est communale.
		
		Pour information, le chef-lieu (ou chx) est un point de la commune autour duquel la population est la plus dense. Le plus souvent, il s'agit de l'hotel de ville de la commune.
	}
	
	\item{Analyse en classes}{
		En semiologie, il est deconseille de multiplier le nombre de classes pour des raisons de visibilite et de clarte de la carte.
		
		Si les donnees ne comprennnent que des valeurs positives ou que des valeurs negatives, il est conseille 5 classes au maximum.
		
		Si les donnees comprennnent a la fois des valeurs positives et negatives, le nombre maximum conseille est 9 classes.
	}
	
	\item{Analyse en classes dans ronds proportionnels}{
		L'analyse en classes colore l'interieur des ronds (fonction \code{shiny_ronds_classes()})
	}
	
	\item{Ronds proportionnels sur analyse en classes}{
		Les ronds proportionnels sont poses sur l'analyse en classes (fonction \code{shiny_classes_ronds()})
	}
	
	\item{Typologie}{
		Meme conseil que pour les analyses en classes. Le choix des couleurs ne doit pas etre un degrade si il n'existe pas de classement dans les modalites.
	}
	
	\item{Oursins}{
		Les oursins representent uniquement l'existence d'un flux entre 2 territoires. Le sens du flux et le volume ne sont pas pris en compte.
		
		Les filtres sur le volume, la distance et les flux majeurs sont independants. Chacun d'eux s'appliquent sur les donnees initiales. Autrement dit, le filtre sur les flux majeurs ne se fera pas apres un eventuel filtre sur la distance mais sur toutes les donnees en entree.
		
		Le filtre sur le volume est plutot complementaire au filtre sur les flux majeurs. En effet, il est possible de vouloir representer des faibles flux significatifs. Le filtre sur les flux majeurs le permet contrairement au filtre sur le volume seul.
	}
	
	\item{Fleches joignantes}{
		Les fleches joignantes representent un flux entre 2 territoires, son sens et son volume. Dans le cas de flux aller et retour, deux fleches de sens oppose sont superposees.
		
		La regle des filtres est la meme que pour les oursins.
	}
	
	\item{Fleches saphirs}{
		Les fleches saphirs convergent vers ou divergent d'un meme territoire. Elles permettent de representer les entrees, les sorties ou le solde (entrees - sorties) d'un territoire par rapport aux autres.
		
		Pour un solde positif, les fleches sont representees en orange, pour un solde negatif en bleu.
		
		Une attention particuliere est a apporter au choix de la variable idDataDepart et idDataArrivee selon si le flux est entrant, sortant ou en solde.
	}
}
}
\value{
	Ne retourne aucun objet.
	
	Ouvre une fenetre avec un environnement R-Shiny dans le navigateur.
}
\seealso{
	\code{\link{leaflet_ronds}, \link{leaflet_classes}, \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds}, \link{leaflet_typo},}
	
	\code{\link{leaflet_oursins}, \link{leaflet_joignantes}, \link{leaflet_saphirs},}
	
	\code{\link{export_qgis_ronds}, \link{export_qgis_classes}, \link{export_qgis_ronds_classes}, \link{export_qgis_classes_ronds},}
	
	\code{\link{export_qgis_typo},}
	
	\code{\link{export_qgis_oursins}, \link{export_qgis_joignantes}, \link{export_qgis_saphirs}}
}
\examples{

data("depm")
data("regm")
data("fram")

data("donnees_monoloc")

if(interactive()){
# Ronds proportionnels
shiny_ronds(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
idData = "COD_DEP", varVolume = "POP_2015")

# Analyse en classes
shiny_classes(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
idData = "COD_DEP", varRatio = "VAR_AN_MOY")

# Analyse en classes dans les ronds proportionnels
shiny_ronds_classes(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY")

# Ronds proportionnels sur une analyse en classes
shiny_classes_ronds(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY")

 # Typologie
shiny_typo(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
idData = "COD_DEP", varTypo = "REG")
}

data("donnees_biloc")

if(interactive()){
# Oursins
shiny_oursins(data = donnees_biloc, fondMaille = regm, fondContour = fram,
idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR")

# Fleches joignantes
shiny_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
fondContour = fram, idDataDepart = "REG_DEPART",
idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR")
}

data("donnees_biloc_saphirs")

if(interactive()){
# Fleches saphirs
shiny_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG",
fondContour = fram, idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
varFlux = "MIGR", direction = "Ent")
}
}
\keyword{documentation}
