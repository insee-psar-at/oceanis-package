\name{leaflet}
\alias{leaflet_ronds}
\alias{leaflet_classes}
\alias{leaflet_ronds_classes}
\alias{leaflet_classes_ronds}
\alias{leaflet_typo}
\alias{leaflet_oursins}
\alias{leaflet_joignantes}
\alias{leaflet_saphirs}
\title{
	Creation 'leaflet' maps
}
\description{
	Create interactive maps for statistical analysis by 'leaflet' (zoom + pop-up).
}
\usage{
leaflet_ronds(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL, idData,
varVolume, rayonRond = NULL, rapportRond = NULL, dom = "0", fondChx = NULL)

leaflet_classes(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL, idData,
varRatio, methode = "kmeans", nbClasses = 3, bornes = NULL,
precision = 1, dom = "0")

leaflet_ronds_classes(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL,
idData, varVolume, varRatio, rayonRond = NULL,
rapportRond = NULL, methode = "kmeans", nbClasses = 3,
bornes = NULL, precision = 1,  dom = "0", fondChx = NULL)

leaflet_classes_ronds(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL,
idData, varVolume, varRatio, rayonRond = NULL,
rapportRond = NULL, methode = "kmeans", nbClasses = 3,
bornes = NULL, precision = 1, dom = "0", fondChx = NULL)

leaflet_typo(data, fondMaille, fondSuppl = NULL, idData, varTypo, dom = "0")

leaflet_oursins(data, fondMaille, fondSuppl = NULL, idDataDepart, idDataArrivee,
varFlux, filtreVol = 0, filtreDist = 100, filtreMajeurs = 10,
decalageAllerRetour = 0, decalageCentroid = 0,
dom = "0")

leaflet_joignantes(data, fondMaille, typeMaille, fondSuppl = NULL, idDataDepart,
idDataArrivee, varFlux, largeurFlecheMax = NULL, filtreVol = 0,
filtreDist = 100, filtreMajeurs = 10, decalageAllerRetour = 0,
decalageCentroid = 0, dom = "0")

leaflet_saphirs(data, fondMaille, typeMaille, fondSuppl = NULL, idDataDepart,
idDataArrivee, varFlux, largeurFlecheMax = NULL, direction = "Ent",
filtreVol = 0, dom = "0")
}
\arguments{
  \item{data}{
	tableau de donnees (data.frame).
}
  \item{fondMaille}{
	objet sf. Fond de carte.
}
  \item{typeMaille}{
	chaine de caracteres (character). Type de maille issus d'un zonage administratifs ou d'etudes ("REG", "DEP", "UU", "AU", "ZE", "BV", "EPCI" ou "COM").
}
  \item{fondMailleElargi}{
	objet sf. Fond de carte. Par defaut a NULL.
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
	chaine de caracteres (character). Variable en volume pour des ronds proportionnels.
}
  \item{varRatio}{
	chaine de caracteres (character). Variable en ratio pour des analyses en classes.
}
  \item{varTypo}{
	chaine de caracteres (character). Variable de typologie.
}
  \item{varFlux}{
	chaine de caracteres (character). Variable de flux pour des oursins, fleches joignantes ou fleches saphirs.
}
  \item{rayonRond}{
	valeur numerique (numeric). Rayon du rond le plus grand en metres. Par defaut a NULL.
}
  \item{rapportRond}{
	valeur numerique (numeric). Rapport entre l'aire du rond le plus grand et la valeur maximale des donnees en volume. Par defaut a NULL.
}
  \item{methode}{
	chaine de caracteres (character). A choisir parmi "kmeans" (par defaut), "fisher", "jenks" ou "quantile".
}
  \item{nbClasses}{
	valeur numerique (numeric). Nombre de classes. Par defaut 3 classes.
}
  \item{bornes}{
	vecteur de valeurs numeriques (numeric). Le nombre de bornes correspond au nombre de classes -1. Par defaut a NULL.
}
  \item{precision}{
	valeur numerique (numeric). Arrondi des valeurs en ratio pour le calcul des bornes de classes. Par defaut, 1 chiffre apres la virgule.
}
  \item{largeurFlecheMax}{
	valeur numerique (numeric). Valeur correspondant a la largeur de la fleche la plus grande de la carte (km). Par defaut a NULL.
}
  \item{direction}{
	chaine de caracteres (character). Pour les fleches saphirs, type de flux a choisir parmi "Ent" pour entrees (par defaut), "Sor" pour sorties et "Sol" pour soldes (entrees - sorties).
}
  \item{filtreVol}{
	valeur numerique (numeric). Filtre sur la valeur minimale du nombre de flux a afficher. Par defaut a 0 : affichage de tous les flux.
}
  \item{filtreDist}{
	valeur numerique (numeric). Filtre sur la distance maximale des fleches pour les oursins et les fleches joignantes. Par defaut a 100 km : affichage des flux a moins de 100 km.
}
  \item{filtreMajeurs}{
	valeur numerique (numeric). Filtre sur le nombre de flux entrants et sortants les plus importants d'un territoire. Par defaut a 10 : les 10 flux les plus importants sortant d'un territoire ou y entrant.
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
	objet sf. Fond des chefs-lieux. Pour les ronds proportionnels avec une maille communale, choix entre la position du centre des ronds sur les centroides des communes (NULL par defaut) ou sur les chefs-lieux (chx) des communes.
}
}
\details{
	Les cartes leaflet sont interactives, il est alors possible :
	
	- de deplacer la carte avec un cliquer-glisser de la souris.
	
	- de zoomer/dezoomer la carte avec la roulette de la souris ou les boutons +/- en haut a gauche de la carte. Le niveau de zoom peut varier de l'echelle de la France metropolitaine a l'echelle communale.
	
	- de faire apparaitre des informations en cliquant sur des territoires. En cliquant, par exemple, sur une maille d'une analyse en classes, un pop-up s'affiche indiquant le nom du territoire et sa valeur en ratio. En cliquant ailleurs, le 1er pop-up est remplace par un nouveau. On ferme le pop-up en cliquant sur sa croix.
	
	- d'ajouter une legende, des elements contextuels et de modifier son apparence via les fonctions oceanis associees.
	
	Les fonctions leaflet_() retournent un objet de type leaflet. Si vous executez une fonction leaflet_() sans recuperer le resultat dans une variable, la carte s'affichera automatiquement dans la fenetre viewer mais ne pourra pas etre reutilisee pour etre modifiee.
	Si vous sauvegardez le resultat dans une variable, la carte ne s'affichera qu'a l'execution de cette variable (voir exemples).
	
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
		
		Il est possible de specifier manuellement les bornes des classes en implementant le parametre \code{bornes} d'un vecteur de valeurs numeriques de type c(bornes1,bornes2,...). Le nombre de bornes correspond au nombre de classes -1 \code{(length(bornes)==nbClasses-1)}. Il n'est pas obligatoire de specifier les valeurs min et max.
	}
	
	\item{Analyse en classes dans ronds proportionnels}{
		L'analyse en classes colore l'interieur des ronds (fonction \code{leaflet_ronds_classes()})
	}
	
	\item{Ronds proportionnels sur analyse en classes}{
		Les ronds proportionnels sont poses sur l'analyse en classes (fonction \code{leaflet_classes_ronds()})
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
	Retourne un objet leaflet.
}
\seealso{
	\code{\link{zonage_a_facon},}
	
	\code{\link{rayon_ronds}, \link{rapport_ronds}, \link{largeur_fleche},}
	
	\code{\link{calcul_ratio}, \link{calcul_tx_evol_global}, \link{calcul_tx_evol_ann_moy}, \link{calcul_part_ens}, \link{calculette},}
	
	\code{\link{distrib_variable},}
	
	\code{\link{add_legende_ronds}, \link{add_legende_classes}, \link{add_legende_typo},}
	
	\code{\link{add_legende_joignantes}, \link{add_legende_saphirs},}
	
	\code{\link{coord_legende},}
	
	\code{\link{add_source}, \link{add_titre}, \link{add_fond_osm},}
	
	\code{\link{export_qgis_ronds}, \link{export_qgis_classes}, \link{export_qgis_ronds_classes}, \link{export_qgis_classes_ronds},}
	
	\code{\link{export_qgis_typo}, \link{export_qgis_oursins}, \link{export_qgis_joignantes}, \link{export_qgis_saphirs},}
	
	\code{\link{export_jpeg}, \link{export_pdf}, \link{export_png}}
 }
\examples{
data("depm")
data("regm")

data("donnees_monoloc")

\donttest{
# Ronds proportionnels
map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015")

 map

# Analyse en classes
map <- leaflet_classes(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varRatio = "VAR_AN_MOY", nbClasses = 4)

 map
}

# Analyse en classes dans les ronds proportionnels
map <- leaflet_ronds_classes(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
\donttest{
 map
}

\donttest{
# Ronds proportionnels sur une analyse en classes
map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)

 map

 # Typologie
map <- leaflet_typo(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
varTypo = "REG")

 map

data("donnees_biloc")

# Oursins
map <- leaflet_oursins(data = donnees_biloc, fondMaille = regm,
idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR",
filtreDist = 1000, filtreMajeurs = 3)

 map

# Fleches joignantes
map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm,
typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
varFlux = "MIGR", filtreDist = 1000, filtreMajeurs = 3)

 map
}

data("donnees_biloc_saphirs")

# Fleches saphirs
map <- leaflet_saphirs(data = donnees_biloc_saphirs, fondMaille = regm,
typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
varFlux = "MIGR", largeurFlecheMax = 80, direction = "Ent")
\donttest{
 map
}
}
\keyword{documentation}
