\name{leaflet}
\alias{leaflet_fonds_simples}
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
leaflet_fonds_simples(listFonds, popup = NULL, init = TRUE, map = NULL)

leaflet_ronds(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL, idData,
varVolume, rayonRond = NULL, rapportRond = NULL, emprise = "FRM", fondEtranger = NULL,
fondChx = NULL, colPos = "#CD853F", colNeg = "#6495ED", colBorderPos = "white",
colBorderNeg = "white", epaisseurBorder = 1, opacityElargi = 0.6, zoomMaille = NULL,
map_proxy = NULL)

leaflet_classes(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL, idData,
varRatio, methode = "kmeans", nbClasses = 3, bornes = NULL, stylePalette = "defaut",
opacityElargi = 0.6, colBorder = "white", precision = 1, emprise = "FRM",
fondEtranger = NULL, zoomMaille = NULL, map_proxy = NULL)

leaflet_ronds_classes(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL,
idData, varVolume, varRatio, rayonRond = NULL,
rapportRond = NULL, methode = "kmeans", nbClasses = 3,
bornes = NULL, stylePalette = "defaut", opacityElargi = 0.6, colBorderPos = "white",
colBorderNeg = "white", epaisseurBorder = 1, precision = 1, emprise = "FRM",
fondEtranger = NULL, fondChx = NULL, zoomMaille = NULL, map_proxy = NULL)

leaflet_classes_ronds(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL,
idData, varVolume, varRatio, rayonRond = NULL,
rapportRond = NULL, methode = "kmeans", nbClasses = 3,
bornes = NULL, stylePalette = "defaut", opacityElargi = 0.6,
colBorderClasses = "white", colBorderRondsPos = "#303030",
colBorderRondsNeg = "#303030", epaisseurBorder = 1.5, precision = 1, emprise = "FRM",
fondEtranger = NULL, fondChx = NULL, zoomMaille = NULL, map_proxy = NULL)

leaflet_typo(data, fondMaille, fondSuppl = NULL, idData, varTypo, emprise = "FRM",
fondEtranger = NULL, zoomMaille = NULL, map_proxy = NULL)

leaflet_oursins(data, fondMaille, fondSuppl = NULL, idDataDepart, idDataArrivee,
varFlux, filtreVol = 0, filtreDist = 100, filtreMajeurs = 10,
decalageAllerRetour = 0, decalageCentroid = 0, emprise = "FRM", fondEtranger = NULL,
zoomMaille = NULL, map_proxy = NULL)

leaflet_joignantes(data, fondMaille, typeMaille, fondSuppl = NULL, idDataDepart,
idDataArrivee, varFlux, largeurFlecheMax = NULL, filtreVol = 0,
filtreDist = 100, filtreMajeurs = 10, decalageAllerRetour = 0,
decalageCentroid = 0, colFleche = "#CD853F", colBorder = "#303030", emprise = "FRM",
fondEtranger = NULL, zoomMaille = NULL, map_proxy = NULL)

leaflet_saphirs(data, fondMaille, typeMaille, fondSuppl = NULL, idDataDepart,
idDataArrivee, varFlux, largeurFlecheMax = NULL, direction = "Ent",
filtreVol = 0, colEntree = "#CD853F", colSortie = "#6495ED",
colBorder = "#303030", emprise = "FRM", fondEtranger = NULL, zoomMaille = NULL,
map_proxy = NULL)
}
\arguments{
  \item{data}{
	tableau de donnees (data.frame).
}
  \item{fondMaille}{
	objet sf. Fond de carte.
}
  \item{listFonds}{
	list d'objets sf. Liste de fonds de carte.
}
  \item{popup}{
	vecteur de numeriques (numeric). Choix des couches avec les popup actifs. Par defaut a NULL (toutes les couches ont les popup actifs).
}
  \item{init}{
	booleen. TRUE pour creer une carte avec des fonds d'habillage. FALSE pour des fonds d'analyses. Par defaut a TRUE.
}
  \item{map}{
	objet leaflet. Permet d'ajouter des couches a une carte existante. Par defaut a NULL.
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
  \item{colPos}{
	chaine de caracteres (character). Couleur nommee (par exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut "#CD853F".
}
  \item{colNeg}{
	chaine de caracteres (character). Couleur nommee (par exemple "blue") ou hexadecimal (par exemple "#0000FF"). Par defaut "#6495ED".
}
  \item{colBorder}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white" pour les classes, "#303030" pour les fleches.
}
  \item{colBorderPos}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
}
  \item{colBorderNeg}{
	chaine de caracteres (character). Couleur nommee (par exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
}
  \item{colBorderRondsPos}{
	chaine de caracteres (character). Couleur nommee (par exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut "#303030".
}
  \item{colBorderRondsNeg}{
	chaine de caracteres (character). Couleur nommee (par exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut "#303030".
}
  \item{colBorderClasses}{
	chaine de caracteres (character). Couleur nommee (par exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut "white".
}
  \item{epaisseurBorder}{
	chaine de caracteres (numeric). Epaisseur de la bordure des ronds. Par defaut a 1".
}
  \item{stylePalette}{
	chaine de caracteres (character). Palette de la charte INSEE. A choisir entre "InseeFlash", "InseeAnalyse", "InseeDossier", "InseePremiere" ou "defaut" (par defaut).
}
  \item{colFleche}{
	chaine de caracteres (character). Couleur nommee (par exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut "#CD853F".
}
  \item{colEntree}{
	chaine de caracteres (character). Couleur nommee (par exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut "#CD853F".
}
  \item{colSortie}{
	chaine de caracteres (character). Couleur nommee (par exemple "blue") ou hexadecimal (par exemple "#0000FF"). Par defaut "#6495ED".
}
  \item{opacityElargi}{
	valeur numerique (numeric). Opacite de la representation elargie. Par defaut a 0.6 (valeur comprise entre 0-transparent et 1-opaque).
}
  \item{emprise}{
	chaine de caracteres (character). "FRM" (par defaut pour la France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973" (Guyane), "974" (La Reunion), "976" (Mayotte) ou "999" (Etranger). Ce parametre permet de definir le systeme de projection de la carte.
}
  \item{fondEtranger}{
	objet sf. Fond de carte d'habillage personnalisable pour les fonds autres que la France (metropolitaine et DOM). Par defaut a NULL.
}
  \item{fondChx}{
	objet sf. Fond des chefs-lieux. Pour les ronds proportionnels avec une maille communale, choix entre la position du centre des ronds sur les centroides des communes (NULL par defaut) ou sur les chefs-lieux (chx) des communes.
}
  \item{zoomMaille}{
	vecteur de caracteres ou numeriques (character ou numeric). Identifiant(s) de la maille du parametre fondMaille. Permet de zoomer sur une ou plusieurs entités de la maille. Par defaut a NULL (zoom sur l'emprise de la maille).
}
  \item{map_proxy}{
	objet leaflet ou leaflet_proxy. Pour l'integration des fonctions leaflet dans les applications shiny (cf vignette). Par defaut a NULL.
}
}
\details{
	Les cartes leaflet sont interactives, il est alors possible :
	
	- de deplacer la carte avec un cliquer-glisser de la souris.
	
	- de zoomer/dezoomer la carte avec la roulette de la souris ou les boutons +/- en haut a gauche de la carte. Le niveau de zoom peut varier de l'echelle de la France metropolitaine a l'echelle communale.
	
	- de faire apparaitre des informations en cliquant sur des territoires. En cliquant, par exemple, sur une maille d'une analyse en classes, un pop-up s'affiche indiquant le nom du territoire et sa valeur en ratio. En cliquant ailleurs, le 1er pop-up est remplace par un nouveau. On ferme le pop-up en cliquant sur sa croix.
	
	- d'ajouter une legende, des elements contextuels et de modifier son apparence via les fonctions oceanis associees.
	
	Les fonctions leaflet_() retournent un objet de type leaflet. Si vous executez une fonction leaflet_() sans recuperer le resultat dans une variable, la carte s'affichera automatiquement dans la fenetre viewer de RStudio mais ne pourra pas etre reutilisee pour etre modifiee.
	Si vous sauvegardez le resultat dans une variable, la carte ne s'affichera qu'a l'execution de cette variable (voir exemples).

	Il est possible de remplacer le fond d'habillage par defaut (France metropolitaine ou DOM) par un autre fond. Pour cela, il faut specifier le parametre emprise="999" ET ajouter un fond sf dans le parametre fondEtranger, par exemple, une carte de l'Europe ou d'un pays particulier.
	Le systeme de coordonnees du fond doit etre projete (en unite de longueur) et non geographique (lng/lat).
	Si aucun code EPSG valide n'est trouve pour ce fond, le code EPSG 3395 est choisi (projection Mercator). Attention car la legende des ronds sera d'autant plus deformee qu'elle se situera loin de l'equateur.
	
	Les fonctions leaflet peuvent etre integrees dans les applications shiny. Le parametre map_proxy permet d'actualiser uniquement les couches modifiees et non toute la carte entiere (voir vignette).
	
  \describe{
  	\item{Fonds simples}{
		Il s'agit d'une carte sans analyse avec uniquement des fonds. Le premier fond de la liste est positionne derriere la carte et le dernier fond de la liste devant. La personnalisation des fonds se fait avec la fonction \code{set_fonds_simples}.
	}
	
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
	
	\code{\link{add_legende_ronds}, \link{add_legende_classes}, \link{add_legende_typo}, \link{add_legende_typo_symboles},}
	
	\code{\link{add_legende_joignantes}, \link{add_legende_saphirs},}
	
	\code{\link{coord_legende},}
	
	\code{\link{add_source}, \link{add_titre}, \link{add_fond_osm},}
	
	\code{\link{set_fonds_simples},}
	
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
