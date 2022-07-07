#' @name add_legende_
#'
#' @title Add a legend on a 'leaflet' map
#'
#' @description Add a legend on a 'leaflet' map for proportional circles, chroropleth,
#' typology, proportional and sapphire arrows.
#'
#' @details Par defaut, \code{lng} et \code{lat} sont NULL ; la legende est alors
#' positionnee en haut a droite de la carte sauf pour l'analyse en classes ou
#' elle est positionnee en bas.
#'
#' Les valeurs \code{lng} et \code{lat} peuvent etre negatives. Une longitude a
#' 0 correspond au meridien de Greenwich.
#'
#' Pour deplacer la legende, reexecutez la fonction add_legende_xxx en
#' specifiant les parametres \code{lng} et \code{lat}.
#'
#' Pour supprimer la legende, reexecutez la fonction add_legende_xxx en
#' laissant les parametres \code{lng} et \code{lat} a NULL.
#'
#' Pour supprimer le titre, reexecutez la fonction add_legende_xxx en laissant
#' le parametre \code{titre} a NULL.
#'
#' Concernant la legende des ronds proportionnels, le grand cercle correspond a
#' la valeur max en volume et le petit cercle au tiers de la valeur max.
#'
#' Idem pour la largeur des fleches joignantes et des fleches saphirs.
#'
#' Les valeurs de la legende des ronds et des fleches peuvent etre arrondies
#' grace au parametre \code{precision}.
#'
#' Pour la légende des cartes en typologie, il est possible de specifier ses
#' propres labels sans avoir besoin de modifier les noms de variables dans la
#' table de donnees.
#'
#' Si le titre et les valeurs de legende sont trop decales, il faut modifier la
#' valeur du zoom : entre 6 (zoom maximal, niveau commune) et 10 (zoom minimal,
#' niveau France metro)
#'
#' @aliases add_legende_fonds_simples add_legende_ronds add_legende_classes
#' add_legende_typo add_legende_typo_symboles add_legende_joignantes
#' add_legende_saphirs
#'
#' @usage add_legende_fonds_simples(map, titre = NULL, lng = NULL, lat = NULL,
#' labels = NULL, choixLeg = NULL, zoom = 8, map_leaflet = NULL)
#'
#' add_legende_ronds(map, titre = NULL, lng = NULL, lat = NULL, precision = 0,
#' zoom = 8, map_leaflet = NULL)
#'
#' add_legende_classes(map, titre = NULL, lng = NULL, lat = NULL, typeLegende =
#' 1, zoom = 8, map_leaflet = NULL)
#'
#' add_legende_typo(map, titre = NULL, lng = NULL, lat = NULL, labels = NULL,
#' zoom = 8, map_leaflet = NULL)
#'
#' add_legende_typo_symboles(map, titre = NULL, lng = NULL, lat = NULL, labels
#' = NULL, zoom = 8, map_leaflet = NULL)
#'
#' add_legende_joignantes(map, titre = NULL, lng = NULL, lat = NULL, precision
#' = 0, zoom = 8, map_leaflet = NULL)
#'
#' add_legende_saphirs(map, titre = NULL, lng = NULL, lat = NULL, precision =
#' 0, zoom = 8, map_leaflet = NULL)
#'
#' @param map objet leaflet.
#' @param titre chaine de caracteres (character). Titre de la legende des
#' classes. Par defaut a NULL.
#' @param lng valeur numerique (numeric). Longitude (x) de la legende des
#' classes dans le systeme de projection WGS84 (code EPSG 4326). Par defaut a
#' NULL.
#' @param lat valeur numerique (numeric). Latitude (y) de la legende des
#' classes dans le systeme de projection WGS84 (code EPSG 4326). Par defaut a
#' NULL.
#' @param typeLegende valeur numerique (numeric). 1 (par defaut) pour une
#' legende litterale, 2 pour une legende en echelle.
#' @param labels vecteur de caracteres (character). Labels personnalises pour
#' la legende de la carte en typologie. Par defaut a NULL.
#' @param choixLeg vecteur de numeriques (numeric). Choix des couches a
#' afficher dans la legende. Par defaut a NULL (toutes les couches presentes).
#' @param precision valeur numerique (numeric). Arrondit les valeurs de la
#' legende des ronds ou des fleches. 0 (par defaut) pour arrondir a l'unite, -1
#' pour arrondir a la dizaine, -2 a la centaine...
#' @param zoom valeur numerique (numeric). Valeur entre 6 et 10 (8 par defaut).
#' Definit le niveau de zoom de la carte pour un affichage optimal.
#' @param map_leaflet objet leaflet. Pour l'integration des fonctions leaflet
#' dans les applications shiny (cf vignette). Par defaut a NULL.
#'
#' @return Retourne un objet de type leaflet.
#'
#' @seealso \code{\link{coord_legende}}
#'
#' \code{\link{leaflet_fonds_simples}}
#'
#' \code{\link{leaflet_ronds} \link{leaflet_classes}
#' \link{leaflet_ronds_classes} \link{leaflet_classes_ronds}
#' \link{leaflet_typo}}
#'
#' \code{\link{leaflet_oursins} \link{leaflet_joignantes}
#' \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#' data("regm")
#'
#' data("donnees_monoloc")
#'
#' # Ronds proportionnels sur une analyse en classes
#' map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
#' map <- add_legende_classes(map = map, titre = "VAR_AN_MOY", lng = 9, lat = 48, zoom = 6)
#' map <- add_legende_ronds(map = map, titre = "POP_2015", lng = 9, lat = 50, zoom = 6)
#' \donttest{
#'  map
#' }
#'
#' data("donnees_biloc")
#'
#' # Fleches joignantes
#' map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
#' idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", filtreDist = 1000, filtreMajeurs = 3)
#' map <- add_legende_joignantes(map = map, titre = "MIGR")
#' \donttest{
#'  map
#' }
#'
#' @import leaflet stringr grDevices graphics
#'
#' @export add_legende_fonds_simples
#' @export add_legende_ronds
#' @export add_legende_classes
#' @export add_legende_typo
#' @export add_legende_typo_symboles
#' @export add_legende_joignantes
#' @export add_legende_saphirs
#'
NULL

#' @name calcul_
#'
#' @title Calculating a class variable
#'
#' @description Add to a data table a column whose values can be represented in a class
#' analysis (ratio, part, evolution ...).
#'
#' The calculation is based on volume data (from the population, for example).
#'
#' @details Les formules utilisees sont : \describe{
#' \item{ratio}{\code{(data[,var1]/data[,var2])*100}}
#' \item{tx_evol_global}{\code{((data[,var2]-data[,var1])/data[,var1])*100}}
#' \item{tx_evol_ann_moy}{\code{((data[,var2]/data[,var1])^(1/nbAnnees)-1)*100}}
#' \item{part_ens}{\code{(data[,var]/sum(data[,var],na.rm = TRUE))*100}} }
#'
#' @aliases calcul_ratio calcul_tx_evol_global calcul_tx_evol_ann_moy
#' calcul_part_ens calculette
#'
#' @usage calcul_ratio(data, var1, var2)
#'
#' calcul_tx_evol_global(data, var1, var2)
#'
#' calcul_tx_evol_ann_moy(data, var1, var2, nbAnnees)
#'
#' calcul_part_ens(data, var)
#'
#' calculette(data, formule = NULL)
#'
#' @param data tableau de donnees (data.frame).
#' @param var1 chaine de caracteres (character). Variable en volume de la
#' table.
#' @param var2 chaine de caracteres (character). Variable en volume de la
#' table.
#' @param nbAnnees nombre (numeric). Nombre d'annees appliquable au taux
#' d'evolution annuel moyen.
#' @param var chaine de caracteres (character). Variable en volume de la table.
#' @param formule chaine de caracteres (character). Formule libre pour le
#' calcul d'une variable de classes. Seuls les caracteres + - * / ^ ( ) sont
#' acceptes.
#'
#' @return Retourne un objet data.frame.
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#'
#' ratio <- calcul_ratio(data = donnees_monoloc, var1 = "POP_2010", var2 = "POP_2015")
#' teg <- calcul_tx_evol_global(data = donnees_monoloc, var1 = "POP_2010", var2 = "POP_2015")
#' team <- calcul_tx_evol_ann_moy(data = donnees_monoloc, var1 = "POP_2010", var2 = "POP_2015",
#' nbAnnees = 5)
#' part <- calcul_part_ens(data = donnees_monoloc, var = "POP_2015")
#'
#' @export calcul_ratio
#' @export calcul_tx_evol_global
#' @export calcul_tx_evol_ann_moy
#' @export calcul_part_ens
#' @export calculette
#'
NULL

#' @name export_
#'
#' @title Export a 'leaflet' map in image format
#'
#' @description Export a 'leaflet' map in image format (.jpeg, .pdf or .png).
#'
#' @details Attention, l'export en format image peut durer quelques minutes.
#'
#' @aliases export_jpeg export_pdf export_png
#'
#' @usage export_jpeg(map, chemin, nomFichier)
#'
#' export_pdf(map, chemin, nomFichier)
#'
#' export_png(map, chemin, nomFichier)
#'
#' @param map objet leaflet.
#' @param chemin chaine de caracteres (character). Chemin du dossier
#' d'exportation de la carte.
#' @param nomFichier chaine de caracteres (character). Nom du fichier en sortie
#' sans l'extension.
#'
#' @return Ne retourne aucun objet.
#'
#' Exporte la carte dans le format .jpeg, .pdf ou .png a l'emplacement
#' specifie.
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},}
#'
#' \code{\link{leaflet_typo}, \link{leaflet_oursins},
#' \link{leaflet_joignantes}, \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#' data("regm")
#'
#' data("donnees_monoloc")
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015")
#'
#' \donttest{
#'  ## Not run:
#'  export_jpeg(map = map, chemin = tempdir(), nomFichier = "map")
#'  export_pdf(map = map, chemin = tempdir(), nomFichier = "map")
#'  export_png(map = map, chemin = tempdir(), nomFichier = "map")
#'  ## End(Not run)
#' }
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#'
#' @export export_jpeg
#' @export export_pdf
#' @export export_png
#'
NULL

#' @name export_qgis_
#'
#' @title Export a 'leaflet' map to Qgis project
#'
#' @description Export a 'leaflet' map to Qgis project.
#'
#' @details Il faut obligatoirement ajouter la legende a la carte avant d'utiliser la
#' fonction d'export Qgis.
#'
#' Le projet .qgs est associe a tous les fonds ShapeFile qui composent la carte
#' (.shp, .dbf, .prj, .shx).
#'
#' Attention, l'export du projet Qgis peut durer quelques minutes.
#'
#' @aliases export_qgis_ronds export_qgis_classes export_qgis_ronds_classes
#' export_qgis_classes_ronds export_qgis_typo export_qgis_oursins
#' export_qgis_joignantes export_qgis_saphirs
#'
#' @usage export_qgis_ronds(map, cheminDossier, nomFichier, titre1 = "", titre2
#' = "", source = "")
#'
#' export_qgis_classes(map, cheminDossier, nomFichier, titre1 = "", titre2 =
#' "", source = "")
#'
#' export_qgis_ronds_classes(map, cheminDossier, nomFichier, titre1 = "",
#' titre2 = "", source = "")
#'
#' export_qgis_classes_ronds(map, cheminDossier, nomFichier, titre1 = "",
#' titre2 = "", source = "")
#'
#' export_qgis_typo(map, cheminDossier, nomFichier, titre1 = "", titre2 = "",
#' source = "")
#'
#' export_qgis_oursins(map, cheminDossier, nomFichier, titre1 = "", titre2 =
#' "", source = "")
#'
#' export_qgis_joignantes(map, cheminDossier, nomFichier, titre1 = "", titre2 =
#' "", source = "")
#'
#' export_qgis_saphirs(map, cheminDossier, nomFichier, titre1 = "", titre2 =
#' "", source = "")
#'
#' @param map objet leaflet.
#' @param cheminDossier chaine de caracteres (character). Chemin du dossier
#' d'exportation du projet.
#' @param nomFichier chaine de caracteres (character). Nom du projet en sortie
#' sans l'extension.
#' @param titre1 chaine de caracteres (character). Titre principal de la carte,
#' le plus souvent informatif.
#' @param titre2 chaine de caracteres (character). Titre secondaire de la
#' carte, le plus souvent descriptif.
#' @param source chaine de caracteres (character). Source de la carte.
#'
#' @return Ne retourne aucun objet.
#'
#' Exporte la carte en projet Qgis a l'emplacement specifie.
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},}
#'
#' \code{\link{leaflet_typo}, \link{leaflet_oursins},
#' \link{leaflet_joignantes}, \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#' data("regm")
#'
#' data("donnees_monoloc")
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015")
#' map <- add_legende_ronds(map = map, titre = "POP_2015", lng = 9, lat = 50, zoom = 6)
#'
#' \donttest{
#'  ## Not run:
#'  export_qgis_ronds(map = map, cheminDossier = tempdir(), nomFichier = "projet_qgis",
#'  source = "Source : INSEE - RP2016")
#'  ## End(Not run)
#' }
#'
#' @import leaflet sf stringr
#' @importFrom utils write.csv
#'
#' @export export_qgis_ronds
#' @export export_qgis_classes
#' @export export_qgis_ronds_classes
#' @export export_qgis_classes_ronds
#' @export export_qgis_typo
#' @export export_qgis_oursins
#' @export export_qgis_joignantes
#' @export export_qgis_saphirs
#'
NULL

#' @name leaflet_
#'
#' @title Creation 'leaflet' maps
#'
#' @description Create interactive maps for statistical analysis by 'leaflet' (zoom +
#' pop-up).
#'
#' @details Les cartes leaflet sont interactives, il est alors possible :
#'
#' - de deplacer la carte avec un cliquer-glisser de la souris.
#'
#' - de zoomer/dezoomer la carte avec la roulette de la souris ou les boutons
#' +/- en haut a gauche de la carte. Le niveau de zoom peut varier de l'echelle
#' de la France metropolitaine a l'echelle communale.
#'
#' - de faire apparaitre des informations en cliquant sur des territoires. En
#' cliquant, par exemple, sur une maille d'une analyse en classes, un pop-up
#' s'affiche indiquant le nom du territoire et sa valeur en ratio. En cliquant
#' ailleurs, le 1er pop-up est remplace par un nouveau. On ferme le pop-up en
#' cliquant sur sa croix.
#'
#' - d'ajouter une legende, des elements contextuels et de modifier son
#' apparence via les fonctions oceanis associees.
#'
#' Les fonctions leaflet_() retournent un objet de type leaflet. Si vous
#' executez une fonction leaflet_() sans recuperer le resultat dans une
#' variable, la carte s'affichera automatiquement dans la fenetre viewer de
#' RStudio mais ne pourra pas etre reutilisee pour etre modifiee.  Si vous
#' sauvegardez le resultat dans une variable, la carte ne s'affichera qu'a
#' l'execution de cette variable (voir exemples).
#'
#' Il est possible de remplacer le fond d'habillage par defaut (France
#' metropolitaine ou DOM) par un autre fond. Pour cela, il faut specifier le
#' parametre emprise="999" ET ajouter un fond sf dans le parametre
#' fondEtranger, par exemple, une carte de l'Europe ou d'un pays particulier.
#' Le systeme de coordonnees du fond doit etre projete (en unite de longueur)
#' et non geographique (lng/lat).  Si aucun code EPSG valide n'est trouve pour
#' ce fond, le code EPSG 3395 est choisi (projection Mercator). Attention car
#' la legende des ronds sera d'autant plus deformee qu'elle se situera loin de
#' l'equateur.
#'
#' Les fonctions leaflet peuvent etre integrees dans les applications shiny. Le
#' parametre map_proxy permet d'actualiser uniquement les couches modifiees et
#' non toute la carte entiere (voir vignette).
#'
#' \describe{ \item{Fonds simples}{ Il s'agit d'une carte sans analyse avec
#' uniquement des fonds. Le premier fond de la liste est positionne derriere la
#' carte et le dernier fond de la liste devant. La personnalisation des fonds
#' se fait avec la fonction \code{set_fonds_simples}.  }
#'
#' \item{Ronds proportionnels}{ L'interieur des cercles est de couleur orange
#' (couleur par defaut) pour representer les valeurs positives de la variable
#' en volume et bleu pour les valeurs negatives.
#'
#' Le parametre \code{fondChx} est a renseigner UNIQUEMENT si la maille est
#' communale.
#'
#' Pour information, le chef-lieu (ou chx) est un point de la commune autour
#' duquel la population est la plus dense. Le plus souvent, il s'agit de
#' l'hotel de ville de la commune.  }
#'
#' \item{Analyse en classes}{ En semiologie, il est deconseille de multiplier
#' le nombre de classes pour des raisons de visibilite et de clarte de la
#' carte.
#'
#' Si les donnees ne comprennnent que des valeurs positives ou que des valeurs
#' negatives, il est conseille 5 classes au maximum.
#'
#' Si les donnees comprennnent a la fois des valeurs positives et negatives, le
#' nombre maximum conseille est 9 classes.
#'
#' Il est possible de specifier manuellement les bornes des classes en
#' implementant le parametre \code{bornes} d'un vecteur de valeurs numeriques
#' de type c(bornes1,bornes2,...). Le nombre de bornes correspond au nombre de
#' classes -1 \code{(length(bornes)==nbClasses-1)}. Il n'est pas obligatoire de
#' specifier les valeurs min et max.  }
#' 
#' Les palettes de couleurs disponibles sont celles de la Charte Graphique
#' INSEE. En exécutant la fonction affiche_palette(nomPalette), il est
#' possible de visualiser les couleurs de chaque palette disponible. A choisir
#' entre "defaut", "Insee_Rouge", "Insee_Jaune",  "Insee_Bleu", "Insee_Turquoise",
#' "Insee_Vert", "Insee_Violet" ou "Insee_Gris".
#'
#' \item{Analyse en classes dans ronds proportionnels}{ L'analyse en classes
#' colore l'interieur des ronds (fonction \code{leaflet_ronds_classes()}) }
#'
#' \item{Ronds proportionnels sur analyse en classes}{ Les ronds proportionnels
#' sont poses sur l'analyse en classes (fonction
#' \code{leaflet_classes_ronds()}) }
#'
#' \item{Typologie}{ Meme conseil que pour les analyses en classes. Le choix
#' des couleurs ne doit pas etre un degrade si il n'existe pas de classement
#' dans les modalites.  }
#'
#' \item{Oursins}{ Les oursins representent uniquement l'existence d'un flux
#' entre 2 territoires. Le sens du flux et le volume ne sont pas pris en
#' compte.
#'
#' Les filtres sur le volume, la distance et les flux majeurs sont
#' independants. Chacun d'eux s'appliquent sur les donnees initiales. Autrement
#' dit, le filtre sur les flux majeurs ne se fera pas apres un eventuel filtre
#' sur la distance mais sur toutes les donnees en entree.
#'
#' Le filtre sur le volume est plutot complementaire au filtre sur les flux
#' majeurs. En effet, il est possible de vouloir representer des faibles flux
#' significatifs. Le filtre sur les flux majeurs le permet contrairement au
#' filtre sur le volume seul.  }
#'
#' \item{Fleches joignantes}{ Les fleches joignantes representent un flux entre
#' 2 territoires, son sens et son volume. Dans le cas de flux aller et retour,
#' deux fleches de sens oppose sont superposees.
#'
#' La regle des filtres est la meme que pour les oursins.  }
#'
#' \item{Fleches saphirs}{ Les fleches saphirs convergent vers ou divergent
#' d'un meme territoire. Elles permettent de representer les entrees, les
#' sorties ou le solde (entrees - sorties) d'un territoire par rapport aux
#' autres.
#'
#' Pour un solde positif, les fleches sont representees en orange, pour un
#' solde negatif en bleu.
#'
#' Une attention particuliere est a apporter au choix de la variable
#' idDataDepart et idDataArrivee selon si le flux est entrant, sortant ou en
#' solde.  } }
#'
#' @aliases leaflet_fonds_simples leaflet_ronds leaflet_classes
#' leaflet_ronds_classes leaflet_classes_ronds leaflet_typo leaflet_oursins
#' leaflet_joignantes leaflet_saphirs
#'
#' @usage leaflet_fonds_simples(listFonds, popup = NULL, init = TRUE, map =
#' NULL)
#'
#' leaflet_ronds(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL,
#' idData, varVolume, rayonRond = NULL, rapportRond = NULL, emprise = "FRM",
#' fondEtranger = NULL, fondChx = NULL, colPos = "#EB617F", colNeg = "#286AC7",
#' colBorderPos = "white", colBorderNeg = "white", epaisseurBorder = 1,
#' opacityElargi = 0.6, zoomMaille = NULL, map_proxy = NULL)
#'
#' leaflet_classes(data, fondMaille, fondMailleElargi = NULL, fondSuppl = NULL,
#' idData, varRatio, methode = "kmeans", nbClasses = 3, bornes = NULL,
#' stylePalette = "defaut", opacityElargi = 0.6, colBorder = "white", precision
#' = 1, emprise = "FRM", fondEtranger = NULL, zoomMaille = NULL, map_proxy =
#' NULL)
#'
#' leaflet_ronds_classes(data, fondMaille, fondMailleElargi = NULL, fondSuppl =
#' NULL, idData, varVolume, varRatio, rayonRond = NULL, rapportRond = NULL,
#' methode = "kmeans", nbClasses = 3, bornes = NULL, stylePalette = "defaut",
#' opacityElargi = 0.6, colBorderPos = "white", colBorderNeg = "white",
#' epaisseurBorder = 1, precision = 1, emprise = "FRM", fondEtranger = NULL,
#' fondChx = NULL, zoomMaille = NULL, map_proxy = NULL)
#'
#' leaflet_classes_ronds(data, fondMaille, fondMailleElargi = NULL, fondSuppl =
#' NULL, idData, varVolume, varRatio, rayonRond = NULL, rapportRond = NULL,
#' methode = "kmeans", nbClasses = 3, bornes = NULL, stylePalette = "defaut",
#' opacityElargi = 0.6, colBorderClasses = "white", colBorderRondsPos =
#' "#303030", colBorderRondsNeg = "#303030", epaisseurBorder = 1.5, precision =
#' 1, emprise = "FRM", fondEtranger = NULL, fondChx = NULL, zoomMaille = NULL,
#' map_proxy = NULL)
#'
#' leaflet_typo(data, fondMaille, fondSuppl = NULL, idData, varTypo, emprise =
#' "FRM", fondEtranger = NULL, zoomMaille = NULL, map_proxy = NULL)
#'
#' leaflet_oursins(data, fondMaille, fondSuppl = NULL, idDataDepart,
#' idDataArrivee, varFlux, filtreVol = 0, filtreDist = 100, filtreMajeurs = 10,
#' decalageAllerRetour = 0, decalageCentroid = 0, emprise = "FRM", fondEtranger
#' = NULL, zoomMaille = NULL, map_proxy = NULL)
#'
#' leaflet_joignantes(data, fondMaille, typeMaille, fondSuppl = NULL,
#' idDataDepart, idDataArrivee, varFlux, largeurFlecheMax = NULL, filtreVol =
#' 0, filtreDist = 100, filtreMajeurs = 10, decalageAllerRetour = 0,
#' decalageCentroid = 0, colFleche = "#286AC7", colBorder = "#303030", emprise
#' = "FRM", fondEtranger = NULL, zoomMaille = NULL, map_proxy = NULL)
#'
#' leaflet_saphirs(data, fondMaille, typeMaille, fondSuppl = NULL,
#' idDataDepart, idDataArrivee, varFlux, largeurFlecheMax = NULL, direction =
#' "Ent", filtreVol = 0, colEntree = "#EB617F", colSortie = "#286AC7",
#' colBorder = "#303030", emprise = "FRM", fondEtranger = NULL, zoomMaille =
#' NULL, map_proxy = NULL)
#'
#' @param data tableau de donnees (data.frame).
#' @param fondMaille objet sf. Fond de carte.
#' @param listFonds list d'objets sf. Liste de fonds de carte.
#' @param popup vecteur de numeriques (numeric). Choix des couches avec les
#' popup actifs. Par defaut a NULL (toutes les couches ont les popup actifs).
#' @param init booleen. TRUE pour creer une carte avec des fonds d'habillage.
#' FALSE pour des fonds d'analyses. Par defaut a TRUE.
#' @param map objet leaflet. Permet d'ajouter des couches a une carte
#' existante. Par defaut a NULL.
#' @param typeMaille chaine de caracteres (character). Type de maille issus
#' d'un zonage administratifs ou d'etudes ("REG", "DEP", "UU", "AU", "ZE",
#' "BV", "EPCI" ou "COM").
#' @param fondMailleElargi objet sf. Fond de carte. Par defaut a NULL.
#' @param fondSuppl objet sf. Fond de carte. Par defaut a NULL.
#' @param idData chaine de caracteres (character). Variable identifiant la
#' maille.
#' @param idDataDepart chaine de caracteres (character). Variable identifiant
#' le depart du flux.
#' @param idDataArrivee chaine de caracteres (character). Variable identifiant
#' l'arrivee du flux.
#' @param varVolume chaine de caracteres (character). Variable en volume pour
#' des ronds proportionnels.
#' @param varRatio chaine de caracteres (character). Variable en ratio pour des
#' analyses en classes.
#' @param varTypo chaine de caracteres (character). Variable de typologie.
#' @param varFlux chaine de caracteres (character). Variable de flux pour des
#' oursins, fleches joignantes ou fleches saphirs.
#' @param rayonRond valeur numerique (numeric). Rayon du rond le plus grand en
#' metres. Par defaut a NULL.
#' @param rapportRond valeur numerique (numeric). Rapport entre l'aire du rond
#' le plus grand et la valeur maximale des donnees en volume. Par defaut a
#' NULL.
#' @param methode chaine de caracteres (character). A choisir parmi "kmeans"
#' (par defaut), "fisher", "jenks" ou "quantile".
#' @param nbClasses valeur numerique (numeric). Nombre de classes. Par defaut 3
#' classes.
#' @param bornes vecteur de valeurs numeriques (numeric). Le nombre de bornes
#' correspond au nombre de classes -1. Par defaut a NULL.
#' @param precision valeur numerique (numeric). Arrondi des valeurs en ratio
#' pour le calcul des bornes de classes. Par defaut, 1 chiffre apres la
#' virgule.
#' @param largeurFlecheMax valeur numerique (numeric). Valeur correspondant a
#' la largeur de la fleche la plus grande de la carte (km). Par defaut a NULL.
#' @param direction chaine de caracteres (character). Pour les fleches saphirs,
#' type de flux a choisir parmi "Ent" pour entrees (par defaut), "Sor" pour
#' sorties et "Sol" pour soldes (entrees - sorties).
#' @param filtreVol valeur numerique (numeric). Filtre sur la valeur minimale
#' du nombre de flux a afficher. Par defaut a 0 : affichage de tous les flux.
#' @param filtreDist valeur numerique (numeric). Filtre sur la distance
#' maximale des fleches pour les oursins et les fleches joignantes. Par defaut
#' a 100 km : affichage des flux a moins de 100 km.
#' @param filtreMajeurs valeur numerique (numeric). Filtre sur le nombre de
#' flux entrants et sortants les plus importants d'un territoire. Par defaut a
#' 10 : les 10 flux les plus importants sortant d'un territoire ou y entrant.
#' @param decalageAllerRetour valeur numerique (numeric). Decalage entre les
#' fleches aller-retour (km).
#' @param decalageCentroid valeur numerique (numeric). Decalage des fleches
#' depuis et vers les centroides des territoires (km).
#' @param colPos chaine de caracteres (character). Couleur nommee (par exemple
#' "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut "#EB617F".
#' @param colNeg chaine de caracteres (character). Couleur nommee (par exemple
#' "blue") ou hexadecimal (par exemple "#0000FF"). Par defaut "#286AC7".
#' @param colBorder chaine de caracteres (character). Couleur nommee (par
#' exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white"
#' pour les classes, "#303030" pour les fleches.
#' @param colBorderPos chaine de caracteres (character). Couleur nommee (par
#' exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
#' @param colBorderNeg chaine de caracteres (character). Couleur nommee (par
#' exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
#' @param colBorderRondsPos chaine de caracteres (character). Couleur nommee
#' (par exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut
#' "#303030".
#' @param colBorderRondsNeg chaine de caracteres (character). Couleur nommee
#' (par exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut
#' "#303030".
#' @param colBorderClasses chaine de caracteres (character). Couleur nommee
#' (par exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut
#' "white".
#' @param epaisseurBorder chaine de caracteres (numeric). Epaisseur de la
#' bordure des ronds. Par defaut a 1".
#' @param stylePalette chaine de caracteres (character). Palette de la charte
#' INSEE. A choisir entre "defaut", "Insee_Rouge", "Insee_Jaune", "Insee_Bleu",
#' "Insee_Turquoise", "Insee_Vert", "Insee_Violet" ou "Insee_Gris"
#' (voir 'affiche_palette(nomPalette)').
#' @param colFleche chaine de caracteres (character). Couleur nommee (par
#' exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut
#' "#286AC7".
#' @param colEntree chaine de caracteres (character). Couleur nommee (par
#' exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut
#' "#EB617F".
#' @param colSortie chaine de caracteres (character). Couleur nommee (par
#' exemple "blue") ou hexadecimal (par exemple "#0000FF"). Par defaut
#' "#286AC7".
#' @param opacityElargi valeur numerique (numeric). Opacite de la
#' representation elargie. Par defaut a 0.6 (valeur comprise entre
#' 0-transparent et 1-opaque).
#' @param emprise chaine de caracteres (character). "FRM" (par defaut pour la
#' France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973"
#' (Guyane), "974" (La Reunion), "976" (Mayotte) ou "999" (Etranger). Ce
#' parametre permet de definir le systeme de projection de la carte.
#' @param fondEtranger objet sf. Fond de carte d'habillage personnalisable pour
#' les fonds autres que la France (metropolitaine et DOM). Par defaut a NULL.
#' @param fondChx objet sf. Fond des chefs-lieux. Pour les ronds proportionnels
#' avec une maille communale, choix entre la position du centre des ronds sur
#' les centroides des communes (NULL par defaut) ou sur les chefs-lieux (chx)
#' des communes.
#' @param zoomMaille vecteur de caracteres ou numeriques (character ou
#' numeric). Identifiant(s) de la maille du parametre fondMaille. Permet de
#' zoomer sur une ou plusieurs entités de la maille. Par defaut a NULL (zoom
#' sur l'emprise de la maille).
#' @param map_proxy objet leaflet ou leaflet_proxy. Pour l'integration des
#' fonctions leaflet dans les applications shiny (cf vignette). Par defaut a
#' NULL.
#'
#' @return Retourne un objet leaflet.
#'
#' @seealso \code{\link{zonage_a_facon},}
#'
#' \code{\link{rayon_ronds}, \link{rapport_ronds}, \link{largeur_fleche},}
#'
#' \code{\link{calcul_ratio}, \link{calcul_tx_evol_global},
#' \link{calcul_tx_evol_ann_moy}, \link{calcul_part_ens}, \link{calculette},}
#'
#' \code{\link{distrib_variable},}
#'
#' \code{\link{add_legende_ronds}, \link{add_legende_classes},
#' \link{add_legende_typo}, \link{add_legende_typo_symboles},}
#'
#' \code{\link{add_legende_joignantes}, \link{add_legende_saphirs},}
#'
#' \code{\link{coord_legende},}
#'
#' \code{\link{add_source}, \link{add_titre},}
#'
#' \code{\link{set_fonds_simples},}
#'
#' \code{\link{export_qgis_ronds}, \link{export_qgis_classes},
#' \link{export_qgis_ronds_classes}, \link{export_qgis_classes_ronds},}
#'
#' \code{\link{export_qgis_typo}, \link{export_qgis_oursins},
#' \link{export_qgis_joignantes}, \link{export_qgis_saphirs},}
#'
#' \code{\link{export_jpeg}, \link{export_pdf}, \link{export_png}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#' data("regm")
#'
#' data("donnees_monoloc")
#'
#' \donttest{
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015")
#'
#'  map
#'
#' # Analyse en classes
#' map <- leaflet_classes(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varRatio = "VAR_AN_MOY", nbClasses = 4)
#'
#'  map
#' }
#'
#' # Analyse en classes dans les ronds proportionnels
#' map <- leaflet_ronds_classes(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
#' \donttest{
#'  map
#' }
#'
#' \donttest{
#' # Ronds proportionnels sur une analyse en classes
#' map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
#'
#'  map
#'
#'  # Typologie
#' map <- leaflet_typo(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varTypo = "REG")
#'
#'  map
#'
#' data("donnees_biloc")
#'
#' # Oursins
#' map <- leaflet_oursins(data = donnees_biloc, fondMaille = regm,
#' idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR",
#' filtreDist = 1000, filtreMajeurs = 3)
#'
#'  map
#'
#' # Fleches joignantes
#' map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm,
#' typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", filtreDist = 1000, filtreMajeurs = 3)
#'
#'  map
#' }
#'
#' data("donnees_biloc_saphirs")
#'
#' # Fleches saphirs
#' map <- leaflet_saphirs(data = donnees_biloc_saphirs, fondMaille = regm,
#' typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", largeurFlecheMax = 80, direction = "Ent")
#' \donttest{
#'  map
#' }
#'
#' @import leaflet sf classInt leaflet.extras grDevices
#' @importFrom lwgeom st_geod_length
#' @rawNamespace import(shiny, except = runExample)
#'
#' @export leaflet_fonds_simples
#' @export leaflet_ronds
#' @export leaflet_classes
#' @export leaflet_ronds_classes
#' @export leaflet_classes_ronds
#' @export leaflet_typo
#' @export leaflet_oursins
#' @export leaflet_joignantes
#' @export leaflet_saphirs
#'
NULL

#' @name plot_
#'
#' @title Creation maps in plot
#'
#' @description Displays the map in a plot output.
#'
#' @details Les cartes en sortie "plot" ne sont pas interactives. Il faut eviter de
#' zoomer sur la carte au risque de voir apparaitre des decalages entre les
#' couches.
#'
#' Elles peuvent etre personnalisees grace aux parametres de style des
#' fonctions plot_().
#'
#' \describe{ \item{Ronds proportionnels}{ L'interieur des cercles est de
#' couleur orange (couleur par defaut) pour representer les valeurs positives
#' de la variable en volume et bleu pour les valeurs negatives.
#'
#' Le parametre \code{fondChx} est a renseigner UNIQUEMENT si la maille est
#' communale.
#'
#' Pour information, le chef-lieu (ou chx) est un point de la commune autour
#' duquel la population est la plus dense. Le plus souvent, il s'agit de
#' l'hotel de ville de la commune.  }
#'
#' \item{Analyse en classes}{ En semiologie, il est deconseille de multiplier
#' le nombre de classes pour des raisons de visibilite et de clarte de la
#' carte.
#'
#' Si les donnees ne comprennnent que des valeurs positives ou que des valeurs
#' negatives, il est conseille 5 classes au maximum.
#'
#' Si les donnees comprennnent a la fois des valeurs positives et negatives, le
#' nombre maximum conseille est 9 classes.
#'
#' Il est possible de specifier manuellement les bornes des classes en
#' implementant le parametre \code{bornes} d'un vecteur de valeurs numeriques
#' de type c(bornes1,bornes2,...). Le nombre de bornes correspond au nombre de
#' classes -1 \code{(length(bornes)==nbClasses-1)}. Il n'est pas obligatoire de
#' specifier les valeurs min et max.  }
#'
#' \item{Analyse en classes dans ronds proportionnels}{ L'analyse en classes
#' colore l'interieur des ronds : (fonction \code{plot_ronds_classes()}) }
#'
#' \item{Ronds proportionnels sur analyse en classes}{ Les ronds proportionnels
#' sont poses sur l'analyse en classes : (fonction \code{plot_classes_ronds()})
#' }
#'
#' \item{Typologie}{ Meme conseil que pour les analyses en classes. Le choix
#' des couleurs ne doit pas etre un degrade si il n'existe pas de classement
#' dans les modalites.
#'
#' Il est possible de specifier ses propres labels sans avoir besoin de
#' modifier les noms de variables dans la table de donnees.  }
#'
#' \item{Typologie en symboles}{ Les fonds de carte doivent etre passes en list
#' et peuvent etre personnalises (voir ci-dessous modifier l'apparence des
#' fonds).
#'
#' Les parametres types, couleurs, tailles et epaisseurs sont des vecteurs de
#' meme longueur. Chaque element correspond a un point du fond fourni. Verifier
#' bien l'ordre des points pour modifier leur apparence ensuite.
#'
#' Il existe differents types de points (voir l'aide de ?graphics::points).  }
#'
#' \item{Oursins}{ Les oursins representent uniquement l'existence d'un flux
#' entre 2 territoires. Le sens du flux et le volume ne sont pas pris en
#' compte.
#'
#' Les filtres sur le volume, la distance et les flux majeurs sont
#' independants. Chacun d'eux s'appliquent sur les donnees initiales. Autrement
#' dit, le filtre sur les flux majeurs ne se fera pas apres un eventuel filtre
#' sur la distance mais sur toutes les donnees en entree.
#'
#' Le filtre sur le volume est plutot complementaire au filtre sur les flux
#' majeurs. En effet, il est possible de vouloir representer des faibles flux
#' significatifs. Le filtre sur les flux majeurs le permet contrairement au
#' filtre sur le volume seul.  }
#'
#' \item{Fleches joignantes}{ Les fleches joignantes representent un flux entre
#' 2 territoires, son sens et son volume. Dans le cas de flux aller et retour,
#' deux fleches de sens oppose sont superposees.
#'
#' La regle des filtres est la meme que pour les oursins.  }
#'
#' \item{Fleches saphirs}{ Les fleches saphirs convergent vers ou divergent
#' d'un meme territoire. Elles permettent de representer les entrees, les
#' sorties ou le solde (entrees - sorties) d'un territoire par rapport aux
#' autres.
#'
#' Pour un solde positif, les fleches sont representees en orange, pour un
#' solde negatif en bleu.
#'
#' Une attention particuliere est a apporter au choix de la variable
#' idDataDepart et idDataArrivee selon si le flux est entrant, sortant ou en
#' solde.  } }
#'
#' La legende est positionnee par defaut sur la carte si les coordonnees x et y
#' ne sont pas specifiees. Elle est positionnee en haut a droite de la carte
#' sauf pour l'analyse en classes ou elle est positionnee en bas a droite.
#'
#' Les coordonnees x et y doivent etre dans le systeme de projection locale :
#' \itemize{ \item France metropolitaine : Lambert 93 (code epsg 2154) \item
#' Guadeloupe : UTM 20 N (code epsg 32620) \item Martinique : UTM 20 N (code
#' epsg 32620) \item Guyane : UTM 22 N (code epsg 2972) \item La Reunion : UTM
#' 40 S (code epsg 2975) \item Mayotte : UTM 38 S (code epsg 4471) }
#'
#' Il est possible d'ajouter des fonds de carte pour l'habiller. Il y existe 2
#' parametres : \code{fondSousAnalyse} et \code{fondSurAnalyse}.
#'
#' Le parametre \code{fondSousAnalyse} permet d'ajouter des fonds en-dessous de
#' l'analyse et la parametre \code{fondSurAnalyse} au-dessus.
#'
#' Chacun de ces parametres est une liste d'objets sf. Par defaut, la couleur
#' de remplissage est transparente, la bordure est noire et l'epaisseur est de
#' 1.
#'
#' Pour modifier l'apparence de ces fonds, il faut ajouter des colonnes dans
#' les objets sf correspondants :
#'
#' \itemize{ \item COL : un colonne COL pour modifier la couleur de
#' remplissage.  \item BORDER : une colonne BORDER pour modifier la couleur de
#' la bordure.  \item EPAISSEUR : une colonne EPAISSEUR pour modifier
#' l'epaisseur de la bordure.  }
#'
#' Pour ajouter des colonnes un objet sf, il faut utiliser la fonction
#' \code{cbind}. Exemple : paysf <- cbind(paysf, COL="grey", BORDER="#404040",
#' EPAISSEUR=2)
#'
#' \describe{ \item{etiquettes}{ Des etiquettes peuvent etre affichees sur les
#' cartes en plot grace au parametre "etiquettes". Comment proceder ?
#'
#' - Specifier un vecteur de codes des territoires a etiqueter appartenant a la
#' maille. La fonction recupere les libelles a afficher. Les etiquettes se
#' placent au centroide des territoires, reperes par leurs coordonnees x et y.
#' Une mise en forme des etiquettes par defaut est proposee.
#'
#' - Pour formater les etiquettes, il est utile de passer par la fonction
#' \code{\link{coordonnees_etiquettes}}. A partir d'un fond de maille et d'un
#' vecteur de codes, un data.frame est produit en sortie de la fonction.
#'
#' Ce tableau contient le code du territoire, le libelle a afficher, les
#' coordonnees x et y du centroide du territoire (position des etiquettes X et
#' Y), la taille (TAILLE), le style (FONT) et la couleur de la police (COL). Le
#' style de police est un entier : 1 (normal), 2 (gras, par defaut), 3
#' (italique) et 4 (gras italique).
#'
#' Il est possible de modifier les valeurs du tableau pour changer le style
#' d'une ou plusieurs etiquettes.
#'
#' Il est egalement possible de modifier les coordonnees x et y pour deplacer
#' les etiquettes et ainsi eviter leur chevauchement.
#'
#' - Pour afficher la carte avec les etiquettes formatees, il faut passer ce
#' tableau dans le parametre "etiquettes" de la fonction plot.  } }
#'
#' Il est tout de meme possible de zoomer sur la carte en specifiant les
#' parametres xlim et ylim. Par defaut, la carte est centree sur le fond de
#' maille. Pour connaitre les limites par defaut, executer la fonction
#' \code{sf::st_bbox(fondMaille)}
#'
#' @aliases plot_ronds plot_classes plot_ronds_classes plot_classes_ronds
#' plot_typo plot_typo_symboles plot_oursins plot_joignantes plot_saphirs
#'
#' @usage plot_ronds(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse =
#' NULL, idData, varVolume, rayonRond = NULL, rapportRond = NULL, emprise =
#' "FRM", fondChx = NULL, precisionLegRonds = 0, titreLeg = "", xLeg = NULL,
#' yLeg = NULL, cadreLeg = FALSE, xLimCadreLeg = NULL, yLimCadreLeg = NULL,
#' titreCarte = "", sourceCarte = "", etiquettes = NULL, colPos =
#' "#EB617F", colNeg = "#286AC7", colBorder = "white", colBorderMaille =
#' "black", xlim = NULL, ylim = NULL)
#'
#' plot_classes(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse =
#' NULL, idData, varRatio, methode = "kmeans", nbClasses = 3, bornes = NULL,
#' precisionLegClasses = 1, titreLeg = "", labels = NULL, xLeg = NULL,
#' yLeg = NULL, cadreLeg = FALSE, xLimCadreLeg = NULL, yLimCadreLeg = NULL,
#' titreCarte = "", sourceCarte = "", etiquettes = NULL, stylePalette = "defaut",
#' palettePos = NULL, paletteNeg = NULL, colBorder = "white", xlim = NULL,
#' ylim = NULL)
#'
#' plot_ronds_classes(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse
#' = NULL, idData, varVolume, varRatio, rayonRond = NULL, rapportRond = NULL,
#' methode = "kmeans", nbClasses = 3, bornes = NULL, precisionLegRonds = 0,
#' precisionLegClasses = 1, emprise = "FRM", fondChx = NULL, titreLegRonds =
#' "", titreLegClasses = "", labels = NULL, xLegRonds = NULL, yLegRonds = NULL,
#' xLegClasses = NULL, yLegClasses = NULL, cadreLeg = FALSE, xLimCadreLegRonds = NULL,
#' yLimCadreLegRonds = NULL, xLimCadreLegClasses = NULL, yLimCadreLegClasses = NULL,
#' titreCarte = "", sourceCarte = "", etiquettes = NULL, stylePalette = "defaut",
#' palettePos = NULL, paletteNeg = NULL, colBorder = "white",
#' colBorderMaille = "black", xlim = NULL, ylim = NULL)
#'
#' plot_classes_ronds(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse
#' = NULL, idData, varVolume, varRatio, rayonRond = NULL, rapportRond = NULL,
#' methode = "kmeans", nbClasses = 3, bornes = NULL, precisionLegRonds = 0,
#' precisionLegClasses = 1, emprise = "FRM", fondChx = NULL, titreLegRonds = "",
#' titreLegClasses = "", labels = NULL, xLegRonds = NULL, yLegRonds = NULL,
#' xLegClasses = NULL, yLegClasses = NULL, cadreLeg = FALSE, xLimCadreLegRonds = NULL,
#' yLimCadreLegRonds = NULL, xLimCadreLegClasses = NULL, yLimCadreLegClasses = NULL,
#' titreCarte = "", sourceCarte = "", etiquettes = NULL, stylePalette = "defaut",
#' palettePos = NULL, paletteNeg = NULL, colBorder = "white",
#' colBorderRonds = "#303030", xlim = NULL, ylim = NULL)
#'
#' plot_typo(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse = NULL,
#' idData, varTypo, titreLeg = "", xLeg = NULL, yLeg = NULL, titreCarte = "",
#' sourceCarte = "", etiquettes = NULL, paletteTypo = NULL, labels = NULL,
#' cadreLeg = FALSE, xLimCadreLeg = NULL, yLimCadreLeg = NULL, colBorder = "white",
#' xlim = NULL, ylim = NULL)
#'
#' plot_typo_symboles(fondPoints, listFonds, emprise = "FRM", types = NULL,
#' couleurs = NULL, tailles = NULL, epaisseurs = NULL, titreLeg = "", xLeg =
#' NULL, yLeg = NULL, cadreLeg = FALSE, xLimCadreLeg = NULL, yLimCadreLeg = NULL,
#' titreCarte = "", sourceCarte = "", etiquettes = NULL, labels = NULL, xlim = NULL,
#' ylim = NULL)
#'
#' plot_oursins(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse =
#' NULL, idDataDepart, idDataArrivee, varFlux, filtreVol = 0, filtreDist = 100,
#' filtreMajeurs = 10, decalageAllerRetour = 0, decalageCentroid = 0,
#' titreCarte = "", sourceCarte = "", etiquettes = NULL, epaisseur = 2,
#' colTrait = "black", colBorderMaille = "black", xlim = NULL, ylim = NULL)
#'
#' plot_joignantes(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse =
#' NULL, typeMaille, idDataDepart, idDataArrivee, varFlux, largeurFlecheMax =
#' NULL, filtreVol = 0, filtreDist = 100, filtreMajeurs = 10,
#' decalageAllerRetour = 0, decalageCentroid = 0, emprise = "FRM",
#' precisionLegFleches = 0, titreLeg = "", xLeg = NULL, yLeg = NULL,
#' cadreLeg = FALSE, xLimCadreLeg = NULL, yLimCadreLeg = NULL, titreCarte = "",
#' sourceCarte = "", etiquettes = NULL, colFleche = "#286AC7", colBorder
#' = "white", colBorderMaille = "black", xlim = NULL, ylim = NULL)
#'
#' plot_saphirs(data, fondMaille, fondSousAnalyse = NULL, fondSurAnalyse =
#' NULL, typeMaille, idDataDepart, idDataArrivee, varFlux, largeurFlecheMax =
#' NULL, direction = "Ent", filtreVol = 0, emprise = "FRM", precisionLegFleches
#' = 0, titreLeg = "", xLeg = NULL, yLeg = NULL, cadreLeg = FALSE,
#' xLimCadreLeg = NULL, yLimCadreLeg = NULL, titreCarte = "", sourceCarte = "",
#' etiquettes = NULL, colEntree = "#EB617F", colSortie = "#286AC7",
#' colBorder = "white", colBorderMaille = "black", xlim = NULL, ylim = NULL)
#'
#' @param data tableau de donnees (data.frame).
#' @param fondMaille objet sf. Fond de carte.
#' @param typeMaille chaine de caracteres (character). Type de maille issus
#' d'un zonage administratifs ou d'etudes ("REG", "DEP", "UU", "AU", "ZE",
#' "BV", "EPCI" ou "COM").
#' @param fondSousAnalyse list d'objets sf. Liste de fonds de carte a ajouter
#' en-dessous de l'analyse. Par exemple, un fond de mer et des pays
#' frontaliers. Par defaut a NULL.
#' @param fondPoints objets sf. Fond de points.
#' @param fondSurAnalyse list d'objets sf. Liste de fonds de carte a ajouter
#' au-dessus de l'analyse. Par exemple, un fond de departement. Par defaut a
#' NULL.
#' @param listFonds list d'objets sf. Liste de fonds de carte a ajouter.
#' @param idData chaine de caracteres (character). Variable identifiant la
#' maille.
#' @param idDataDepart chaine de caracteres (character). Variable identifiant
#' le territoire de depart du flux.
#' @param idDataArrivee chaine de caracteres (character). Variable identifiant
#' le territoire d'arrivee du flux.
#' @param varVolume chaine de caracteres (character). Variable en volume pour
#' des ronds proportionnels.
#' @param varRatio chaine de caracteres (character). Variable en ratio pour des
#' analyses en classes.
#' @param varTypo chaine de caracteres (character). Variable de typologie.
#' @param varFlux chaine de caracteres (character). Variable de flux pour des
#' oursins, fleches joignantes ou fleches saphirs.
#' @param rayonRond valeur numerique (numeric). Rayon du rond le plus grand en
#' metres.
#' @param rapportRond valeur numerique (numeric). Rapport entre l'aire du rond
#' le plus grand et la valeur maximale des donnees en volume.
#' @param methode chaine de caracteres (character). A choisir parmi "kmeans"
#' (par defaut), "fisher", "jenks" ou "quantile".
#' @param nbClasses valeur numerique (numeric). Nombre de classes. Par defaut 3
#' classes.
#' @param bornes vecteur de valeurs numeriques (numeric). Le nombre de bornes
#' correspond au nombre de classes -1. Par defaut a NULL.
#' @param precisionLegRonds valeur numerique (numeric). Arrondit les valeurs de
#' la legende des ronds. 0 (par defaut) pour arrondir a l'unite, -1 pour
#' arrondir a la dizaine, -2 a la centaine...
#' @param precisionLegClasses valeur numerique (numeric). Arrondit les valeurs
#' en ratio pour le calcul des bornes de classes. Par defaut, 1 chiffre apres
#' la virgule.
#' @param precisionLegFleches valeur numerique (numeric). Arrondit les valeurs
#' de la legende des fleches. 0 (par defaut) pour arrondir a l'unite, -1 pour
#' arrondir a la dizaine, -2 a la centaine...
#' @param largeurFlecheMax valeur numerique (numeric). Valeur relative
#' correspondant a la largeur de la fleche la plus grande de la carte. Par
#' defaut a NULL.
#' @param direction chaine de caracteres (character). Pour les fleches saphirs,
#' type de flux a choisir parmi "Ent" pour entrees (par defaut), "Sor" pour
#' sorties et "Sol" pour soldes (entrees - sorties).
#' @param filtreVol valeur numerique (numeric). Filtre des valeurs en volume de
#' flux. Par defaut a 0 : affichage de tous les flux.
#' @param filtreDist valeur numerique (numeric). Filtre sur la distance des
#' fleches pour les oursins et les fleches joignantes. Par defaut a 100 km :
#' affichage des flux a moins de 100 km.
#' @param filtreMajeurs valeur numerique (numeric). Filtre sur les flux
#' entrants et sortants les plus importants d'un territoire. Par defaut a 10 :
#' 10 flux maximum sortent d'un territoire ou en entrent.
#' @param decalageAllerRetour valeur numerique (numeric). Decalage entre les
#' fleches aller-retour (km).
#' @param decalageCentroid valeur numerique (numeric). Decalage des fleches
#' depuis et vers les centroides des territoires (km).
#' @param emprise chaine de caracteres (character). "FRM" (par defaut pour la
#' France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973"
#' (Guyane), "974" (La Reunion) ou "976" (Mayotte). Ce parametre permet de
#' definir le systeme de projection de la carte.
#' @param fondChx objet sf. Fond des chefs-lieux. Pour les ronds proportionnels
#' avec une maille communale uniquement, choix entre position du centre des
#' ronds sur les centroides des communes (NULL par defaut) ou sur les
#' chefs-lieux (chx) des communes.
#' @param types vecteur de numeriques (numeric). Par defaut a NULL. Voir
#' details.
#' @param couleurs vecteur de caracteres (character). Par defaut a NULL. Voir
#' details.
#' @param tailles vecteur de numeriques (numeric). Par defaut a NULL. Voir
#' details.
#' @param epaisseurs vecteur de numeriques (numeric). Par defaut a NULL. Voir
#' details.
#' @param titreLeg chaine de caracteres (character). Titre de la legende.
#' @param labels chaine de caracteres (character). Labels personnalises de la legende.
#' @param xLeg valeur numerique (numeric). Coordonnees x de la legende dans le
#' systeme de projection locale. Par defaut a NULL.
#' @param yLeg valeur numerique (numeric). Coordonnees y de la legende dans le
#' systeme de projection locale. Par defaut a NULL.
#' @param cadreLeg booleen (logical). Affichage d'un cadre blanc autour de la legende.
#' Par defaut FALSE.
#' @param xLimCadreLeg vecteur numerique (numeric). Coordonnees xmin et xmax du cadre de
#' la legende. Par defaut a NULL.
#' @param yLimCadreLeg vecteur numerique (numeric). Coordonnees ymin et ymax du cadre de
#' la legende. Par defaut a NULL.
#' @param xLimCadreLegRonds vecteur numerique (numeric). Coordonnees xmin et xmax du cadre de
#' la legende des ronds. Par defaut a NULL.
#' @param yLimCadreLegRonds vecteur numerique (numeric). Coordonnees ymin et ymax du cadre de
#' la legende des ronds. Par defaut a NULL.
#' @param xLimCadreLegClasses vecteur numerique (numeric). Coordonnees xmin et xmax du cadre de
#' la legende des classes. Par defaut a NULL.
#' @param yLimCadreLegClasses vecteur numerique (numeric). Coordonnees ymin et ymax du cadre de
#' la legende des classes. Par defaut a NULL.
#' @param titreLegRonds chaine de caracteres (character). Titre de la legende
#' des ronds.
#' @param xLegRonds valeur numerique (numeric). Coordonnees x de la legende des
#' ronds dans le systeme de projection locale. Par defaut a NULL.
#' @param yLegRonds valeur numerique (numeric). Coordonnees y de la legende des
#' ronds dans le systeme de projection locale. Par defaut a NULL.
#' @param titreLegClasses chaine de caracteres (character). Titre de la legende
#' des classes.
#' @param xLegClasses valeur numerique (numeric). Coordonnees x de la legende
#' des classes dans le systeme de projection locale. Par defaut a NULL.
#' @param yLegClasses valeur numerique (numeric). Coordonnees y de la legende
#' des classes dans le systeme de projection locale. Par defaut a NULL.
#' @param titreCarte chaine de caracteres (character). Titre de la carte.
#' @param sourceCarte chaine de caracteres (character). Source de la carte.
#' @param etiquettes vecteur de caracteres (character) ou tableau de donnees
#' (data.frame). Liste des codes de la maille dont on affiche le libelle sur la
#' carte. Par defaut a NULL. Voir details.
#' @param colPos chaine de caracteres (character). Couleur nommee (par exemple
#' "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut "#EB617F".
#' @param colNeg chaine de caracteres (character). Couleur nommee (par exemple
#' "blue") ou hexadecimal (par exemple "#0000FF"). Par defaut "#286AC7".
#' @param stylePalette chaine de caracteres (character). Palette de la charte
#' INSEE. A choisir entre "defaut", "Insee_Rouge", "Insee_Jaune", "Insee_Bleu",
#' "Insee_Turquoise", "Insee_Vert", "Insee_Violet" ou "Insee_Gris" 
#' (voir 'affiche_palette(nomPalette)').
#' @param palettePos vecteur de caracteres (character). Couleurs nommees (par
#' exemple c("red","orange","yellow")) ou hexadecimal (par exemple
#' c("#FFA500")). Par defaut a NULL.
#' @param paletteNeg vecteur de caracteres (character). Couleurs nommees (par
#' exemple c("purple","blue","green")) ou hexadecimal (par exemple
#' c("#800080","#0000FF","#008000")). Par defaut a NULL.
#' @param paletteTypo vecteur de caracteres (character). Couleurs nommees (par
#' exemple c("red","blue","green")) ou hexadecimal (par exemple
#' c("#FFA500","#0000FF","#008000")). Par defaut a NULL.
#' @param labels vecteur de caracteres (character). Labels personnalises pour
#' la legende de la carte en typologie. Par defaut a NULL.
#' @param epaisseur valeur numerique (numeric). Epaisseur des traits des
#' oursins. Par defaut 2.
#' @param colBorder chaine de caracteres (character). Couleur nommee (par
#' exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white".
#' @param colTrait chaine de caracteres (character). Couleur nommee (par
#' exemple "black") ou hexadecimal (par exemple "#000000"). Par defaut "black".
#' @param colBorderMaille chaine de caracteres (character). Couleur nommee (par
#' exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut "black".
#' @param colBorderRonds chaine de caracteres (character). Couleur nommee (par
#' exemple "grey") ou hexadecimal (par exemple "#808080"). Par defaut
#' "#303030".
#' @param colFleche chaine de caracteres (character). Couleur nommee (par
#' exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut
#' "#286AC7".
#' @param colEntree chaine de caracteres (character). Couleur nommee (par
#' exemple "orange") ou hexadecimal (par exemple "#FFA500"). Par defaut
#' "#EB617F".
#' @param colSortie chaine de caracteres (character). Couleur nommee (par
#' exemple "blue") ou hexadecimal (par exemple "#0000FF"). Par defaut
#' "#286AC7".
#' @param xlim vecteur numerique (numeric). Coordonnees xmin et xmax de la
#' carte. Par defaut a NULL.
#' @param ylim vecteur numerique (numeric). Coordonnees ymin et ymax de la
#' carte. Par defaut a NULL.
#'
#' @return Retourne un objet sf. Il s'agit du fond des ronds (ronds
#' proportionnels, classes dans ronds proportionnels, ronds sur analyse en
#' classes), de la maille (analyse en classes, typologie) ou des fleches
#' (oursins, fleches joignantes, fleches saphirs).
#'
#' Affiche dans le plot la carte demandee.
#'
#' @seealso \code{\link{zonage_a_facon},}
#'
#' \code{\link{coordonnees_etiquettes},}
#'
#' \code{\link{rayon_ronds}, \link{rapport_ronds}, \link{largeur_fleche},}
#'
#' \code{\link{calcul_ratio}, \link{calcul_tx_evol_global},
#' \link{calcul_tx_evol_ann_moy}, \link{calcul_part_ens}, \link{calculette},}
#'
#' \code{\link{distrib_variable},}
#'
#' \code{\link{recup_palette}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#' data("depm")
#'
#' # Ronds proportionnels sur une analyse en classes
#' fond_ronds <- plot_classes_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4,
#' titreLegRonds = "POP_2015", titreLegClasses = "VAR_AN_MOY",
#' xLegClasses = 1150000, yLegClasses = 6600000)
#'
#' etiquettes <- coordonnees_etiquettes(fondMaille = depm,
#' listeCode = c("06","13","31","33","44","67","69","59","75"))
#' etiquettes$LIBELLE <- c("Nice","Marseille","Toulouse","Bordeaux","Nantes","Lille",
#' "Strasbourg","Lyon","Paris")
#' etiquettes[etiquettes$CODE=="75","TAILLE"] <- 1.5
#'
#' # Ronds proportionnels sur une analyse en classes
#' fond_ronds <- plot_classes_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4,
#' titreLegRonds = "POP_2015", titreLegClasses = "VAR_AN_MOY",
#' xLegClasses = 1150000, yLegClasses = 6600000,
#' etiquettes = etiquettes)
#'
#' @import classInt sf grDevices dplyr graphics
#' @importFrom lwgeom st_geod_length
#'
#' @export plot_ronds
#' @export plot_classes
#' @export plot_ronds_classes
#' @export plot_classes_ronds
#' @export plot_typo
#' @export plot_typo_symboles
#' @export plot_oursins
#' @export plot_joignantes
#' @export plot_saphirs
#'
NULL

#' @name set_couleur_
#'
#' @title Modify the colors of 'leaflet' map's analysis
#'
#' @description Modify the colors of 'leaflet' map's analysis.
#'
#' @details Les palettes proposées pour l'analyse en classes sont celles
#' utilisées par la Charte Graphique INSEE. Utilisez la fonction
#' affiche_palette(nomPalette) pour visualiser les couleurs d'une palette.
#' L'argument 'nomPalette' peut prendre comme valeur : "defaut", "Insee_Rouge",
#' "Insee_Jaune", "Insee_Bleu", "Insee_Turquoise", "Insee_Vert", "Insee_Violet"
#' ou "Insee_Gris".
#'
#' Le code des couleurs est le code hexadécimal. Pour récupérer les codes
#' hexadécimaux des différentes palettes proposées : data("palettes_insee")
#'
#' @aliases set_couleur_ronds set_couleur_classes set_couleur_typo
#' set_couleur_joignantes set_couleur_saphirs
#'
#' @usage set_couleur_ronds(map, colorPos = "#EB617F", colorNeg = "#286AC7",
#' map_leaflet = NULL)
#'
#' set_couleur_classes(map, stylePalette = "defaut", palettePos = NULL,
#' paletteNeg = NULL, colBorder = "white", map_leaflet = NULL)
#'
#' set_couleur_typo(map, paletteTypo = NULL, colBorder = "white", map_leaflet =
#' NULL)
#'
#' set_couleur_joignantes(map, colFleche = "#286AC7", colBorder = "black",
#' map_leaflet = NULL)
#'
#' set_couleur_saphirs(map, colEntree = "#EB617F", colSortie = "#286AC7",
#' colBorder = "black", map_leaflet = NULL)
#'
#' @param map objet leaflet.
#' @param colorPos chaine de caracteres (character). Couleur nommee ou
#' hexadecimal. Par defaut "#EB617F".
#' @param colorNeg chaine de caracteres (character). Couleur nommee ou
#' hexadecimal. Par defaut "#286AC7".
#' @param stylePalette chaine de caracteres (character). A choisir parmi
#' "defaut", "Insee_Rouge", "Insee_Jaune", "Insee_Bleu", "Insee_Turquoise",
#' "Insee_Vert", "Insee_Violet" ou "Insee_Gris" (voir
#' 'affiche_palette(nomPalette)').
#' @param palettePos vecteur de caracteres (character). Specifier le vecteur de
#' couleurs (nommees ou hexadecimal) des valeurs positives (au choix). Par
#' defaut a NULL.
#' @param paletteNeg vecteur de caracteres (character). Specifier le vecteur de
#' couleurs (nommees ou hexadecimal) des valeurs negatives (au choix). Par
#' defaut a NULL.
#' @param paletteTypo vecteur de caracteres (character). Specifier le vecteur
#' de couleurs (nommees ou hexadecimal) de la typologie (au choix). Par defaut
#' a NULL.
#' @param colBorder chaine de caracteres (character). Couleur nommee (par
#' exemple "white") ou hexadecimal (par exemple "#FFFFFF"). Par defaut "white"
#' pour classes et typo, "black" pour joignantes et saphirs.
#' @param colFleche chaine de caracteres (character). Couleur nommee ou
#' hexadecimal. Par defaut "#286AC7".
#' @param colEntree chaine de caracteres (character). Couleur nommee ou
#' hexadecimal. Par defaut "#EB617F".
#' @param colSortie chaine de caracteres (character). Couleur nommee ou
#' hexadecimal. Par defaut "#286AC7".
#' @param map_leaflet objet leaflet. Pour l'integration des fonctions leaflet
#' dans les applications shiny (cf vignette). Par defaut a NULL.
#'
#' @return Retourne un objet de type leaflet.
#'
#' @seealso \code{\link{recup_palette},}
#'
#' \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},
#' \link{leaflet_typo},}
#'
#' \code{\link{leaflet_oursins}, \link{leaflet_joignantes},
#' \link{leaflet_saphirs}}
#'
#' @references Un convertisseur de couleurs pour visualiser une couleur a
#' partir de son nom, son code hexadecimal ou RGB :
#' http://www.proftnj.com/RGB3.htm
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#'
#' data("donnees_monoloc")
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varVolume = "POP_2015")
#' map <- set_couleur_ronds(map = map, colorPos = "orange")
#' \donttest{
#'  map
#' }
#'
#' # Ronds proportionnels sur une analyse en classes
#' map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
#' map <- set_couleur_classes(map = map, stylePalette = "Insee_Rouge", colBorder = "black")
#' \donttest{
#'  map
#' }
#'
#' data("donnees_biloc_saphirs")
#' data("regm")
#'
#' # Fleches saphirs
#' map <- leaflet_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG",
#' idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", largeurFlecheMax = 500, direction = "Ent")
#' map <- set_couleur_saphirs(map = map, colEntree = "#4BB375", colBorder = "grey")
#' \donttest{
#'  map
#' }
#'
#' @import leaflet sf stringr grDevices
#'
#' @export set_couleur_ronds
#' @export set_couleur_classes
#' @export set_couleur_typo
#' @export set_couleur_joignantes
#' @export set_couleur_saphirs
#'
NULL

#' @name shiny_
#'
#' @title Creation 'leaflet' maps in a 'shiny' web environment
#'
#' @description Creation 'leaflet' maps in a 'shiny' web environment where the parameters
#' are modifiable on the fly.
#'
#' @details Les cartes sont externalisees de l'environnement R contrairement aux cartes
#' plot et leaflet. Elles s'affichent dans le navigateur.
#'
#' Les cartes produites possedent les memes avantages que les cartes leaflet
#' (deplacement de la carte, zoom/dezoom, pop-up d'informations, placement
#' libre de la legende).
#'
#' L'environnement R-Shiny propose en plus la possibilite de modifier a la
#' volee les parametres de la carte (ordre des fonds, ajout de fonds,
#' representation elargie, taille et rapport des ronds, methode des classes,
#' nombre de classes, bornes, filtre de flux, legende).
#'
#' Les cartes peuvent etre exportees en projet Qgis via un bouton de
#' l'interface R-Shiny. La fonctionnalite cree un .zip a dezipper au meme
#' emplacement.
#'
#' L'apparence des cartes (choix de la palette et des couleurs, etiquettes)
#' n'est cependant pas modifiable. Le but des fonctions shiny est de proposer
#' des cartes realisables et modifiables rapidement avec un minimum de
#' parametrage. En effet, une mise en forme specifique pourra s'effecuer dans
#' Qgis pour la diffusion par exemple.
#'
#' Il est obligatoire de renseigner le contour du territoire d'études dans le
#' paramètre \code{fondContour}.
#'
#' Il est possible de remplacer le fond d'habillage par defaut (France
#' metropolitaine ou DOM) par un autre fond. Pour cela, il faut specifier le
#' parametre emprise="999" ET ajouter un fond sf dans le parametre
#' fondEtranger, par exemple, une carte de l'Europe ou d'un pays particulier.
#' Le systeme de coordonnees du fond doit etre projete (en unite de longueur)
#' et non geographique (lng/lat).  Si aucun code EPSG valide n'est trouve pour
#' ce fond, le code EPSG 3395 est choisi (projection Mercator). Attention car
#' la legende des ronds sera d'autant plus deformee qu'elle se situera loin de
#' l'equateur.
#'
#' \describe{ \item{Ronds proportionnels}{ L'interieur des cercles est de
#' couleur orange (couleur par defaut) pour representer les valeurs positives
#' de la variable en volume et bleu pour les valeurs negatives.
#'
#' Le parametre \code{fondChx} est a renseigner UNIQUEMENT si la maille est
#' communale.
#'
#' Pour information, le chef-lieu (ou chx) est un point de la commune autour
#' duquel la population est la plus dense. Le plus souvent, il s'agit de
#' l'hotel de ville de la commune.  }
#'
#' \item{Analyse en classes}{ En semiologie, il est deconseille de multiplier
#' le nombre de classes pour des raisons de visibilite et de clarte de la
#' carte.
#'
#' Si les donnees ne comprennnent que des valeurs positives ou que des valeurs
#' negatives, il est conseille 5 classes au maximum.
#'
#' Si les donnees comprennnent a la fois des valeurs positives et negatives, le
#' nombre maximum conseille est 9 classes.  }
#'
#' \item{Analyse en classes dans ronds proportionnels}{ L'analyse en classes
#' colore l'interieur des ronds (fonction \code{shiny_ronds_classes()}) }
#'
#' \item{Ronds proportionnels sur analyse en classes}{ Les ronds proportionnels
#' sont poses sur l'analyse en classes (fonction \code{shiny_classes_ronds()})
#' }
#'
#' \item{Typologie}{ Meme conseil que pour les analyses en classes. Le choix
#' des couleurs ne doit pas etre un degrade si il n'existe pas de classement
#' dans les modalites.  }
#'
#' \item{Oursins}{ Les oursins representent uniquement l'existence d'un flux
#' entre 2 territoires. Le sens du flux et le volume ne sont pas pris en
#' compte.
#'
#' Les filtres sur le volume, la distance et les flux majeurs sont
#' independants. Chacun d'eux s'appliquent sur les donnees initiales. Autrement
#' dit, le filtre sur les flux majeurs ne se fera pas apres un eventuel filtre
#' sur la distance mais sur toutes les donnees en entree.
#'
#' Le filtre sur le volume est plutot complementaire au filtre sur les flux
#' majeurs. En effet, il est possible de vouloir representer des faibles flux
#' significatifs. Le filtre sur les flux majeurs le permet contrairement au
#' filtre sur le volume seul.  }
#'
#' \item{Fleches joignantes}{ Les fleches joignantes representent un flux entre
#' 2 territoires, son sens et son volume. Dans le cas de flux aller et retour,
#' deux fleches de sens oppose sont superposees.
#'
#' La regle des filtres est la meme que pour les oursins.  }
#'
#' \item{Fleches saphirs}{ Les fleches saphirs convergent vers ou divergent
#' d'un meme territoire. Elles permettent de representer les entrees, les
#' sorties ou le solde (entrees - sorties) d'un territoire par rapport aux
#' autres.
#'
#' Pour un solde positif, les fleches sont representees en orange, pour un
#' solde negatif en bleu.
#'
#' Une attention particuliere est a apporter au choix de la variable
#' idDataDepart et idDataArrivee selon si le flux est entrant, sortant ou en
#' solde.  } }
#'
#' @aliases shiny_ronds shiny_classes shiny_ronds_classes shiny_classes_ronds
#' shiny_typo shiny_oursins shiny_joignantes shiny_saphirs
#'
#' @usage shiny_ronds(data, fondMaille, fondMailleElargi = NULL, fondContour,
#' fondSuppl = NULL, idData, varVolume, emprise = "FRM", fondEtranger = NULL,
#' fondChx = NULL)
#'
#' shiny_classes(data, fondMaille, fondMailleElargi = NULL, fondContour,
#' fondSuppl = NULL, idData, varRatio, emprise = "FRM", fondEtranger = NULL)
#'
#' shiny_ronds_classes(data, fondMaille, fondMailleElargi = NULL, fondContour,
#' fondSuppl = NULL, idData, varVolume, varRatio, emprise = "FRM", fondEtranger
#' = NULL, fondChx = NULL)
#'
#' shiny_classes_ronds(data, fondMaille, fondMailleElargi = NULL, fondContour,
#' fondSuppl = NULL, idData, varVolume, varRatio, emprise = "FRM", fondEtranger
#' = NULL, fondChx = NULL)
#'
#' shiny_typo(data, fondMaille, fondContour, fondSuppl = NULL, idData, varTypo,
#' emprise = "FRM", fondEtranger = NULL)
#'
#' shiny_oursins(data, fondMaille, fondContour, fondSuppl = NULL, idDataDepart,
#' idDataArrivee, varFlux, decalageAllerRetour = 0, decalageCentroid = 0,
#' emprise = "FRM", fondEtranger = NULL)
#'
#' shiny_joignantes(data, fondMaille, typeMaille, fondContour, fondSuppl =
#' NULL, idDataDepart, idDataArrivee, varFlux, decalageAllerRetour = 0,
#' decalageCentroid = 0, emprise = "FRM", fondEtranger = NULL)
#'
#' shiny_saphirs(data, fondMaille, typeMaille, fondContour, fondSuppl = NULL,
#' idDataDepart, idDataArrivee, varFlux, direction = "Ent", emprise = "FRM",
#' fondEtranger = NULL)
#'
#' @param data tableau de donnees (data.frame).
#' @param fondMaille objet sf. Fond de carte.
#' @param typeMaille chaine de caracteres (character). Type de maille issu d'un
#' zonage administratif ou d'etudes ("REG", "DEP", "UU", "AU", "ZE", "BV",
#' "EPCI" ou "COM").
#' @param fondMailleElargi objet sf. Fond de carte. Par defaut a NULL.
#' @param fondContour objet sf. Fond de carte.
#' @param fondSuppl objet sf. Fond de carte. Par defaut a NULL.
#' @param idData chaine de caracteres (character). Variable identifiant la
#' maille.
#' @param idDataDepart chaine de caracteres (character). Variable identifiant
#' le depart du flux.
#' @param idDataArrivee chaine de caracteres (character). Variable identifiant
#' l'arrivee du flux.
#' @param varVolume chaine de caracteres (character). Variable en volume pour
#' les ronds proportionnels.
#' @param varRatio chaine de caracteres (character). Variable en ratio pour
#' l'analyse en classes.
#' @param varTypo chaine de caracteres (character). Variable de typologie.
#' @param varFlux chaine de caracteres (character). Variable de flux pour les
#' oursins, fleches joignantes ou fleches saphirs.
#' @param direction chaine de caracteres (character). Type de flux. A choisir
#' parmi "Ent" pour entrees (par defaut), "Sor" pour sorties et "Sol" pour
#' soldes (entrees - sorties).
#' @param decalageAllerRetour valeur numerique (numeric). Decalage entre les
#' fleches aller-retour (km).
#' @param decalageCentroid valeur numerique (numeric). Decalage des fleches
#' depuis et vers les centroides des territoires (km).
#' @param emprise chaine de caracteres (character). "FRM" (par defaut pour la
#' France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973"
#' (Guyane), "974" (La Reunion), "976" (Mayotte) ou "999" (Etranger). Ce
#' parametre permet de definir le systeme de projection de la carte.
#' @param fondEtranger objet sf. Fond de carte d'habillage personnalisable pour
#' les fonds autres que la France (metropolitaine et DOM). Par defaut a NULL.
#' @param fondChx objet sf. Fond des chefs-lieux. Pour les ronds proportionnels
#' avec une maille communale uniquement, choix entre position du centre des
#' ronds sur les centroides des communes (NULL par defaut) ou sur les
#' chefs-lieux (chx) des communes.
#'
#' @return Ne retourne aucun objet.
#'
#' Ouvre une fenetre avec un environnement R-Shiny dans le navigateur.
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},
#' \link{leaflet_typo},}
#'
#' \code{\link{leaflet_oursins}, \link{leaflet_joignantes},
#' \link{leaflet_saphirs},}
#'
#' \code{\link{export_qgis_ronds}, \link{export_qgis_classes},
#' \link{export_qgis_ronds_classes}, \link{export_qgis_classes_ronds},}
#'
#' \code{\link{export_qgis_typo},}
#'
#' \code{\link{export_qgis_oursins}, \link{export_qgis_joignantes},
#' \link{export_qgis_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#'
#' data("depm")
#' data("regm")
#' data("fram")
#'
#' data("donnees_monoloc")
#'
#' if(interactive()){
#' # Ronds proportionnels
#' shiny_ronds(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
#' idData = "COD_DEP", varVolume = "POP_2015")
#'
#' # Analyse en classes
#' shiny_classes(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
#' idData = "COD_DEP", varRatio = "VAR_AN_MOY")
#'
#' # Analyse en classes dans les ronds proportionnels
#' shiny_ronds_classes(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY")
#'
#' # Ronds proportionnels sur une analyse en classes
#' shiny_classes_ronds(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY")
#'
#'  # Typologie
#' shiny_typo(data = donnees_monoloc, fondMaille = depm, fondContour = fram,
#' idData = "COD_DEP", varTypo = "REG")
#' }
#'
#' data("donnees_biloc")
#'
#' if(interactive()){
#' # Oursins
#' shiny_oursins(data = donnees_biloc, fondMaille = regm, fondContour = fram,
#' idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR")
#'
#' # Fleches joignantes
#' shiny_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
#' fondContour = fram, idDataDepart = "REG_DEPART",
#' idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR")
#' }
#'
#' data("donnees_biloc_saphirs")
#'
#' if(interactive()){
#' # Fleches saphirs
#' shiny_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG",
#' fondContour = fram, idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE",
#' varFlux = "MIGR", direction = "Ent")
#' }
#'
#' @rawNamespace import(shiny, except = runExample)
#' @import shinythemes shinyjs leaflet sf ggplot2 classInt leaflet.extras dplyr
#' @importFrom shinyBS bsButton updateButton
#' @importFrom lwgeom st_geod_length
#' @importFrom utils write.csv
#' @importFrom zip zip
#'
#' @export shiny_ronds
#' @export shiny_classes
#' @export shiny_ronds_classes
#' @export shiny_classes_ronds
#' @export shiny_typo
#' @export shiny_oursins
#' @export shiny_joignantes
#' @export shiny_saphirs
#'
NULL

#' @name donnees
#'
#' @title Examples of data
#'
#' @description Data tables for examples.
#'
#' - \code{com_dep_13_30_83_84} for \code{zonage_a_facon}
#'
#' - \code{donnees_a_facon} for \code{zonage_a_facon}
#'
#' - \code{donnees_monoloc} for proportional circles, chroropleth and typology
#'
#' - \code{donnees_biloc} for propotionnal arrows
#'
#' - \code{donnees_biloc_saphirs} for sapphire arrows
#'
#' - \code{depm, regm, fram} for the meshes and contours of the maps
#' 
#' - \code{palettes_insee} for palette of the graphic chart of INSEE
#'
#' @aliases com_dep_13_30_83_84 donnees_a_facon donnees_monoloc donnees_biloc
#' donnees_biloc_saphirs depm regm fram palettes_insee
#'
#' @docType data
#'
#' @usage data("com_dep_13_30_83_84")
#'
#' data("donnees_a_facon")
#'
#' data("donnees_monoloc")
#'
#' data("donnees_biloc")
#'
#' data("donnees_biloc_saphirs")
#'
#' data("depm")
#'
#' data("regm")
#'
#' data("fram")
#' 
#' data("palettes_insee")
#'
#' @format \code{com_dep_13_30_83_84} Un data frame de 792 observations et 3
#' variables.  \describe{ \item{"CODE"}{vecteur caracteres}
#' \item{"LIBELLE"}{vecteur caracteres}
#' \item{"geometry"}{sfc_GEOMETRY} }
#'
#' Un data frame de 281 observations et 3 variables.  \describe{
#' \item{"DEPCOM"}{vecteur caracteres} \item{"ZE2010"}{vecteur
#' caracteres} \item{"LIB_ZE2010"}{vecteur caracteres} }
#'
#' \code{donnees_monoloc} Un data frame de 96 observations et 6 variables.
#' \describe{ \item{"COD_DEP"}{vecteur caracteres}
#' \item{"LIB_DEP"}{vecteur caracteres} \item{"POP_2010"}{vecteur
#' numerique} \item{"POP_2015"}{vecteur numerique}
#' \item{"VAR_AN_MOY"}{vecteur numerique} \item{"REG"}{vecteur
#' caracteres} }
#'
#' \code{donnees_biloc} Un data frame de 378 observations et 3 variables.
#' \describe{ \item{"REG_DEPART"}{vecteur caracteres}
#' \item{"REG_ARRIVEE"}{vecteur caracteres} \item{"MIGR"}{vecteur
#' numerique} }
#'
#' \code{donnees_biloc_saphirs} Un data frame de 21 observations et 3
#' variables.  \describe{ \item{"REG_DEPART"}{vecteur caracteres}
#' \item{"REG_ARRIVEE"}{vecteur caracteres} \item{"MIGR"}{vecteur
#' numerique} }
#'
#' \code{depm} Un data frame de 96 observations et 5 variables.  \describe{
#' \item{"CODE"}{vecteur caracteres} \item{"LIBELLE"}{vecteur
#' caracteres} \item{"REG"}{vecteur caracteres}
#' \item{"SURF"}{vecteur numerique} \item{"geometry"}{sfc_GEOMETRY}
#' }
#'
#' \code{regm} Un data frame de 13 observations et 4 variables.  \describe{
#' \item{"CODE"}{vecteur caracteres} \item{"LIBELLE"}{vecteur
#' caracteres} \item{"SURF"}{vecteur numerique}
#' \item{"geometry"}{sfc_GEOMETRY} }
#'
#' \code{fram} Un data frame de 1 observation et 4 variables.  \describe{
#' \item{"CODE"}{vecteur caracteres} \item{"LIBELLE"}{vecteur
#' caracteres} \item{"SURF"}{vecteur numerique}
#' \item{"geometry"}{sfc_GEOMETRY} }
#' 
#' \code{palettes_insee} Une liste de vecteurs caracteres.  \describe{
#' \item{"nomPalette"}{vecteur caracteres de codes hexadecimaux} }
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(com_dep_13_30_83_84)
#' data(donnees_a_facon)
#' data(donnees_monoloc)
#' data(donnees_biloc)
#' data(donnees_biloc_saphirs)
#' data(depm)
#' data(regm)
#' data(fram)
#' data(palettes_insee)
#'
NULL

#' @name oceanis-package
#'
#' @title Package : oceanis
#'
#' @description Type : Package
#'
#' Version : 1.0.3
#'
#' Date : 2019-09-06
#'
#' License : GPL(>=2.0)
#'
#' @details Outil de cartographie permettant de realiser des cartes d'etudes et
#' d'analyses (ronds proportionnels, analyse en classes, typologie, oursins,
#' fleches joignantes et fleches saphirs).
#'
#' Le package \code{oceanis} repond a 3 types de besoins :
#'
#' - Creer des cartes fixes avec la possibilite d'ajouter des etiquettes. Ideal
#' pour les impressions de documents (.pdf ou autres).
#'
#' - Creer des cartes interactives grace au zoom et aux pop-up (technologie
#' leaflet). Ideal pour l'integration dans une application (en shiny par
#' exemple) ou dans une presentation en direct (en HTML).
#'
#' - Creer des cartes dans un environnement web ou les parametres sont
#' modifiables a la volee (technologie R-Shiny + leaflet). Ideal pour une
#' visualisation rapide des donnees ou pour une presentation en direct. Ici, la
#' carte est externalisee vers une nouvelle fenetre du navigateur.
#'
#' Les fonctions de creation de cartes sont classees en 3 categories,
#' correspondant aux 3 besoins precedents. Elles sont respectivement prefixees
#' par \code{plot_}, \code{leaflet_} et \code{shiny_}.
#'
#' D'autres fonctions existent pour completer l'offre :
#'
#' Des fonctions pour exporter une carte leaflet en projet Qgis, des fonctions
#' pour modifier son apparence, des fonctions permettant de creer ses propres
#' zonages (connus ou a facon) a partir de tableaux de donnees.
#'
#' Un exemple possible d'enchainement de fonctions :
#'
#' 1- Chargement du package oceanis (\code{library(oceanis)})
#'
#' 2- Chargement des donnees avec la fonction \code{import()} du package
#' \code{rio}
#'
#' 3- Chargement des fonds de carte avec la fonction \code{read_sf()} du
#' package \code{sf}
#'
#' 4- Appel a une fonction de creation de carte (shiny_(), leaflet_() ou
#' plot_())
#'
#' 5- Modification du parametrage (legende, fonds supplementaire, taille,
#' couleur, habillage...)
#'
#' 6- Export de la carte en projet Qgis (uniquement pour les fonctions shiny_()
#' et leaflet_())
#'
#' ou 6-bis- Export de la carte en format image.
#'
#' Pour resumer, voici un comparatif des fonctionnalites proposees :
#'
#' PLOT
#'
#' Interactivite : Non
#'
#' Personnalisation / Parametrages : Fort
#'
#' Integration dans les applications : Moyen
#'
#' Export Qgis : Non
#'
#' Etiquettes : Oui
#'
#' Habillage : Fort
#'
#' Zonage a facon : Oui
#'
#' Representation elargie : Non
#'
#' LEAFLET
#'
#' Interactivite : Oui
#'
#' Personnalisation / Parametrages : Moyen
#'
#' Integration dans les applications : Fort
#'
#' Export Qgis : Oui
#'
#' Etiquettes : Non
#'
#' Habillage : Fort
#'
#' Zonage a facon : Oui
#'
#' Representation elargie : Oui
#'
#' R-SHINY
#'
#' Interactivite : Oui
#'
#' Personnalisation / Parametrages : Faible
#'
#' Integration dans les applications : Moyen
#'
#' Export Qgis : Oui
#'
#' Etiquettes : Non
#'
#' Habillage : Faible
#'
#' Zonage a facon : Oui
#'
#' Representation elargie : Oui
#'
#' @aliases oceanis-package
#'
#' @docType package
#'
#' @keywords package
#'
NULL
