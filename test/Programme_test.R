data("regm")
data("donnees_monoloc")


# Exemple 1
dep28 <- read_sf("dep_reg_28_2019.shp",stringsAsFactors = F)

map <- leaflet_ronds(data = donnees_monoloc, fondMaille = dep28, fondMailleElargi = depm,
                     fondSuppl =depm, rayonRond = NULL,rapportRond = NULL,
                     idData = "COD_DEP", varVolume = "POP_2015", fondChx = NULL) %>%
  add_legende_ronds(map=., titre = "Donnees par dep de la region 28", lng = -1.8, lat = 50, dom = "0")
map
rayon_ronds(map)
rapport_ronds(map)


load (file="base_communale_2018_popleg2016.RData")

base_reg84 <- BDCOM_com[BDCOM_com$REG=="84",]
chx84 <- read_sf("cheflieu_reg_84_2018.shp",stringsAsFactors = F)
comf <- read_sf("commune_francemetro_2018.shp",stringsAsFactors = F)
com84 <- read_sf("commune_reg_84_2018.shp",stringsAsFactors = F)

map <- leaflet_ronds(data = base_reg84, fondMaille = com84, fondMailleElargi = NULL,
                     fondSuppl =depm, rayonRond = 20000,rapportRond = NULL,
                     idData = "DEPCOM", varVolume = "Pop_mun_2016", fondChx = NULL) %>%
  add_legende_ronds(map=., titre = NULL, lng = NULL, lat = NULL, dom = "0")
map


map <- leaflet_ronds(data = base_reg84, fondMaille = com84, fondMailleElargi = NULL,
                     fondSuppl =depm, rayonRond = 20000,rapportRond = NULL,
                     idData = "DEPCOM", varVolume = "Pop_mun_2016", fondChx = chx84) %>%
  add_legende_ronds(map=., titre = NULL, lng = NULL, lat = NULL, dom = "0")
map


map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP", varVolume = "POP_2015") %>%
  add_legende_ronds(map=., titre = NULL, lng = NULL, lat = NULL, dom = "0")
map
export_qgis_ronds(map, "/home/thr4dx/fonds", "ronds1", titre1 = "Les ronds", titre2 = "toto1", source = "RP 2016")


base_reg971<- BDCOM_com[BDCOM_com$REG=="01",]
com971 <- read_sf("commune_reg_01_2018.shp",stringsAsFactors = F)

map <- leaflet_ronds(data = base_reg971, fondMaille = com971, fondMailleElargi = NULL,
                     fondSuppl =NULL, rayonRond = NULL,rapportRond = NULL,
                     idData = "DEPCOM", varVolume = "Pop_mun_2016", fondChx = NULL,dom="971") %>% 
  add_legende_ronds(map=., titre = NULL, lng = NULL, lat = NULL, dom = "971")
map


# Exemple 2
txevolglobal <- calcul_tx_evol_global(data=donnees_monoloc, var1="POP_2015", var2="POP_2010")

map <- leaflet_classes(data = txevolglobal, fondMaille = depm, fondMailleElargi = NULL, fondSuppl = NULL, 
                       idData = "COD_DEP", varRatio = "TEG", methode = "kmeans",
                       nbClasses = 4, bornes = c(-1,0,1), precision = 1,
                       dom = "0")
map <- add_legende_classes(map, titre = NULL, lng = 8, lat = 50, typeLegende = 1, zoom = 7.5)
map

ratio <- calcul_ratio(data=donnees_monoloc, var1="POP_2015", var2="POP_2010")

evolanmoy <- calcul_tx_evol_ann_moy(data=donnees_monoloc, var1="POP_2010", var2="POP_2015", nbAnnees=5)

part <- calcul_part_ens(data=donnees_monoloc, var="POP_2015")

calcul <- calculette(data=donnees_monoloc, formule = "POP_2015/POP_2010")

base_reg973<- BDCOM_com[BDCOM_com$REG=="03",]
com973 <- read_sf("commune_reg_03_2018.shp",stringsAsFactors = F)
reg_R03 <-read_sf("reg_reg_03_2018.shp",stringsAsFactors = F)

map <- leaflet_classes(data = base_reg973, fondMaille = com973, fondMailleElargi = NULL, fondSuppl = reg_R03, 
                       idData = "DEPCOM", varRatio = "Pop_mun_2016", methode = "kmeans",
                       nbClasses = 3, bornes = NULL, precision = 1,
                       dom = "973") %>%
  
  add_legende_classes(map=., titre = NULL, lng = NULL, lat = NULL, typeLegende = 1, zoom = 8)
map


# Exemple 3
donnees_monoloc11 <- donnees_monoloc[donnees_monoloc$REG=="11",]
dep11 <- read_sf("dep_reg_11_2018.shp",stringsAsFactors = F)

map <- leaflet_ronds_classes(data = donnees_monoloc, fondMaille = dep11, fondMailleElargi = NULL, 
                             fondSuppl = regm, idData = "COD_DEP", varVolume = "POP_2015", 
                             varRatio = "VAR_AN_MOY", rayonRond = NULL, rapportRond = NULL,
                             methode = "kmeans", nbClasses = 4, bornes = NULL, precision = 2,
                             dom = "0", fondChx = NULL)  %>%
  
  add_legende_classes(map=., titre = NULL, lng = NULL, lat = NULL, typeLegende = 1, zoom = 9)  %>%
  add_legende_ronds(map=., titre = NULL, lng =NULL, lat = NULL, dom = "0")
map

map <- leaflet_ronds_classes(data = base_reg973, fondMaille = com973, fondMailleElargi = NULL, 
                             fondSuppl = NULL , idData = "DEPCOM", varVolume = "Pop_mun_2016", 
                             varRatio = "SURFACE", rayonRond = NULL, rapportRond = NULL,
                             methode = "kmeans", nbClasses = 4, bornes = NULL, precision = 0,
                             dom = "973", fondChx = NULL)  %>%
  
  add_legende_classes(map=., titre = NULL, lng = NULL, lat = NULL, typeLegende = 1, zoom = 7)  %>%
  add_legende_ronds(map=., titre = NULL, lng =NULL, lat = NULL, dom = "973",precision=-2)
map


# Exemple 4
map <- leaflet_classes_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
                             varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4)
map

# Exemple 5

# ne peut on pas enlever le fond bleu ?
map<-leaflet_fonds_simples(list(depm,regm))  %>%
  set_fonds_simples(map=., colRemplissageFonds = NULL, colBordureFonds = c("red","black"),
                    transparenceFonds = c(0,0), epaisseurFonds = c(0.5,2))
map

map <- leaflet_fonds_simples(listFonds = list(depm))
points <- suppressWarnings(sf::st_centroid(depm[depm$CODE=="75",]))
points <- rbind(points,suppressWarnings(sf::st_centroid(depm[depm$CODE=="13",])))
points <- rbind(points,suppressWarnings(sf::st_centroid(depm[depm$CODE=="69",])))
types <- c(7,8,21)
couleurs <- c("blue","red","red")
tailles <- c(25,20,20)
map <- add_typo_symboles(map, fondPoints=points, types = types,
                         couleurs = couleurs, tailles = tailles)
map <- add_legende_typo_symboles(map, titre ="Legende toto", lng = NULL, lat = NULL, labels = NULL,
                                 zoom = 8)

map




map <- leaflet_typo(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP", varTypo = "REG")
map

data("donnees_biloc")

# Exemple 6

map <- leaflet_oursins(data = donnees_biloc, fondMaille = regm, idDataDepart = "REG_DEPART",
                       idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", filtreVol = 0,
                       filtreDist = 5000, filtreMajeurs = 2) %>%
  add_source(map=.,"source=Insee RP 2016")  %>%
  add_titre(map=.,"Flux domicile-travail")
map



# Exemple 7
library(foreign)
dtPACA <- read.dbf(file = "DT_PACA_sup_100_RP2012_geo2014.dbf",as.is = TRUE)

com93 <- read_sf("commune_reg_93_2018.shp",stringsAsFactors = F)

map <- leaflet_oursins(data = dtPACA, fondMaille = com93, idDataDepart = "CODGEO",
                       idDataArrivee = "DCLT", varFlux = "NB")

map <- leaflet_joignantes(data=dtPACA, fondMaille=com93, typeMaille="COM",  
                          idDataDepart="CODGEO", idDataArrivee="DCLT",
                          varFlux="NB", largeurFlecheMax = 3, filtreVol = 100, filtreDist = 100,
                          filtreMajeurs = 5, dom = "0") %>%
  add_legende_joignantes(map=., titre = NULL, lng =6.9, lat = 44, dom = "0", zoom = 9)
map
largeur_fleche(map)



map <- leaflet_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG",
                          idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR",
                          filtreDist = 500, filtreMajeurs = 10)
map



data("donnees_biloc_saphirs")
# Exemple 8
map <- leaflet_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG",
                       idDataDepart = "REG_ARRIVEE", idDataArrivee = "REG_DEPART", varFlux = "MIGR",
                       largeurFlecheMax = NULL, direction = "Sor") %>%
  add_legende_saphirs(map=., titre = NULL, lng = NULL, lat = NULL, dom = "0", zoom = 7)
map

Solde<- lecture_fichier(file = "U:/OCEANIS/package/Solde_carte2.dbf")

map <- leaflet_saphirs(data = Solde, fondMaille = regm, typeMaille = "REG",
                       idDataDepart="ORIGINE", idDataArrivee="DEST", varFlux = "SOLDE",
                       largeurFlecheMax = 100, direction = "Sol", dom = "0") %>%
  add_legende_saphirs(map=., titre = NULL, lng = NULL, lat = NULL, dom = "0", zoom = 8)
map
export_qgis_saphirs(map, "U:/OCEANIS/package/qgis", "saphir",  source = "RP 2016")


# Les plots

data("donnees_monoloc")
data("depm")
fond_ronds <- plot_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
                         varVolume = "POP_2015", rayonRond = NULL, rapportRond = NULL)

etiquettes <- coordonnees_etiquettes(fondMaille = depm,
                                     listeCode = c("06","13","31","33","44","67","69","59","75"))

etiquettes$LIBELLE <- c("Nice","Marseille","Toulouse","Bordeaux","Nantes","Lille","Strasbourg",
                        "Lyon","Paris")
etiquettes[etiquettes$CODE=="75","TAILLE"] <- 1.5
etiquettes$COL <- "red"
etiquettes$FONT <- 1

paysf <- read_sf("paysf_2018.shp",stringsAsFactors = F)
merf <- read_sf("merf_2018.shp",stringsAsFactors = F)

merf2 <- cbind(merf,COL="#6495ED",BORDER="#6495ED")
paysf2 <- cbind(paysf,COL="grey",BORDER="black",EPAISSEUR=0.5)
regm2 <- cbind(regm,COL="transparent",BORDER="black",EPAISSEUR=1.5)

fond_ronds <-plot_ronds(data = donnees_monoloc, fondMaille = depm, fondSousAnalyse = list(paysf2,merf2), 
                        fondSurAnalyse=list(regm2), idData = "COD_DEP", 
                        varVolume="POP_2015", rayonRond = 40000, rapportRond = NULL, dom = "0", fondChx = NULL, 
                        precisionLegRonds = -1, titreLeg = "test plot-rond", xLeg = 1100000, yLeg = 7000000, etiquettes = etiquettes, colPos = "pink", 
                        colNeg = "#6495ED", colBorder = "brown", colBorderMaille = "grey")

fond_classes <- plot_classes(data = donnees_monoloc, fondMaille = depm, fondSousAnalyse = list(paysf2,merf2), 
                             fondSurAnalyse=list(regm2),
                             idData = "COD_DEP", varRatio = "VAR_AN_MOY", methode = "kmeans",
                             nbClasses = 4, bornes = NULL, precisionLegClasses = 0, titreLeg = "PLot classes", xLeg = 1079000,
                             yLeg = 7060000, etiquettes = etiquettes, stylePalette = "InseePremiere", palettePos = NULL,
                             paletteNeg = NULL, colBorder = "white")


fond_classes_ronds <- plot_classes_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
                                         varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4,
                                         precisionLegRonds = -1, precisionLegClasses = 0, 
                                         titreLegRonds = "POP_2015", titreLegClasses = "VAR_AN_MOY",
                                         xLegClasses = 1150000, yLegClasses = 6600000, etiquettes = etiquettes)

fond_rond_classes <- plot_ronds_classes(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
                                        varVolume = "POP_2015", varRatio = "VAR_AN_MOY", 
                                        rayonRond = 45000, rapportRond = NULL, methode = "kmeans", nbClasses = 4,
                                        bornes = NULL, dom = "0", fondChx = NULL, precisionLegRonds=-2,precisionLegClasses=0,
                                        titreLegRonds = "ronds", titreLegClasses = "classes", xLegRonds = NULL,
                                        yLegRonds = NULL, xLegClasses = 1128060, yLegClasses = 6300010, etiquettes = NULL,
                                        stylePalette = "defaut", palettePos = NULL, paletteNeg = NULL,
                                        colBorder = "white", colBorderMaille = "black")




com44 <- read_sf("commune_reg_44_2018.shp",stringsAsFactors = F)

fond_typo <- plot_typo(data=BDCOM_com, fondMaille=com44, fondSousAnalyse = list(paysf2), fondSurAnalyse=list(regm2),
                       idData="DEPCOM", varTypo="DEP", 
                       titreLeg = "toto", xLeg = 1080000,
                       yLeg = 6979381, etiquettes = NULL, 
                       paletteTypo =c("purple","blue","green","white","black","red","pink","orange","brown","yellow"), 
                       colBorder = "white")

fond_typo <- plot_typo(data=BDCOM_com, fondMaille=com44, fondSousAnalyse = NULL, fondSurAnalyse=list(regm2),
                       idData="DEPCOM", varTypo="DEP", 
                       titreLeg = "toto", xLeg = 1080000,
                       yLeg = 6979381, etiquettes = NULL, 
                       paletteTypo =c("purple","blue","green","white","black","red","pink","orange","brown","yellow"), 
                       colBorder = "white")

plot_typo_symboles(fondPoints=points, listFonds=list(depm), dom = "0", types = c(15,3,8), couleurs = c("black","blue","pink"),
                   tailles = NULL, epaisseurs = NULL, titreLeg = "Légende", xLeg = 1100000, yLeg = 6803250,
                   titreCarte = "", sourceCarte = "", etiquettes = NULL, labels = NULL) 

paysf <- read_sf("//applishare.insee.fr/applishare/PROD/creacartes/pd/fichiers-ihm/2018/Europe/paysf_2018.shp",stringsAsFactors = F)
merf <- read_sf("//applishare.insee.fr/applishare/PROD/creacartes/pd/fichiers-ihm/2018/Europe/merf_2018.shp",stringsAsFactors = F)

etik <- coordonnees_etiquettes(fondMaille=regm, listeCode=c("11","93","94","44","52","53","75","76","24","27","28"))
etik$TAILLE <- 0.8

fond_oursins <- plot_oursins(data=donnees_biloc, fondMaille=regm, fondSousAnalyse = list(paysf2,merf2), 
                             idDataDepart="REG_DEPART", idDataArrive="REG_ARRIVEE", varFlux="MIGR",
                             filtreVol = 10, filtreDist = 1000, filtreMajeurs = 10, etiquettes = etik,
                             epaisseur = 1, colTrait = "red", colBorderMaille = "black")

fond_joignantes <- plot_joignantes(data=donnees_biloc, fondMaille=regm, fondSousAnalyse = list(paysf2,merf2), typeMaille="REG", 
                                   idDataDepart="REG_DEPART", idDataArrive="REG_ARRIVEE", varFlux="MIGR",
                                   largeurFlecheMax = NULL, filtreVol = 0, filtreDist = 1000,
                                   filtreMajeurs = 5, dom = "0", titreLeg = "flÃ¨ches joignantes", xLeg = NULL, yLeg = NULL,
                                   etiquettes = etik, colFleche = "pink", colBorder = "pink",
                                   colBorderMaille = "black")


fond_saphirs <- plot_saphirs(data=donnees_biloc_saphirs, fondMaille=regm, fondSousAnalyse = list(paysf2,merf2), typeMaille="REG", 
                             idDataDepart="REG_DEPART", idDataArrive="REG_ARRIVEE", varFlux="MIGR",
                             largeurFlecheMax = 1000, direction = "Ent", filtreVol = 0, dom = "0",
                             titreLeg = "", xLeg = NULL, yLeg = NULL, etiquettes = etik, colEntree = "#CD853F",
                             colSortie = "#6495ED", colBorder = "white", colBorderMaille = "black")

fluxze<- lecture_fichier(file = "U:/OCEANIS/package/fluxze.dbf")
ze <- read_sf("//applishare.insee.fr/applishare/PROD/creacartes/pd/fichiers-ihm/2018/reg/93/z10_reg_93_2018.shp",stringsAsFactors = F)

fond_saphirs <- plot_saphirs(data=fluxze, fondMaille=ze, fondSousAnalyse = NULL, typeMaille="ZE", 
                             idDataDepart="CODDEP", idDataArrive="CODARR", varFlux="MIGR",
                             largeurFlecheMax = NULL, direction = "Ent", filtreVol = 0, dom = "0",precisionLegFleches = 0,
                             titreLeg = "", xLeg = NULL, yLeg = NULL, etiquettes = NULL, colEntree = "#CD853F",
                             colSortie = "#6495ED", colBorder = "white", colBorderMaille = "black")


dep13<- lecture_fichier(file = "U:/OCEANIS/package/depflux13.dbf")

fond_saphirs <- plot_saphirs(data=dep13, fondMaille=depm, fondSousAnalyse = NULL, typeMaille="DEP", 
                             idDataDepart="DEP1", idDataArrive="DEP", varFlux="MIGR",
                             largeurFlecheMax = NULL, direction = "Sor", filtreVol = 0, dom = "0",precisionLegFleches = 0,
                             titreLeg = "", xLeg = NULL, yLeg = NULL, etiquettes = NULL, colEntree = "#CD853F",
                             colSortie = "#6495ED", colBorder = "white", colBorderMaille = "black")



# Zonage a facon

tab44<- lecture_fichier(file = "U:/OCEANIS/package/selection1.dbf")
tab44$GROUPE <- as.character(tab44$GROUPE)
com44 <- read_sf("//applishare.insee.fr/applishare/PROD/creacartes/pd/fichiers-ihm/2018/reg/44/commune_reg_44_2018.shp",stringsAsFactors = F)

zone44 <- zonage_a_facon(fondMaille=com44, groupe=tab44, idMaille="CODE", idGroupe="GROUPE", libGroupe="LIBGROUP", fondContour = NULL, dom = "0")
plot(st_geometry(com44),col = "transparent", border = "grey")
plot(st_geometry(zone44), add=T,col = "pink", border = "red")


data("donnees_a_facon")
data("com_dep_13_30_83_84")
data("depm")

ze13etplus <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon,
                             idMaille = "DEPCOM", idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
                             fondContour = NULL, dom = "0") # sans contour

ze13 <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon, idMaille = "DEPCOM",
                       idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
                       fondContour = depm[depm$CODE=="13",], dom = "0") # avec contour

## affiche les ZE2010 ayant au moins une partie dans les Bouches-du-Rhone
plot(st_geometry(ze13etplus), col = "transparent", border = "grey")
## affiche uniquement la partie des ZE2010 contenue dans les Bouches-du-Rhone
plot(st_geometry(ze13), col = "transparent", add = T)
## affiche le contour des Bouches-du-Rhone
plot(st_geometry(depm[depm$CODE=="13",]), border = "red", add = T)

# SHINY

data("depm")
data("regm")
data("fram")

# Run exemples

data("donnees_monoloc")
# Exemple 1
shiny_ronds(data = donnees_monoloc, fondMaille = depm, fondContour = fram, idData = "COD_DEP",
            varVolume = "POP_2015")
# Exemple 2
shiny_classes(data = donnees_monoloc, fondMaille = depm, fondContour = fram, idData = "COD_DEP",
              varRatio = "VAR_AN_MOY")
# Exemple 3
shiny_ronds_classes(data = donnees_monoloc, fondMaille = depm, fondContour = fram, idData = "COD_DEP",
                    varVolume = "POP_2015", varRatio = "VAR_AN_MOY")
# Exemple 4
shiny_classes_ronds(data = donnees_monoloc, fondMaille = depm, fondContour = fram, idData = "COD_DEP",
                    varVolume = "POP_2015", varRatio = "VAR_AN_MOY")
# Exemple 5
shiny_typo(data = donnees_monoloc, fondMaille = depm, fondContour = fram, idData = "COD_DEP",
           varTypo = "REG")

data("donnees_biloc")
# Exemple 6
shiny_oursins(data = donnees_biloc, fondMaille = regm, fondContour = fram, idDataDepart = "REG_DEPART",
              idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR")
# Exemple 7
shiny_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG", fondContour = fram,
                 idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR")

data("donnees_biloc_saphirs")
# Exemple 8
shiny_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG", fondContour = fram,
              idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", direction = "Ent")


# 1. Chargement de Base_E44_Export
library(dplyr)
DEP_0_EQTP <- lecture_fichier("U:/OCEANIS/package/DEP_0_EQTP.rds") %>% 
  # On cree la var de zonage avant cartographie
  mutate(ZONAGE_CARTO=substr(`Nom de la zone d etude`,1,4)) %>%
  # On convertit la tibble en data.frame
  as.data.frame()

#### 2. Import des fonds de carte ####
library(sf)
mesze <- st_read("//applishare.insee.fr/applishare/PROD/creacartes/pd/fichiers-ihm/2017/francemetro/z10_francemetro_2017.shp")
mesreg <- st_read("//applishare.insee.fr/applishare/PROD/creacartes/pd/fichiers-ihm/2017/francemetro/reg_francemetro_2017.shp")

# Carte de type analyse thematique + ronds
shiny_classes_ronds(data = DEP_0_EQTP, fondMaille = mesze, fondContour = mesze[mesze$code %in% DEP_0_EQTP$ZONAGE_CARTO,], 
                    idData = "ZONAGE_CARTO",varVolume = "Effectifs salaries dependants de la zone", 
                    varRatio = "Taux de dependance de la zone (en %)")

names(DEP_0_EQTP) <- c("nom","eff_dep","eff","taux","ZONAGE_CARTO")

shiny_classes_ronds(data = DEP_0_EQTP, fondMaille = mesze, fondContour = mesze[mesze$code %in% DEP_0_EQTP$ZONAGE_CARTO,], 
                    idData = "ZONAGE_CARTO",varVolume = "eff_dep", 
                    varRatio = "taux")

names(DEP_0_EQTP) <- c("Nomdelazonedetude","Effectifssalariesdependantsdelazone","eff","Tauxdedependancedelazone","ZONAGE_CARTO")

shiny_classes_ronds(data = DEP_0_EQTP, fondMaille = mesze, fondContour = mesze[mesze$code %in% DEP_0_EQTP$ZONAGE_CARTO,], 
                    idData = "ZONAGE_CARTO",varVolume = "Effectifssalariesdependantsdelazone", 
                    varRatio = "Tauxdedependancedelazone")

# Tests 15 avril 2019

# chargement des donn?es
print(system.file("data/donnees_monoloc.rda", package = "oceanis"))
donnees_monoloc <- lecture_fichier(file = system.file("data/donnees_monoloc.rda", package = "oceanis"))

# visualisation de la distribution de la variable VAR_AN_MOY
distrib_variable(data = donnees_monoloc ,varRatio = "VAR_AN_MOY", nbClasses = 4)

donnees_a_facon <- lecture_fichier(file = system.file("data/donnees_a_facon.rda", package = "oceanis"))
# fond communal des d?partements 13, 30, 83 et 84
com_dep_13_30_83_84 <- lecture_fichier(file = system.file("data/com_dep_13_30_83_84.rda", package = "oceanis"))

# chemin du fond de carte .shp
path_to_shp <- system.file("extdata","dep_francemetro_2018.shp", package = "oceanis")
# import en objet sf
depm <- st_read(dsn = path_to_shp, quiet = TRUE, stringsAsFactors = FALSE)

# cr?ation du zonage des zones d'emploi des Bouches-du-Rh?ne (partie enti?re des ze)
ze13etplus <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon,
                             idMaille = "DEPCOM", idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
                             fondContour = NULL, dom = "0")
# cr?ation du zonage des zones d'emploi des Bouches-du-Rh?ne (partie tronqu?e des ze au contour du d?partement)
ze13 <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon, idMaille = "DEPCOM",
                       idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
                       fondContour = depm[depm$code=="13",], dom = "0")

# visualisation de la g?om?trie
# modification des marges
par(mai = c(0,0,0,0))
# fond des ze des Bouches-du-Rh?ne en 1er pour fixer le niveau de zoom, en bleu
plot(st_geometry(ze13etplus), col = "powderblue", border = "transparent")
# fond de la partie tronqu?e des ze des Bouches-du-Rh?ne, en rouge
plot(st_geometry(ze13), col = "lightsalmon", border = "transparent", add = TRUE)
# contour des communes
plot(st_geometry(com_dep_13_30_83_84), col = "transparent", border = "lavender", add = TRUE)
# contour de la partie tronqu?e des ze des Bouches-du-Rh?ne, en rouge
plot(st_geometry(ze13), col = "transparent", border = "indianred", lwd = 3, add = TRUE)
# contour des ze des Bouches-du-Rh?ne, en bleu
plot(st_geometry(ze13etplus), col = "transparent", border = "steelblue", lwd = 3, add = TRUE)

library(oceanis)
library(sf)

# chargement des donn?es
donnees_monoloc <- lecture_fichier(file = system.file("data/donnees_monoloc.rda", package = "oceanis"))

# import du fond des d?partements
depm <- st_read(dsn = system.file("extdata","dep_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond des r?gions
regm <- st_read(dsn = system.file("extdata","reg_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond de France m?tropolitaine
fram <- st_read(dsn = system.file("extdata","francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)

shiny_classes_ronds(data = donnees_monoloc, fondMaille = depm[depm$reg %in% c("93","94"),], fondMailleElargi = depm, fondContour = fram, fondSuppl = regm, idData = "COD_DEP",varVolume = "POP_2015", varRatio = "VAR_AN_MOY")

# chargement des donn?es
donnees_biloc <- lecture_fichier(file = system.file("data/donnees_biloc.rda", package = "oceanis"))

# import du fond des r?gions
regm <- st_read(dsn = system.file("extdata","reg_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond des d?partements
depm <- st_read(dsn = system.file("extdata","dep_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond de France m?tropolitaine
fram <- st_read(dsn = system.file("extdata","francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)

shiny_joignantes(data = donnees_biloc, fondMaille = regm, typeMaille = "REG", fondContour = fram, fondSuppl = depm, idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", decalageAllerRetour = 10, decalageCentroid = 20)

library(oceanis)
library(sf)
library(leaflet)

# chargement des donn?es
donnees_monoloc <- lecture_fichier(file = system.file("data/donnees_monoloc.rda", package = "oceanis"))

# import du fond des d?partements
depm <- st_read(dsn = system.file("extdata","dep_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond des r?gions
regm <- st_read(dsn = system.file("extdata","reg_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)

# affichage de la carte
map <- leaflet_ronds_classes(data = donnees_monoloc, fondMaille = depm[depm$reg=="93",], fondMailleElargi = depm, fondSuppl = regm, idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY")

map

# affichage du rayon du rond le plus grand en m?tres
rayon_ronds(map)
## [1] 23668.46

# affichage de la carte avec des rayons de ronds plus grands
map <- leaflet_ronds_classes(data = donnees_monoloc, fondMaille = depm[depm$reg == "93",], fondMailleElargi = depm, fondSuppl = regm, idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", rayonRond = 29000, rapportRond = NULL)

# affichage du rapport du rond le plus grand, ? r?cup?rer pour permettre la comparaison de plusieurs cartes
rapport_ronds(map)
## [1] 1310165

map

# ajout de la l?gende des ronds (position par d?faut)
map <- add_legende_ronds(map = map, titre = "Population en 2015", zoom = 6)
## [INFO] Les coordonn?es de la l?gende des ronds sont : longitude (x) =
## 7.71612696800038 degr? ; latitude (y) = 45.1268480101767 degr?
# ajout de la l?gende des classes (position par d?faut)
map <- add_legende_classes(map = map, titre = "Variation ann.moy. 2010-2015", zoom = 6)
## [INFO] Les coordonn?es de la l?gende des classes sont : longitude (x) =
## 7.71612696800038 degr? ; latitude (y) = 42.9820332433454 degr?

map
# modification de la position de la l?gende des ronds et du niveau de zoom
map <- add_legende_ronds(map = map, titre = "Population en 2015", lng = 8.5, lat = 45, zoom = 8)
## [INFO] Les coordonn?es de la l?gende des ronds sont : longitude (x) = 8.5
## degr? ; latitude (y) = 45 degr?
# modification de la position de la l?gende des classes et du niveau de zoom
map <- add_legende_classes(map = map, titre = "Variation ann.moy. 2010-2015", lng = 8, lat = 44.5, zoom = 8)
## [INFO] Les coordonn?es de la l?gende des classes sont : longitude (x) = 8
## degr? ; latitude (y) = 44.5 degr?

map


# ajout d'une source ? la carte
map <- add_source(map = map, source = "Source : INSEE - RP2016")
# ajout d'un titre ? la carte
map <- add_titre(map = map, titre = "Population des d?partements de la r?gion Provence-Alpes-C?te d'Azur en 2015 et son ?volution depuis 2010")

map

# affichage de la palette par d?faut
recup_palette(stylePalette = "defaut")
## [[1]]
## [1] "#5A0A14" "#82141B" "#9B231C" "#B24B1D" "#D47130" "#E4A75A" "#F2CE93"
## 
## [[2]]
## [1] "#C9DAF0" "#95BAE2" "#5182B6" "#005289" "#003269" "#001E5A" "#000050"

# affichage de la palette InseePremiere
recup_palette(stylePalette = "InseePremiere")
## [[1]]
## [1] "#7F0029" "#CC1543" "#DE635B" "#F79C85" "#FDE3DE"
## 
## [[2]]
## [1] "#ECF4D8" "#CDD78C" "#91B778" "#549534" "#005941"

# modification de la couleur de bordure des ronds
map <- set_couleur_ronds(map = map, colBorder = "grey")
# modification du style de la palette
map <- set_couleur_classes(map = map, stylePalette = "InseePremiere")

map

# modification de l'opacit? de la repr?sentation ?largie
map <- set_opacite_elargi(map, opacite = 0.3)

map

# ajout d'un fond OpenStreetMap
map <- add_fond_osm(map)

map

# chargement des donn?es
donnees_biloc_saphirs <- lecture_fichier(file = system.file("data/donnees_biloc_saphirs.rda", package = "oceanis"))

# import du fond des r?gions
regm <- st_read(dsn = system.file("extdata","reg_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond de France m?tropolitaine
fram <- st_read(dsn = system.file("extdata","francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond des pays
paysm <- st_read(dsn = system.file("extdata","paysf_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond de mer
merm <- st_read(dsn = system.file("extdata","merf_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", direction = "Ent", titreLeg = "Entr?es", xLeg = 1100000, yLeg = 6470000, titreCarte = "Migrations r?sidentielles vers l'?le-de-France", sourceCarte = "Source : INSEE - RP2016", colEntree = "#D2691E", colBorder = "transparent", colBorderMaille = "grey")
## [INFO] La largeur maximale des fleches = 100[INFO] Les coordonnees de la
## legende sont x = 1100000 metres ; y = 6470000 metres


# construction de la table des ?tiquettes
etiquettes <- coordonnees_etiquettes(fondMaille = regm, listeCode = as.character(regm$code))
# modification des valeurs (latitude Y, longitude X, taille, couleur et style de police)
etiquettes[etiquettes$CODE=="24","Y"] <- 6680000
etiquettes[etiquettes$CODE=="27","Y"] <- 6660000
etiquettes[etiquettes$CODE=="28","X"] <- 410000
etiquettes[etiquettes$CODE=="32","Y"] <- 7015000
etiquettes[etiquettes$CODE=="44","X"] <- 955000
etiquettes[etiquettes$CODE=="52","X"] <- 330000
etiquettes[etiquettes$CODE=="52","Y"] <- 6700000
etiquettes[etiquettes$CODE=="53","X"] <- 215000
etiquettes[etiquettes$CODE=="75","Y"] <- 6420000
etiquettes[etiquettes$CODE=="76","Y"] <- 6270000
etiquettes[etiquettes$CODE=="84","Y"] <- 6455000
etiquettes[etiquettes$CODE=="93","Y"] <- 6290000
etiquettes[etiquettes$CODE=="94","Y"] <- 6120000
etiquettes[etiquettes$CODE!="11","TAILLE"] <- 0.6
etiquettes[etiquettes$CODE=="11","COL"] <- "#002D7F"
etiquettes[etiquettes$CODE!="11","FONT"] <- 1

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", direction = "Ent", titreLeg = "Entr?es", xLeg = 1150000, yLeg = 6470000, titreCarte = "Migrations r?sidentielles vers l'?le-de-France", sourceCarte = "Source : INSEE - RP2016", etiquettes = etiquettes, colEntree = "#D2691E", colBorder = "transparent", colBorderMaille = "grey")
## [INFO] La table d'?tiquettes fournie contient au moins 4 colonnes.
## Elles doivent ?tre : l'identifiant de la maille ('CODE'), le libell? de
## l'?tiquette ('LIBELLE') et les coordonn?es 'X' et 'Y' de l'?tiquette. Peut
## etre ajout?es ?ventuellement les colonnes 'TAILLE', 'FONT' et 'COL'.[INFO]
## La largeur maximale des fleches = 100[INFO] Les coordonnees de la legende
## sont x = 1150000 metres ; y = 6470000 metres


# ajout de colonnes dans les fonds pour modifier leur apparence
# couleur de remplissage : COL
# couleur des contours : BORDER
# ?paisseur des contours : EPAISEEUR
merm$COL <- "lightsteelblue"
merm$BORDER <- "lightsteelblue"
paysm$COL <- "gray"
paysm$BORDER <- "white"
fram$BORDER <- "darkgray"
fram$EPAISSEUR <- 2

# cr?ation des listes des fonds d'habillage, en-dessous et au-dessus de l'analyse
fondSousAnalyse <- list(merm,paysm)
fondSurAnalyse <- list(fram)

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, fondSousAnalyse = fondSousAnalyse, fondSurAnalyse = fondSurAnalyse,typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", direction = "Ent", titreLeg = "Entr?es", xLeg = 1150000, yLeg = 6470000, titreCarte = "Migrations r?sidentielles vers l'?le-de-France", sourceCarte = "Source : INSEE - RP2016", etiquettes = etiquettes, colEntree = "#D2691E", colBorder = "transparent", colBorderMaille = "grey")
## [INFO] La table d'?tiquettes fournie contient au moins 4 colonnes.
## Elles doivent ?tre : l'identifiant de la maille ('CODE'), le libell? de
## l'?tiquette ('LIBELLE') et les coordonn?es 'X' et 'Y' de l'?tiquette. Peut
## etre ajout?es ?ventuellement les colonnes 'TAILLE', 'FONT' et 'COL'.[INFO]
## La largeur maximale des fleches = 100[INFO] Les coordonnees de la legende
## sont x = 1150000 metres ; y = 6470000 metres

library(grDevices)

jpeg(filename = "sortie.jpg", quality = 100, width = 16, height = 18, units = "cm", res = 120)

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, fondSousAnalyse = fondSousAnalyse, fondSurAnalyse = fondSurAnalyse,typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", direction = "Ent", titreLeg = "Entr?es", xLeg = 1150000, yLeg = 6470000, titreCarte = "Migrations r?sidentielles vers l'?le-de-France", sourceCarte = "Source : INSEE - RP2016", etiquettes = etiquettes, colEntree = "#D2691E", colBorder = "transparent", colBorderMaille = "grey")

dev.off()

pdf(file = "sortie.pdf", width = 10, height = 10)

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs, fondMaille = regm, fondSousAnalyse = fondSousAnalyse, fondSurAnalyse = fondSurAnalyse,typeMaille = "REG", idDataDepart = "REG_DEPART", idDataArrivee = "REG_ARRIVEE", varFlux = "MIGR", direction = "Ent", titreLeg = "Entr?es", xLeg = 1150000, yLeg = 6470000, titreCarte = "Migrations r?sidentielles vers l'?le-de-France", sourceCarte = "Source : INSEE - RP2016", etiquettes = etiquettes, colEntree = "#D2691E", colBorder = "transparent", colBorderMaille = "grey")

dev.off()

library(oceanis)
library(sf)
library(leaflet)

# chargement des donn?es
donnees_monoloc <- lecture_fichier(file = system.file("data/donnees_monoloc.rda", package = "oceanis"))

# import du fond des d?partements
depm <- st_read(dsn = system.file("extdata","dep_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)
# import du fond des r?gions
regm <- st_read(dsn = system.file("extdata","reg_francemetro_2018.shp", package = "oceanis"), quiet = TRUE, stringsAsFactors = FALSE)

# affichage de la carte avec des rayons de ronds plus grands
map <- leaflet_ronds_classes(data = donnees_monoloc, fondMaille = depm[depm$reg == "93",], fondMailleElargi = depm, fondSuppl = regm, idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", rayonRond = 29000, rapportRond = NULL)

# modification de la position de la l?gende des ronds et du niveau de zoom
map <- add_legende_ronds(map = map, titre = "Population en 2015", lng = 8.5, lat = 45, zoom = 8)
# modification de la position de la l?gende des classes et du niveau de zoom
map <- add_legende_classes(map = map, titre = "Variation ann.moy. 2010-2015", lng = 8, lat = 44.5, zoom = 8)
# ajout d'une source ? la carte
map <- add_source(map = map, source = "Source : INSEE - RP2016")
# ajout d'un titre ? la carte
map <- add_titre(map = map, titre = "Population des d?partements de la r?gion Provence-Alpes-C?te d'Azur en 2015 et son ?volution depuis 2010")
# modification de la couleur de bordure des ronds
map <- set_couleur_ronds(map = map, colBorder = "grey")
# modification du style de la palette
map <- set_couleur_classes(map = map, stylePalette = "InseePremiere")
# modification de l'opacit? de la repr?sentation ?largie
map <- set_opacite_elargi(map, opacite = 0.3)
map

export_qgis_ronds_classes(map, cheminDossier = getwd(), nomFichier = "export_carte_rp_ac", titre1 = "Population des d?partements de la r?gion Provence-Alpes-C?te d'Azur en 2015 et son ?volution depuis 2010", titre2 = "", source = "Source : INSEE - RP2016")
