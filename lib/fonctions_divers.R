library(utils)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(DT)
library(foreign)
library(sf)
library(sfc)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(classInt)
library(units)
library(grDevices)
library(readODS)
library(xlsx)
library(ggplot2)
library(dplyr)
library(graphics)
library(mapview)
library(tidyr)
library(munsell)
library(lwgeom)

donnees_monoloc <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/donnees_monoloc.RDS")
donnees_biloc <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/donnees_biloc.RDS")
donnees_biloc_saphirs <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/donnees_biloc_saphirs.RDS")
com_dep_13_30_83_84 <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/com_dep_13_30_83_84.RDS")
donnees_a_facon <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/donnees_a_facon.RDS")
fram <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/fram.RDS")
regm <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/regm.RDS")
depm <- readRDS("D:/applicarto/oceanis_init/OCEANIS_shiny/FONCTIONS/bin/fonds RDS/depm.RDS")

#####################################
### RP/AC/RP_AC/AC_RP/TY/OU/FJ/FS ###
#####################################

zonage_a_facon <-
  function(fondMaille,groupe,idMaille,idGroupe,libGroupe,fondContour=NULL,dom="0")
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10 <- NULL
    
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error1 <- "Le fond de maille doit etre un objet sf / "
    if(any(class(groupe)!="data.frame")) msg_error2 <- "Le groupe doit etre un data.frame / "
    if(any(class(idMaille)!="character")) msg_error3 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idGroupe)!="character")) msg_error4 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(libGroupe)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(fondContour)) if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error1 <- "Le fond du contour doit etre un objet sf / "
    if(any(class(dom)!="character")) msg_error6 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    
    if(!any(names(groupe) %in% idMaille))  msg_error7 <- "La variable identifiant la maille n'existe pas dans la table / "
    if(!any(names(groupe) %in% idGroupe))  msg_error8 <- "La variable identifiant le groupe n'existe pas dans la table / "
    if(!any(names(groupe) %in% libGroupe))  msg_error9 <- "La variable libelle du groupe n'existe pas dans la table / "
    if(length(names(fondMaille))<3) msg_error10 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),
           !is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),!is.null(msg_error9),!is.null(msg_error10)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,msg_error9,msg_error10)))
    }
    
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    
    groupe <- groupe[order(groupe[,idGroupe]),]
    liste_code <- groupe[,idMaille]
    zonage_a_facon <- fondMaille[fondMaille$CODE %in% liste_code,]
    
    if(nrow(zonage_a_facon)==0)
    {
      stop(simpleError("Les identifiants des mailles a regrouper ne correspondent pas aux identifiants du fond de carte."))
    }
    
    zonage_a_facon <- merge(zonage_a_facon,groupe,by.x="CODE",by.y=idMaille)
    if(length(names(zonage_a_facon)[grep("..y",names(zonage_a_facon))])>1)
    {
      zonage_a_facon <- zonage_a_facon[,-grep("..y",names(zonage_a_facon))]
      names(zonage_a_facon)[grep("..x",names(zonage_a_facon))]<-sub(".x","",names(zonage_a_facon)[grep("..x",names(zonage_a_facon))])
    }
    
    zonage_a_facon <- st_sf(zonage_a_facon[,c("CODE","LIBELLE",idGroupe,libGroupe,"geometry")],stringsAsFactors = FALSE)
    names(zonage_a_facon) <- c("CODE","LIBELLE","CODE_TERR","LIB_TERR","geometry")
    zonage_a_facon <- zonage_a_facon[order(as.data.frame(zonage_a_facon)[,"CODE_TERR"]),]
    
    zonage_a_facon_split <- split(zonage_a_facon,as.factor(zonage_a_facon$CODE_TERR))
    
    assign(paste0("zonage_a_facon_",1),st_union(zonage_a_facon_split[[1]],by_feature = FALSE))
    zonage_a_facon <- st_sf(geometry=get(paste0("zonage_a_facon_",1)))
    
    for(i in 2:length(zonage_a_facon_split))
    {
      assign(paste0("zonage_a_facon_",i),st_union(zonage_a_facon_split[[i]],by_feature = FALSE))
      zonage_a_facon <- unique(rbind(zonage_a_facon,get(paste0("zonage_a_facon_",i))))
    }
    
    zonage_a_facon <- cbind(CODE_TERR=unique(groupe[,idGroupe]),LIB_TERR=unique(groupe[,libGroupe]),zonage_a_facon)
    
    if(!is.null(fondContour))
    {
      st_agr(fondContour) <- "constant"
      st_agr(zonage_a_facon) <- "constant"
      zonage_a_facon <- st_intersection(zonage_a_facon,fondContour)
      if(length(names(zonage_a_facon)[grep("..1",names(zonage_a_facon))])>0)
      {
        zonage_a_facon <- zonage_a_facon[,-grep("..1",names(zonage_a_facon))]
      }
    }
    
    return(zonage_a_facon)
  }

add_titre <-
  function(map, titre, sousTitre=NULL)
  {
    msg_error1<-msg_error2 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(titre)) if(any(class(titre)!="character")) msg_error2 <- "Le titre doit etre de type caractere / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }
    
    idx_titre <- NULL
    idx_soustitre <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(any(map$x$calls[[i]]$args[[4]]=="map-title")) idx_titre <- i
        if(any(map$x$calls[[i]]$args[[4]]=="map-subtitle")) idx_soustitre <- i
      }
    }
    
    tag.map.title <- tags$style(HTML("
                                     .leaflet-control.map-title {
                                     position: fixed !important;
                                     left: 8%;
                                     text-align: center;
                                     background: rgba(255,255,255,0.75);
                                     font-weight: bold;
                                     font-size: 16px;
                                     }
                                     "))
    
    tag.map.subtitle <- tags$style(HTML("
                                        .leaflet-control.map-subtitle {
                                        position: fixed !important;
                                        left: 15%;
                                        top: 10%;
                                        text-align: center;
                                        background: rgba(255,255,255,0.75);
                                        font-size: 14px;
                                        }
                                        "))
    
    if(!is.null(titre))
    {
      titre <- iconv(titre,"latin1","utf8")
      
      title <- tags$div(
        tag.map.title, HTML(titre)
      )
      
      if(!is.null(sousTitre))
      {
        sousTitre <- iconv(sousTitre,"latin1","utf8")
        
        subtitle <- tags$div(
          tag.map.subtitle, HTML(sousTitre)
        )
      }
      
      if(is.null(idx_titre)) # on n'a pas encore mis de titre
      {
        map <- addControl(map = map, title, position = "topleft", className="map-title")
      }else # un titre existe deja, on le remplace
      {
        ancien_titre <- map$x$calls[[idx_titre]]$args[[1]]
        map$x$calls[[idx_titre]]$args[[1]] <- paste0(
          substr(ancien_titre,1,str_locate(ancien_titre,"</style>\n")[2]+2),
          titre,
          substr(ancien_titre,str_locate(ancien_titre,"\n</div>")[1],nchar(ancien_titre))
        )
      }
      
      if(is.null(idx_soustitre)) # on n'a pas mis de sous-titre
      {
        if(!is.null(sousTitre)) map <- addControl(map = map, subtitle, position = "topleft", className="map-subtitle")
      }else # un sous-titre existe deja
      {
        if(!is.null(sousTitre)) # on le remplace
        {
          ancien_soustitre <- map$x$calls[[idx_soustitre]]$args[[1]]
          map$x$calls[[idx_soustitre]]$args[[1]] <- paste0(
            substr(ancien_soustitre,1,str_locate(ancien_soustitre,"</style>\n")[2]+2),
            sousTitre,
            substr(ancien_soustitre,str_locate(ancien_soustitre,"\n</div>")[1],nchar(ancien_soustitre))
          )
        }else # on supprime le sous-titre
        {
          map$x$calls[[idx_soustitre]]$args[[1]] <- ""
          map$x$calls[[idx_soustitre]]$args[[4]] <- ""
        }
      }
    }else # le titre est NULL, on le supprime si il existe
    {
      if(!is.null(idx_titre)) # un titre existe deja, on le supprime
      {
        map$x$calls[[idx_titre]]$args[[1]] <- ""
        map$x$calls[[idx_titre]]$args[[4]] <- ""
        if(!is.null(idx_soustitre))
        {
          map$x$calls[[idx_soustitre]]$args[[1]] <- ""
          map$x$calls[[idx_soustitre]]$args[[4]] <- ""
        }
      }
    }
    
    return(map)
  }


add_source <-
  function(map, source)
  {
    msg_error1<-msg_error2 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(source)) if(any(class(source)!="character")) msg_error2 <- "La source doit etre de type caractere / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }
    
    idx_source <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(any(map$x$calls[[i]]$args[[4]]=="map-sourc")) idx_source <- i
      }
    }
    
    tag.map.sourc <- tags$style(HTML("
                                     .leaflet-control.map-sourc {
                                     position: fixed !important;
                                     left: 2%;
                                     text-align: center;
                                     font-size: 12px;
                                     }
                                     "))
    
    if(!is.null(source))
    {
      source <- iconv(source,"latin1","utf8")
      
      sourc <- tags$div(
        tag.map.sourc, HTML(source)
      )
      
      if(is.null(idx_source)) # on n'a pas encore mis de source
      {
        map <- addControl(map = map, sourc, position = "bottomright", className="map-sourc")
      }else # une source existe deja, on la remplace
      {
        ancienne_source <- map$x$calls[[idx_source]]$args[[1]]
        map$x$calls[[idx_source]]$args[[1]] <- paste0(
          substr(ancienne_source,1,str_locate(ancienne_source,"</style>\n")[2]+2),
          source,
          substr(ancienne_source,str_locate(ancienne_source,"\n</div>")[1],nchar(ancienne_source))
        )
      }
    }else # la source est NULL, on la supprime si elle existe
    {
      if(!is.null(idx_source))# une source existe deja, on la supprime
      {
        map$x$calls[[idx_source]]$args[[1]] <- ""
        map$x$calls[[idx_source]]$args[[4]] <- ""
      }
    }
    
    return(map)
  }


lecture_fichier <-
  function(file)
  {
    msg_error1 <- NULL
    
    if(any(class(file)!="character")) msg_error1 <- "Le file doit etre une chaC.ne de caracteres / "
    
    if(!is.null(msg_error1))
    {
      stop(simpleError(msg_error1))
    }
    
    fichiers <- NULL
    
    if(str_sub(file,start=-4) %in% c(".dbf","DBF")) fichiers <- read.dbf(file, as.is=T)
    if(str_sub(file,start=-4) %in% c(".xls","XLS")) fichiers <- read.xlsx(file, sheetIndex = 1)
    if(str_sub(file,start=-4) %in% c(".ods",".ODS")) fichiers <- read_ods(file)
    if(str_sub(file,start=-4) %in% c(".csv",".CSV"))
    {
      fichiers <- read.table(file, sep=",", quote = "\"")
      names_col <- names(read.csv(file, sep=",", quote = "\""))
      names(fichiers) <- names_col
      if(names_col[1]=="X") fichiers <- fichiers[,-1]
    }
    if(str_sub(file,start=-4) %in% c(".rds",".RDS")) fichiers <- readRDS(file)
    if(str_sub(file,start=-4) %in% c(".rda",".RDA",".rdata",".RData","RDATA"))
    {
      nom_fichier <- load(file)
      fichiers <- get(nom_fichier)
      rm(nom_fichier)
    }
    
    if(is.null(fichiers))
    {
      stop(simpleError("Impossible de lire le fichier. Verifier svp le chemin d'acces au fichier."))
    }else
    {
      return(fichiers)
    }
  }


export_png <-
  function(map,chemin,nomFichier)
  {
    mapshot(map, file = paste0(chemin,"/",nomFichier,".png"))
  }


export_jpeg <-
  function(map,chemin,nomFichier)
  {
    mapshot(map, file = paste0(chemin,"/",nomFichier,".jpeg"))
  }


export_pdf <-
  function(map,chemin,nomFichier)
  {
    mapshot(map, file = paste0(chemin,"/",nomFichier,".pdf"))
  }


addTiles_insee <-
  function (map, urlTemplate = "",
            type = NULL, attribution = NULL, layerId = NULL, group = NULL, options = tileOptions())
  {
    options$value = type
    options$attribution = attribution
    invokeMethod(map, getMapData(map), "addTiles", urlTemplate,
                 layerId, group, options)
  }


add_fond_osm <-
  function(map)
  {
    msg_error1 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1)))
    {
      stop(simpleError(msg_error1))
    }
    
    j <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        j <- c(j,i)
      }
    }
    map$x$calls <- map$x$calls[-j[c(1,2)]]
    
    map$x$calls[[4]]$args[[4]]$weight <- 2
    
    map <- addTiles(map,
                    urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                    attribution = "OCEANIS - <a href=\"http://www.insee.fr\">INSEE</a>")
    
    return(map)
  }


coordonnees_etiquettes <-
  function(fondMaille,listeCode)
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2 <- NULL
    
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error1 <- "Le fond de maille doit etre un objet sf / "
    if(any(class(listeCode)!="character")) msg_error2 <- "La liste de code doit etre un vecteur de type caractere / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }
    
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    
    fondEtiquettes <- fondMaille[fondMaille$CODE %in% listeCode,c("CODE","LIBELLE")]
    if(nrow(fondEtiquettes)==0) stop(simpleError("Aucun CODE a etiqueter n'a ete trouve dans fondMaille."))
    suppressWarnings(centroid <- st_coordinates(st_centroid(st_geometry(fondEtiquettes))))
    tableEtiquettes <- as.data.frame(fondEtiquettes)[,c("CODE","LIBELLE")]
    tableEtiquettes <- cbind(tableEtiquettes,centroid)
    tableEtiquettes$TAILLE <- 0.9
    tableEtiquettes$FONT <- 2
    tableEtiquettes$COL <- "black"
    
    return(tableEtiquettes)
  }


table_etiquettes <-
  function(fondMaille,etiquettes)
  {
    etiquettes <- data.frame(etiquettes, stringsAsFactors = FALSE)
    names(etiquettes)[1] <- "CODE"
    if(ncol(etiquettes)==1) # table avec "CODE"
    {
      tableEtiquettes <- coordonnees_etiquettes(fondMaille,etiquettes$CODE)
    }else if(ncol(etiquettes)==3) # table avec "CODE", "X", "Y"
    {
      message(simpleMessage(paste0("[INFO] La table d'\u00e9tiquettes ne contient que 3 colonnes, qui doivent \u00eatre : l'identifiant de la maille ('CODE') et les coordonn","\u00e9","es 'X' et 'Y' de l'\u00e9tiquette.")))
      names(etiquettes)[2] <- "X"
      names(etiquettes)[3] <- "Y"
      tableEtiquettes <- as.data.frame(fondMaille)[fondMaille$CODE %in% etiquettes$CODE,c("CODE","LIBELLE")]
      if(nrow(tableEtiquettes)==0) stop(simpleError("Aucun CODE a etiqueter n'a ete trouve dans fondMaille."))
      tableEtiquettes <- cbind(tableEtiquettes,etiquettes$X,etiquettes$Y)
      tableEtiquettes$TAILLE <- 0.9
      tableEtiquettes$FONT <- 2
      tableEtiquettes$COL <- "black"
    }else
    {
      message(simpleMessage(paste0("[INFO] La table d'\u00e9tiquettes fournie contient au moins 4 colonnes. Elles doivent \u00eatre : l'identifiant de la maille ('CODE'), le libell\u00e9 de l'\u00e9tiquette ('LIBELLE') et les coordonn","\u00e9","es 'X' et 'Y' de l'\u00e9tiquette. Peut etre ajout","\u00e9","es \u00e9ventuellement les colonnes 'TAILLE', 'FONT' et 'COL'.")))
      msg_error1<-msg_error2<-msg_error3 <- NULL
      if(!any(names(etiquettes) %in% "LIBELLE"))  msg_error1 <- "La table d'etiquettes doit contenir une colonne nommee LIBELLE / "
      if(!any(names(etiquettes) %in% "X"))  msg_error2 <- "La table d'etiquettes doit contenir une colonne nommee X / "
      if(!any(names(etiquettes) %in% "Y"))  msg_error3 <- "La table d'etiquettes doit contenir une colonne nommee Y / "
      if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
      {
        stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
      }
      
      if(!any(names(etiquettes) %in% "TAILLE")) tableEtiquettes$TAILLE <- 0.9
      if(!any(names(etiquettes) %in% "FONT")) tableEtiquettes$FONT <- 2
      if(!any(names(etiquettes) %in% "COL")) tableEtiquettes$COL <- "black"
      
      tableEtiquettes <- etiquettes
    }
    
    return(tableEtiquettes)
  }


modification <-
  function(canevas,modifs)
  {
    for (i in 1:dim(canevas)[1])
    {
      for (j in 1:dim(modifs)[1])
      {
        canevas[i,1]=str_replace(canevas[i,1],modifs[j,1],modifs[j,2])
      }
    }
    return(canevas)
  }


modif_canevas <-
  function(xmin,xmax,ymin,ymax,projproj,nbfonds)
  {
    canevas<-balises_qgis()[[1]]
    
    modifs=data.frame(c("XXXMIN","XXXMAX","YYYMIN","YYYMAX","PROJECTIONPROJET","NOMBREDEFONDS"),stringsAsFactors=F)
    modifs[1,2]=xmin
    modifs[2,2]=xmax
    modifs[3,2]=ymin
    modifs[4,2]=ymax
    modifs[5,2]=projproj
    modifs[6,2]=nbfonds
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(canevas,modifs)
    return(qgs)
  }


modif_canevas_MEP <-
  function(nommep,tit,sstit,an,sourc,xmin,xmax,ymin,ymax)
  {
    canevas_MEP<-balises_qgis()[[2]]
    
    modifs=data.frame(c("NOMMEP","TITRE","SSTIT","AN","SOURCE","XXXMIN","XXXMAX","YYYMIN","YYYMAX"),stringsAsFactors=F)
    modifs[1,2]=nommep
    modifs[2,2]=tit
    modifs[3,2]=sstit
    modifs[4,2]=an
    modifs[5,2]=sourc
    modifs[6,2]=xmin
    modifs[7,2]=xmax
    modifs[8,2]=ymin
    modifs[9,2]=ymax
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(canevas_MEP,modifs)
    return(qgs)
  }


modif_blocleg <-
  function(nomcouche,idcouche)
  {
    blocleg<-balises_qgis()[[3]]
    
    modifs=data.frame(c("NOMDELACOUCHE","ID_COUCHE"),stringsAsFactors=F)
    modifs[1,2]=nomcouche
    modifs[2,2]=idcouche
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocleg,modifs)
    return(qgs)
  }


modif_bloclayeritem <-
  function(nomcouche,idcouche,styl)
  {
    bloclayerItem<-balises_qgis()[[9]]
    
    modifs=data.frame(c("NOMDELACOUCHE","ID_COUCHE","STYLELEG","NOMDELACOUCHE"),stringsAsFactors=F)
    modifs[1,2]=nomcouche
    modifs[2,2]=idcouche
    modifs[3,2]=styl
    modifs[4,2]=nomcouche
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(bloclayerItem,modifs)
    return(qgs)
  }


modif_blocvectorClassification <-
  function(text)
  {
    blocvectorClassification<-balises_qgis()[[10]]
    
    modifs=data.frame(c("TEXT"),stringsAsFactors=F)
    modifs[1,2]=text
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocvectorClassification,modifs)
    return(qgs)
  }


modif_blocprojectlayers <-
  function(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
  {
    blocprojectlayers<-balises_qgis()[[4]]
    
    modifs=data.frame(c("GEOMETRIE","IDCOUCHE","CHEMINCOUCHE","NOMDELACOUCHE","PROJECTIONDELACOUCHE","ATTR","TYPEANALYSE"),stringsAsFactors=F)
    modifs[1,2]=geometrie
    modifs[2,2]=idcouche
    modifs[3,2]=chemincouche
    modifs[4,2]=nomcouche
    modifs[5,2]=projcouche
    modifs[6,2]=attr
    modifs[7,2]=typeanalyse
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocprojectlayers,modifs)
    return(qgs)
  }


modif_blocsymbolsPolygon <-
  function(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
  {
    blocsymbolsPolygon<-balises_qgis()[[5]]
    
    modifs=data.frame(c("COULEURFOND","COULEURBORDURE","REMPLISSAGEFOND","STYLEBORDURE","EPAISSEURBORDURE","NAME"),stringsAsFactors=F)
    modifs[1,2]=couleurfond
    modifs[2,2]=couleurbordure
    modifs[3,2]=remplissagefond
    modifs[4,2]=stylebordure
    modifs[5,2]=epaisseurbordure
    modifs[6,2]=name
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocsymbolsPolygon,modifs)
    return(qgs)
  }


modif_blocsymbolsLine <-
  function(couleurbordure,stylebordure,epaisseurbordure,name)
  {
    blocsymbolsLine<-balises_qgis()[[7]]
    
    modifs=data.frame(c("COULEURBORDURE","STYLEBORDURE","EPAISSEURBORDURE","NAME"),stringsAsFactors=F)
    modifs[1,2]=couleurbordure
    modifs[2,2]=stylebordure
    modifs[3,2]=epaisseurbordure
    modifs[4,2]=name
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(blocsymbolsLine,modifs)
    return(qgs)
  }


modif_bloccategories <-
  function(symbol,value,label)
  {
    bloccategories<-balises_qgis()[[8]]
    
    modifs=data.frame(c("SYMBOL","VALUE","LABEL"),stringsAsFactors=F)
    modifs[1,2]=symbol
    modifs[2,2]=value
    modifs[3,2]=label
    modifs[,2]=as.character(modifs[,2])
    qgs=modification(bloccategories,modifs)
    return(qgs)
  }


##################################
### RP/AC/RP_AC/AC_RP/TY/FJ/FS ###
##################################

coord_legende <-
  function(map)
  {
    msg_error1 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1)))
    {
      stop(simpleError(msg_error1))
    }
    
    ronds <- FALSE
    lng_ronds <- NULL
    lat_ronds <- NULL
    if(any(map$x$calls[[4]]$args[[3]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds")))
    {
      ronds <- TRUE
    }
    
    if(ronds) # La map comporte une une analyse en ronds
    {
      j <- NULL
      for(i in 1:length(map$x$calls)) # On compte le nombre d'objets circle dans le leaflet
      {
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          j <- c(j,i)
        }
      }
      
      if(length(j)==1) # La legende n'a pas encore ete creee, on recupere une position par defaut de la legende
      {
        lng_classes <- map$x$fitBounds[[4]]
        lat_classes <- map$x$fitBounds[[3]]
      }else # On recupere la derniere position connue de la legende
      {
        lng_ronds <- map$x$calls[[j[length(j)]]]$args[[2]][[1]]
        lat_ronds <- map$x$calls[[j[length(j)]]]$args[[1]][[1]]
      }
    }
    
    classes <- FALSE
    lng_classes <- NULL
    lat_classes <- NULL
    if(any(map$x$calls[[4]]$args[[3]]$nom_couche %in% c("carte_classes","carte_ronds_classes","carte_classes_ronds","carte_typo")))
    {
      classes <- TRUE
    }
    
    if(classes) # La map comporte une une analyse en classes
    {
      j <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          j <- c(j,i)
        }
      }
      
      if(length(j)<5) # La legende n'a pas encore ete creee, on recupere une position par defaut de la legende
      {
        lng_classes <- map$x$fitBounds[[4]]
        lat_classes <- map$x$fitBounds[[1]]
      }else # On recupere la derniere position connue de la legende
      {
        lng_classes <- max(map$x$calls[[j[length(j)]]]$args[[1]][[1]][[1]][[1]]$lng)
        lat_classes <- max(map$x$calls[[j[length(j)]]]$args[[1]][[1]][[1]][[1]]$lat)
      }
    }
    
    fleches <- FALSE
    lng_fleches <- NULL
    lat_fleches <- NULL
    if(any(map$x$calls[[4]]$args[[3]]$nom_couche %in% c("carte_joignantes","carte_saphirs")))
    {
      fleches <- TRUE
    }
    
    if(fleches) # La map comporte une une analyse en ronds
    {
      j <- NULL
      for(i in 1:length(map$x$calls)) # On compte le nombre d'objets circle dans le leaflet
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          j <- c(j,i)
        }
      }
      
      if(length(j)<5) # La legende n'a pas encore ete creee, on recupere une position par defaut de la legende
      {
        lng_fleches <- map$x$fitBounds[[4]]
        lat_fleches <- map$x$fitBounds[[3]]
      }else # On recupere la derniere position connue de la legende
      {
        lng_fleches <- min(map$x$calls[[j[length(j)]]]$args[[1]][[2]][[1]][[1]]$lng)
        lat_fleches <- min(map$x$calls[[j[length(j)]]]$args[[1]][[2]][[1]][[1]]$lat)
      }
    }
    
    coord_leg <- data.frame(X=c(lng_ronds,lng_classes,lng_fleches),Y=c(lat_ronds,lat_classes,lat_fleches))
    if(ronds & classes)
    {
      rownames(coord_leg) <- c("coord_leg_ronds","coord_leg_classes")
    }else if(ronds)
    {
      rownames(coord_leg) <- c("coord_leg_ronds")
    }else if(classes)
    {
      rownames(coord_leg) <- c("coord_leg_classes")
    }
    if(fleches) rownames(coord_leg) <- c("coord_leg_fleches")
    
    return(coord_leg)
  }


######################
### RP/RP_AC/AC_RP ###
######################

k_ronds <-
  function(fond_carto_k,fond_carto_elargi_k,variable_jointure_fond_carto_k,donnees_k,variable_jointure_donnees_k,variable_a_representer_k,elargi,choix_centroid,centroid)
  {
    donnees_k$save <- donnees_k[,variable_a_representer_k]
    donnees_k[,variable_a_representer_k] <- abs(as.numeric(donnees_k[,variable_a_representer_k]))
    suppressWarnings(donnees_k<-donnees_k[!is.na(donnees_k[,variable_a_representer_k]),])
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
    base <- merge(as.data.frame(fond_carto_k)[,-length(names(fond_carto_k))],donnees_k,by.x=variable_jointure_fond_carto_k,by.y=variable_jointure_donnees_k)
    
    if(length(names(base)[grep("..y",names(base))])>0)
    {
      base2 <- base[,-grep("..y",names(base))]
      names(base)[grep("..x",names(base))]<-sub(".x","",names(base)[grep("..x",names(base))])
    }
    if(nrow(base)>0)
    {
      base <- base[abs(base[,variable_a_representer_k])>=0,]
      base <- base[order(base[,variable_a_representer_k],decreasing = T),]
      rownames(base) <- as.character(1:dim(base)[1])
    }else
    {
      return(NULL)
    }
    base$id <- c(1:nrow(base))
    base <- base[order(base[,variable_jointure_fond_carto_k]),]
    fond_carto_k <- merge(base[,c(variable_jointure_fond_carto_k,"id")],fond_carto_k,by=variable_jointure_fond_carto_k)
    fond_carto_k <- fond_carto_k[order(fond_carto_k$id),]
    fond_carto_k <- fond_carto_k[,-2]
    fond_carto_k <- fond_carto_k[!duplicated(fond_carto_k$CODE),]
    
    base <- base[order(base$id),]
    base <- base[,-(ncol(base))]
    base <- unique(base)
    
    if(choix_centroid=="centroid")
    {
      suppressWarnings(centroid_analyse <- st_centroid(st_geometry(st_as_sf(fond_carto_k))))
    }else
    {
      centroid_analyse <- centroid[match(fond_carto_k$CODE,centroid$CODE),]
    }
    
    if(elargi)
    {
      fond_carto_elargi_k <- fond_carto_elargi_k[as.data.frame(fond_carto_elargi_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
      
      base_elargi <- merge(as.data.frame(fond_carto_elargi_k)[,-length(names(fond_carto_elargi_k))],donnees_k,by.x=variable_jointure_fond_carto_k,by.y=variable_jointure_donnees_k)
      if (length(names(base_elargi)[grep("..y",names(base_elargi))])>0)
      {
        base_elargi <- base_elargi[,-grep("..y",names(base_elargi))]
        names(base_elargi)[grep("..x",names(base_elargi))]<-sub(".x","",names(base_elargi)[grep("..x",names(base_elargi))])
      }
      
      base_elargi <- base_elargi[abs(base_elargi[,variable_a_representer_k])>=0,]
      base_elargi <- base_elargi[order(base_elargi[,variable_a_representer_k],decreasing = T),]
      rownames(base_elargi) <- as.character(1:dim(base_elargi)[1])
      
      base_elargi$id <- c(1:nrow(base_elargi))
      base_elargi <- base_elargi[order(base_elargi[,variable_jointure_fond_carto_k]),]
      fond_carto_elargi_k <- merge(base_elargi[,c(variable_jointure_fond_carto_k,"id")],fond_carto_elargi_k,by=variable_jointure_fond_carto_k)
      fond_carto_elargi_k <- fond_carto_elargi_k[order(fond_carto_elargi_k$id),]
      fond_carto_elargi_k <- fond_carto_elargi_k[,-2]
      fond_carto_elargi_k <- fond_carto_elargi_k[!duplicated(fond_carto_elargi_k$CODE),]
      
      base_elargi <- base_elargi[order(base_elargi$id),]
      base_elargi <- base_elargi[,-(ncol(base_elargi))]
      base_elargi <- unique(base_elargi)
      
      if(choix_centroid=="centroid")
      {
        suppressWarnings(centroid_elargi <- st_centroid(st_geometry(st_as_sf(fond_carto_elargi_k))))
      }else
      {
        centroid_elargi <- centroid[match(fond_carto_elargi_k$CODE,centroid$CODE),]
      }
      
      return(list(analyse_points=centroid_analyse,donnees=base,analyse_points_elargi=centroid_elargi,donnees_elargi=base_elargi))
    }else
    {
      return(list(analyse_points=centroid_analyse,donnees=base))
    }
  }


construction_ronds_legende <-
  function(lon,lat,code_epsg,taille_rond_m)
  {
    centres_leg <- t(data_frame(c(lon,lat)))
    
    #On cree les points en WGS84
    ronds_sf_leg <- st_sf(geometry=st_sfc(st_point(centres_leg),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
    
    #On les convertit en projection locale (pl) pour avoir des metres
    ronds_pl_leg <- st_transform(ronds_sf_leg,paste0("+init=epsg:",code_epsg)) # Lambert 93
    
    #On cree le grand cercle en pl
    ronds_pl_leg_1 <- st_buffer(ronds_pl_leg, taille_rond_m)
    
    #que l'on convertit en WGS84
    ronds_sf_leg_1 <- st_transform(ronds_pl_leg_1,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    #On cree le petit cercle en pl
    ronds_pl_leg_2 <- st_buffer(ronds_pl_leg, taille_rond_m/sqrt(3))
    
    #que l'on decale sur le grand cercle
    ronds_pl_leg_3 <- st_sf(geometry=st_sfc(st_geometry(ronds_pl_leg_2)+c(0,(st_bbox(ronds_pl_leg_1)[2]-st_bbox(ronds_pl_leg_2)[2])),crs=paste0("+init=epsg:",code_epsg)))
    
    #On fusionne les 2 cercles en pl
    ronds_pl_leg <- rbind(ronds_pl_leg_1,ronds_pl_leg_3)
    
    #On convertit le petit cercle en WGS84
    ronds_sf_leg_2 <- st_transform(ronds_pl_leg_2,"+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    #que l'on decale sur le grand cercle
    ronds_sf_leg_3 <- st_sf(geometry=st_sfc(st_geometry(ronds_sf_leg_2)+c(0,(st_bbox(ronds_sf_leg_1)[2]-st_bbox(ronds_sf_leg_2)[2])),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
    
    #On fusionne les 2 cercles en WGS84
    ronds_sf_leg <- rbind(ronds_sf_leg_1,ronds_sf_leg_3)
    
    return(list(ronds_sf_leg,ronds_pl_leg))
  }


construction_lignes_legende <-
  function(ronds_leg,coeff,code_epsg)
  {
    ronds_sf_leg <- ronds_leg[[1]]
    ronds_pl_leg <- ronds_leg[[2]]
    
    # Pour le leaflet en WGS84
    x1_grand <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])]
    y1_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
    pts1_grand <- c(x1_grand,y1_grand)
    x2_grand <- x1_grand+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_grand)+coeff*0.5
    y2_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
    pts2_grand <- c(x2_grand,y2_grand)
    ligne_grand <- rbind(pts1_grand,pts2_grand)
    
    x1_petit <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])]
    y1_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
    pts1_petit <- c(x1_petit,y1_petit)
    x2_petit <- x1_petit+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_petit)+coeff*0.5
    y2_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
    pts2_petit <- c(x2_petit,y2_petit)
    ligne_petit <- rbind(pts1_petit,pts2_petit)
    
    lignes <- st_multilinestring(list(ligne_grand,ligne_petit))
    
    # Pour l'export Qgis en projection locale
    x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
    y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
    pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
    x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+coeff*30000 #on est ici en Lambert93 pour l'export en Qgis
    y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
    pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
    ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl)
    
    x1_petit_pl <- x1_grand_pl
    y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
    pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
    x2_petit_pl <- x2_grand_pl
    y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
    pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
    ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl)
    
    lignes_pl <- st_sf(st_geometry(st_multilinestring(list(ligne_grand_pl,ligne_petit_pl))))
    lignes_pl <- st_set_crs(lignes_pl,paste0("+init=epsg:",code_epsg))
    
    # On ajoute un titre a la legende
    x_titre_1 <- min(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])
    y_titre_1 <- y1_grand+coeff*0.7
    
    return(list(lignes,ligne_grand,ligne_petit,x_titre_1,y_titre_1,ronds_pl_leg,lignes_pl))
  }


rapport_ronds <-
  function(map)
  {
    msg_error1 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1)))
    {
      stop(simpleError(msg_error1))
    }
    
    idx_legende <- NULL
    j <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        j <- c(j,i)
        if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
      }
    }
    
    if(!is.null(j))
    {
      if(!is.null(idx_legende))
      {
        if(length(j)>2) idx <- 2 else idx <- 1
      }else
      {
        if(length(j)>1) idx <- 2 else idx <- 1
      }
      rayonRond <- max(map$x$calls[[j[idx]]]$args[[3]])
      max_var <- map$x$calls[[j[idx]]]$args[[5]]$max_var
      rapport <- (pi*(rayonRond)^2)/max_var
    }else
    {
      stop(simpleError("Il n'y a pas d'analyse en ronds dans la map"))
    }
    
    return(rapport)
  }


rayon_ronds <-
  function(map)
  {
    msg_error1 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1)))
    {
      stop(simpleError(msg_error1))
    }
    
    idx_legende <- NULL
    j <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        j <- c(j,i)
        if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
      }
    }
    
    if(!is.null(j))
    {
      if(!is.null(idx_legende))
      {
        if(length(j)>2) idx <- 2 else idx <- 1
      }else
      {
        if(length(j)>1) idx <- 2 else idx <- 1
      }
      rayonRond <- max(map$x$calls[[j[idx]]]$args[[3]])
    }else
    {
      stop(simpleError("Il n'y a pas d'analyse en ronds dans la map"))
    }
    
    return(rayonRond)
  }


add_legende_ronds <-
  function(map,titre=NULL,lng=NULL,lat=NULL,dom="0",precision=0,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error4 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    idx_carte <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(any(map$x$calls[[i]]$args[5][[1]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method=="addPolylines")
        {
          if(map$x$calls[[i]]$args[3][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_ronds") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[5]]$code_epsg
    
    lng_init <- lng
    lat_init <- lat
    if(!is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }else
    {
      if(!is.null(idx_legende)) map$x$calls <- map$x$calls[-idx_legende] # Si la legende existe, on la supprime pour la recreer
      
      j <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          j <- i
        }
      }
      
      rayonRond <- max(map$x$calls[[j]]$args[[3]])
      
      max_var <- map$x$calls[[j]]$args[[5]]$max_var
      max_var <- as.numeric(str_replace_all(max_var,",","."))
      
      coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
      
      lng_init <- lng
      lat_init <- lat
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map$x$fitBounds[[4]]
        lat <- map$x$fitBounds[[3]]
      }
      
      ronds_leg <- construction_ronds_legende(lng,lat,code_epsg,rayonRond)
      
      ronds_sf_leg <- ronds_leg[[1]]
      
      # Pour le leaflet en WGS84
      x1_grand <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])]
      y1_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
      pts1_grand <- c(x1_grand,y1_grand)
      x2_grand <- x1_grand+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_grand)+coeff*0.5
      y2_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
      pts2_grand <- c(x2_grand,y2_grand)
      ligne_grand <- rbind(pts1_grand,pts2_grand)
      
      x1_petit <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])]
      y1_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
      pts1_petit <- c(x1_petit,y1_petit)
      x2_petit <- x1_petit+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_petit)+coeff*0.5
      y2_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
      pts2_petit <- c(x2_petit,y2_petit)
      ligne_petit <- rbind(pts1_petit,pts2_petit)
      
      lignes <- st_multilinestring(list(ligne_grand,ligne_petit))
      
      suppressWarnings(map <- addCircles(map = map,
                                         lng = st_coordinates(st_centroid(ronds_sf_leg))[,1],
                                         lat = st_coordinates(st_centroid(ronds_sf_leg))[,2],
                                         stroke = TRUE,
                                         opacity = 1,
                                         color = "#2B3E50",
                                         weight = 2,
                                         radius = c(rayonRond,rayonRond/sqrt(3)),
                                         options = pathOptions(clickable = F),
                                         popup = c(max_var,round(max_var/3,0)),
                                         fill = T,
                                         fillColor = "white",
                                         fillOpacity = 1,
                                         group = list(nom_couche="legende_ronds",code_epsg=code_epsg,nom_fond="fond_ronds_leg_carte"))
      )
      
      # leaflet lignes
      map <- addPolylines(map = map, data = lignes,
                          stroke = TRUE,
                          opacity = 1,
                          color = "#2B3E50",
                          weight = 2,
                          options = pathOptions(clickable = F),
                          fill = F,
                          fillOpacity = 1,
                          group = list(nom_couche="legende_ronds",code_epsg=code_epsg,nom_fond="fond_lignes_leg")
      )
      
      # leaflet valeur ronds
      map <- addLabelOnlyMarkers(map = map,
                                 lng = ligne_grand[2,1], lat = ligne_grand[2,2], #ligne_grand
                                 label = as.character(format(round(max_var,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group = list(nom_couche="legende_ronds",code_epsg=code_epsg)
      )
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = ligne_petit[2,1], lat = ligne_petit[2,2], #ligne_petit
                                 label = as.character(format(round(max_var/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group = list(nom_couche="legende_ronds",code_epsg=code_epsg)
      )
      
      if(!is.null(titre))
      {
        # On ajoute un titre a la legende
        x_titre_1 <- min(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])
        y_titre_1 <- y1_grand+coeff*0.8
        
        map <- addLabelOnlyMarkers(map = map,
                                   lng = x_titre_1, lat = y_titre_1,
                                   label = titre,
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "14px"
                                                               )),
                                   group = list(nom_couche="legende_ronds",code_epsg=code_epsg)
        )
      }
    }
    
    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende des ronds sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
    return(map)
  }


######################
### AC/RP_AC/AC_RP ###
######################

calcul_bornes <-
  function(donnees,bornes_analyse,variable_classe,max_classes,methode,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL)
  {
    if(min(bornes_analyse$brks)<0 & max(bornes_analyse$brks)>=0) # Si - et +
    {
      # La gestion de la borne zero est delicate
      # On determine d'abord le nombre de classes requise pour chaque signe
      # On ajoute la borne zero et on relance le calcul des bornes
      
      # Calcul des bornes pour la serie entiere
      if(is.null(variable_classe) | is.null(max_classes) | is.null(methode))
      {
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(donnees[,names(donnees)[sapply(donnees,is.numeric)][2]]),4,style="kmeans",rtimes=10,intervalClosure="left"))
      }else
      {
        if(methode!="manuel")
          suppressWarnings(bornes_analyse <- classIntervals(donnees[,variable_classe],as.numeric(max_classes),style=methode,rtimes=10,intervalClosure="left"))
        else
          suppressWarnings(bornes_analyse <- classIntervals(donnees[,variable_classe],as.numeric(max_classes),style="kmeans",rtimes=10,intervalClosure="left"))
      }
      
      bornes <- bornes_analyse$brks
      
      # On separe les donnees en 2, positives ou nulles d'un cote et negatives de l'autre
      donnees_pos <- donnees[donnees[,variable_classe]>=0,variable_classe]
      donnees_neg <- donnees[donnees[,variable_classe]<0,variable_classe]
      
      # On determine les bornes + et - les plus proches de zero
      borne_pos <- min(bornes[bornes>0])
      borne_neg <- max(bornes[bornes<0])
      
      if(!is.null(stylePalette))
      {
        palette <- recup_palette(stylePalette=stylePalette)
        pal_classes_pos <- palette[[1]]
        pal_classes_neg <- palette[[2]]
      }else
      {
        pal_classes_pos <- palettePos
        pal_classes_neg <- paletteNeg
      }
      
      if(!methode %in% c("quantile","manuel"))
      {
        if(!any(bornes %in% 0)) # Si la methode kmeans genere une borne 0, c'est ideale la distribution est parfaite pour la methode choisie sinon on doit generer une borne zero
        {
          # Pour les methodes (kmeans, jenks, fisher et manuel (kmeans par defaut)), on privilegie la valeur de la borne. On transforme a zero la borne la plus proche de zero.
          
          if(length(bornes[bornes<0])==1)
          {
            nb_classes_pos <- length(bornes[bornes>0])-1
            nb_classes_neg <- 1
          }else if(length(bornes[bornes>0])==1)
          {
            nb_classes_pos <- 1
            nb_classes_neg <- length(bornes[bornes<0])-1
          }else # On a plus d'une borne - et plus d'une borne +
          {
            # On cherche la valeur la plus proche de zero a remplacer par zero
            if(length(min(abs(bornes)))==1) # il n'y a qu'une valeur plus proche de zero
            {
              bornes[abs(bornes)==min(abs(bornes))] <- 0
              
              nb_classes_pos <- length(bornes[bornes>0])
              nb_classes_neg <- length(bornes[bornes<0])
            }else # il y a 2 valeurs plus proches de zero (-2 et 2 par exemple)
            {
              # On garde la borne ayant les plus d'effectifs
              # On compte les effectifs dans chacune des classes autour de zero
              nb_eff_pos <- length(donnees_pos[0 <= donnees_pos & donnees_pos < borne_pos])
              nb_eff_neg <- length(donnees_neg[borne_neg < donnees_neg & donnees_neg < 0])
              
              if(nb_eff_pos > nb_eff_neg)
              {
                # On conserve la borne + et on enleve une classe -
                nb_classes_pos <- length(bornes[bornes>0])
                nb_classes_neg <- length(bornes[bornes<0])-1
              }else if (nb_eff_pos < nb_eff_neg)
              {
                # On conserve la borne - et on enleve une classe +
                nb_classes_pos <- length(bornes[bornes>0])-1
                nb_classes_neg <- length(bornes[bornes<0])
              }else
              {
                # Les valeurs et les effectifs sont identiques, on decide de conserver la borne +.
                bornes[bornes==max(bornes[bornes<0])] <- 0
                
                nb_classes_pos <- length(bornes[bornes>0])
                nb_classes_neg <- length(bornes[bornes<0])-1
              }
            }
          }
          
          if(nb_classes_pos>1)
          {
            suppressWarnings(bornes_analyse_pos <- classIntervals(donnees_pos,nb_classes_pos,style=methode,rtimes=10,intervalClosure="left")$brks)
            bornes_analyse_pos <- bornes_analyse_pos[-1]
          }else # nb_classes_pos == 1
          {
            bornes_analyse_pos <- max(donnees_pos)
          }
          
          if(nb_classes_neg>1)
          {
            suppressWarnings(bornes_analyse_neg <- classIntervals(donnees_neg,nb_classes_neg,style=methode,rtimes=10,intervalClosure="left")$brks)
            bornes_analyse_neg <- bornes_analyse_neg[-length(bornes_analyse_neg)]
          }else # nb_classes_neg == 1
          {
            bornes_analyse_neg <- min(donnees_neg)
          }
          
          bornes <- unique(c(bornes_analyse_neg,0,bornes_analyse_pos))
          bornes <- sort(bornes, decreasing = TRUE)
          
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
          pal_classes <- c(pal_classes_pos,pal_classes_neg)
        }
      }else if(methode!="manuel")# Pour la methode des quantiles, on ne gere pas la borne zero pour ne pas fausser l'equi-distribution des effectifs dans les classes.
      {
        print("La methode des quantiles ne permet pas de gerer la borne a 0. Vous pouvez passer en mode manuel pour modifier les bornes des classes.")
        
        # On doit tout de meme determiner la couleur de la classe autour de zero
        
        if(length(bornes[bornes<0])==1) # il n'y a qu'une seule valeur - dc on conserve la classe -
        {
          col_classe <- "NEG"
        }else if(length(bornes[bornes>0])==1) # il n'y a qu'une seule valeur + dc on conserve la classe +
        {
          col_classe <- "POS"
        }else # On a plus d'une borne - et plus d'une borne +
        {
          # On compte les effectifs dans chacune des classes autour de zero
          nb_eff_pos <- length(donnees_pos[0 <= donnees_pos & donnees_pos < borne_pos])
          nb_eff_neg <- length(donnees_neg[borne_neg < donnees_neg & donnees_neg < 0])
          
          if(nb_eff_pos > nb_eff_neg)
          {
            col_classe <- "POS"
          }else if(nb_eff_pos < nb_eff_neg)
          {
            col_classe <- "NEG"
          }else # les effectifs sont identiques, on compare les valeurs
          {
            if(borne_pos-abs(borne_neg)>0)
            {
              col_classe <- "POS"
            }else if(borne_pos-abs(borne_neg)<0)
            {
              col_classe <- "NEG"
            }else # il y a tjs egalite, on conserve par defaut la classe +
            {
              col_classe <- "POS"
            }
          }
        }
        
        if(col_classe=="POS")
        {
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])-1]
        }else #col_classe=="NEG"
        {
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+2):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
        }
        
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }else # methode "manuel"
      {
        if(any(bornes %in% 0)) # Si il y a le 0 de present
        {
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
          pal_classes <- c(pal_classes_pos,pal_classes_neg)
        }else # Si il n'y a pas de 0, on determine la couleur de la classe autour de 0
        {
          if(length(bornes[bornes<0])==1) # il n'y a qu'une seule valeur - dc on conserve la classe -
          {
            col_classe <- "NEG"
          }else # Sinon on garde la classe + par defaut
          {
            col_classe <- "POS"
          }
          
          if(col_classe=="POS")
          {
            pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
            pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])-1]
          }else #col_classe=="NEG"
          {
            pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+2):length(pal_classes_pos)]
            pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
          }
          
          pal_classes <- c(pal_classes_pos,pal_classes_neg)
        }
      }
    }
    
    if(min(bornes_analyse$brks)>=0) # Si +
    {
      bornes <- c(bornes_analyse$brks[bornes_analyse$brks>=0])
      bornes <- sort(bornes, decreasing = TRUE)
      
      if(!is.null(stylePalette))
      {
        palette <- recup_palette(stylePalette=stylePalette)
        pal_classes_pos <- palette[[1]]
      }else
      {
        pal_classes_pos <- palettePos
      }
      
      if(length(bornes)>=(length(pal_classes_pos)+1))
      {
        pal_classes <- pal_classes_pos
      }else
      {
        pal_classes <- pal_classes_pos[-c(1:(length(pal_classes_pos)-length(bornes)+1))] # On enleve les couleurs fonces inutiles
      }
    }
    
    if(max(bornes_analyse$brks)<0) # Si -
    {
      bornes <- c(bornes_analyse$brks[bornes<0])
      bornes <- sort(bornes, decreasing = TRUE)
      
      if(!is.null(stylePalette))
      {
        palette <- recup_palette(stylePalette=stylePalette)
        pal_classes_neg <- palette[[2]]
      }else
      {
        pal_classes_neg <- paletteNeg
      }
      
      if(length(bornes)>=(length(pal_classes_neg)+1))
      {
        pal_classes <- pal_classes_neg
      }else
      {
        pal_classes <- pal_classes_neg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
      }
    }
    return(list(bornes,pal_classes))
  }


distrib_variable <-
  function(data,varRatio,methode="kmeans",nbClasses=3,bornes=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(class(varRatio)!="character")) msg_error2 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(methode)!="character")) msg_error3 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error4 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error5 <- "La variable doit etre un vecteur numerique / "
    
    if(!any(names(data) %in% varRatio))  msg_error6 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error7 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7)))
    }
    
    donnees <- data.frame(VAR=as.numeric(data[,varRatio]))
    
    if(is.null(bornes))
    {
      suppressWarnings(bornes <- classIntervals(data[,varRatio],as.numeric(nbClasses),style=methode,rtimes=10,intervalClosure="left")$brks)
    }
    
    ggplot(donnees, aes(x=donnees$VAR)) +
      stat_bin(breaks=unique(sort(c(min(donnees$VAR),bornes,max(donnees$VAR)))), closed = "left", fill="#5182B6", col="white") +
      scale_x_continuous(breaks=unique(sort(c(min(donnees$VAR),bornes,max(donnees$VAR)))), labels = round(unique(sort(c(min(donnees$VAR),bornes,max(donnees$VAR)))),2)) +
      ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
      xlab(label = varRatio)
  }


recup_palette <-
  function(stylePalette)
  {
    # Palettes
    inseeDefautPos <- c("#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93") # Rouge du +fonce au + clair
    inseeDefautNeg <- c("#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050") # Bleu du + clair au + fonce
    inseeFlashPos <- c("#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93")
    inseeFlashNeg <- c("#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289")
    inseeAnalysePos <- c("#5E2057","#853567","#8E5981","#BA97B2","#D7C0CC")
    inseeAnalyseNeg <- c("#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289")
    inseeDossierPos <- c("#4F185E","#65317B","#9475A5","#BFA5C6","#E7D1E5")
    inseeDossierNeg <- c("#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289")
    inseePremierePos <- c("#7F0029","#CC1543","#DE635B","#F79C85","#FDE3DE")
    inseePremiereNeg <- c("#ECF4D8","#CDD78C","#91B778","#549534","#005941")
    
    if(stylePalette=="InseeFlash")
    {
      inseePos <- inseeFlashPos
      inseeNeg <- inseeFlashNeg
    }
    if(stylePalette=="InseeAnalyse")
    {
      inseePos <- inseeAnalysePos
      inseeNeg <- inseeAnalyseNeg
    }
    if(stylePalette=="InseeDossier")
    {
      inseePos <- inseeDossierPos
      inseeNeg <- inseeDossierNeg
    }
    if(stylePalette=="InseePremiere")
    {
      inseePos <- inseePremierePos
      inseeNeg <- inseePremiereNeg
    }
    if(stylePalette=="defaut")
    {
      inseePos <- inseeDefautPos
      inseeNeg <- inseeDefautNeg
    }
    
    return(list(inseePos,inseeNeg))
  }


add_legende_classes <-
  function(map,titre=NULL,lng=NULL,lat=NULL,typeLegende=1,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(any(class(typeLegende)!="numeric")) msg_error4 <- "Le type de legende doit etre de type numerique (1:litterale ou 2:en echelle) / "
    if(!typeLegende %in% c(1,2)) msg_error5 <- "La variable typeLegende doit etre 1 ou 2 / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    idx_carte <- NULL
    idx_legende <- NULL
    ronds <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[[3]]$nom_couche %in% c("carte_classes","carte_ronds_classes","carte_classes_ronds"))) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% "carte_ronds_classes")
        {
          idx_carte <- c(idx_carte,i)
          ronds <- T
        }
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(ronds) arg <- 5 else arg <- 3
    code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$code_epsg
    
    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
    
    lng_init <- lng
    lat_init <- lat
    if(is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init))) # La legende n'a pas encore ete creee, on la cree avec une position par defaut
    {
      lng <- map$x$fitBounds[[4]]
      lat <- map$x$fitBounds[[1]]
    }else if(is.null(idx_legende)) # La legende n'a pas encore ete creee, on la cree avec la position definie par l'utilisateur
    {
      # voir plus loin
    }else # l'utilisateur veut modifier la legende existante, on la supprime pour la recreer
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }
    
    if(is.null(idx_legende) | !is.null(idx_legende) & !(is.null(lng_init) | is.null(lat_init))) # Si la legende doit etre creee ou recreee
    {
      # on calcule idx_carte au cas oC9 la legende ait ete supprimee, c'est le nombre de polygons dans le leaflet
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche %in% "carte_ronds_classes")
          {
            idx_carte <- c(idx_carte,i)
          }
        }
      }
      
      if(!ronds)
      {
        nb_classes <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
        pal_classes <- unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor)
      }else
      {
        nb_classes <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[6]]$fillColor))
        pal_classes <- unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[6]]$fillColor)
      }
      
      palette <- recup_palette(stylePalette=map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$style)
      pal_classes_pos <- palette[[1]]
      pal_classes_neg <- palette[[2]]
      pal_classes_pos <- pal_classes_pos[pal_classes_pos %in% pal_classes]
      pal_classes_neg <- pal_classes_neg[pal_classes_neg %in% pal_classes]
      pal_classes <- c(pal_classes_pos,pal_classes_neg)
      
      # Coordonnees du point haut/gauche des rectangles de la legende
      if(typeLegende==1) decalage <- 0.7 else decalage <- 0.5
      x_coord_rectangle <- lng
      for(i in 1:nb_classes)
      {
        if(i==1) #1er rectangle
        {
          y_coord_rectangle <- lat-coeff
        }else
        {
          y_coord_rectangle <- y_coord_rectangle-coeff*decalage
        }
        assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
      }
      
      if(ronds) arg <- 5 else arg <- 3
      precision <- as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$precision)
      
      if(typeLegende==1) # Litterale
      {
        # On ajoute un cadre blanc autour de la legende
        y_coord_rectangle <- min(get(paste0("rectangle_",nb_classes))[[1]][,2])
        
        # leaflet du cadre blanc en 1er
        map <- addRectangles(map = map,
                             lng1 = lng-coeff*0.5, lat1 = lat+coeff*0.5,
                             lng2 = x_coord_rectangle+coeff*10, lat2 = y_coord_rectangle-coeff*0.8,
                             stroke = TRUE,
                             color = paste0("#2B3E50", ";background: #ffffff;
                                            border-left:2px solid #2B3E50;
                                            border-right:2px solid #2B3E50;
                                            border-top:2px solid #2B3E50;
                                            border-bottom:2px solid #2B3E50;
                                            border-radius: 5%"),
                             weight = 1,
                             options = pathOptions(clickable = F),
                             fill = T,
                             fillColor = "white",
                             fillOpacity = 0.5,
                             group=list(nom_couche="legende_classes")
                             )
        
        # leaflet rectangles et valeurs classes
        label_rectangle <- NULL
        
        for(i in 1:nb_classes)
        {
          if(i==1)
          {
            label_rectangle <- c(label_rectangle,paste0(format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i+1]),precision), big.mark=" ",decimal.mark=",",nsmall=0)," et plus"))
          }else if(i==nb_classes)
          {
            label_rectangle <- c(label_rectangle,paste0("Moins de ", format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i]),precision), big.mark=" ",decimal.mark=",",nsmall=0)))
          }else
          {
            label_rectangle <- c(label_rectangle,paste0("De ", format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i+1]),precision), big.mark=" ",decimal.mark=",",nsmall=0)," \u00E0 moins de ", format(round(as.numeric(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes[i]),precision), big.mark=" ",decimal.mark=",",nsmall=0)))
          }
        }
        
        for(i in 1:nb_classes)
        {
          map <- addLabelOnlyMarkers(map = map,
                                     lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                     label = label_rectangle[i],
                                     labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                 style = list(
                                                                   "color" = "black",
                                                                   "font-size" = "12px"
                                                                 )),
                                     group=list(nom_couche="legende_classes")
          )
        }
        
        # On cree les polygons ensemble a la fin de l'objet leaflet juste avant le titre
        for(i in 1:nb_classes)
        {
          map <- addPolygons(map = map, data = st_polygon(get(paste0("rectangle_",i))),
                             stroke = FALSE,
                             options = pathOptions(clickable = F),
                             fill = T,
                             fillColor = pal_classes[i],
                             fillOpacity = 1,
                             group=list(nom_couche="legende_classes")
          )
        }
        
        # leaflet titre
        x_titre <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
        y_titre <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.4
        
        map <- addLabelOnlyMarkers(map = map,
                                   lng = x_titre, lat = y_titre,
                                   label = titre,
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "14px"
                                                               )),
                                   group=list(nom_couche="legende_classes")
        )
      }
      
      if(typeLegende==2) # En echelle
      {
        # On ajoute un cadre blanc autour de la legende
        y_coord_rectangle <- min(get(paste0("rectangle_",nb_classes))[[1]][,2])
        
        # leaflet du cadre blanc en 1er
        map <- addRectangles(map = map,
                             lng1 = lng-coeff*0.5, lat1 = lat+coeff*0.5,
                             lng2 = x_coord_rectangle+coeff*5, lat2 = y_coord_rectangle-coeff*0.8,
                             stroke = TRUE,
                             color = paste0("#2B3E50", ";background: #ffffff;
                                            border-left:2px solid #2B3E50;
                                            border-right:2px solid #2B3E50;
                                            border-top:2px solid #2B3E50;
                                            border-bottom:2px solid #2B3E50;
                                            border-radius: 5%"),
                             weight = 1,
                             options = pathOptions(clickable = F),
                             fill = T,
                             fillColor = "white",
                             fillOpacity = 0.5,
                             group=list(nom_couche="legende_classes")
                             )
        
        if(ronds) arg <- 5 else arg <- 3
        bornes <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[arg]]$bornes
        
        for(i in 1:nb_classes)
        {
          if(i<nb_classes)
          {
            x1 <- max(get(paste0("rectangle_",i))[[1]][,1])
            y1 <- min(get(paste0("rectangle_",i))[[1]][,2])
            x2 <- max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.2
            y2 <- min(get(paste0("rectangle_",i))[[1]][,2])
            ligne <- st_linestring(rbind(c(x1,y1),c(x2,y2)))
            
            map <- addPolylines(map = map, data = ligne,
                                color = "black",
                                weight = 1,
                                options = pathOptions(clickable = F),
                                fill = F,
                                fillOpacity = 1,
                                group=list(nom_couche="legende_classes")
            )
            
            map <- addLabelOnlyMarkers(map = map,
                                       lng = x2, lat = y2,
                                       label = as.character(format(round(as.numeric(bornes[i+1]),precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                       labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                                   style = list(
                                                                     "color" = "black",
                                                                     "font-size" = "12px"
                                                                   )),
                                       group=list(nom_couche="legende_classes")
            )
          }
        }
        
        # On cree les polygons ensemble a la fin de l'objet leaflet juste avant le titre
        for(i in 1:nb_classes)
        {
          map <- addPolygons(map = map, data = st_polygon(get(paste0("rectangle_",i))),
                             stroke = FALSE,
                             options = pathOptions(clickable = F),
                             fill = T,
                             fillColor = pal_classes[i],
                             fillOpacity = 1,
                             group=list(nom_couche="legende_classes")
          )
        }
        
        # leaflet titre
        x_titre <- min(get("rectangle_1")[[1]][,1])
        y_titre <- max(get("rectangle_1")[[1]][,2])+coeff*0.6
        
        map <- addLabelOnlyMarkers(map = map,
                                   lng = x_titre, lat = y_titre,
                                   label = titre,
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "14px"
                                                               )),
                                   group=list(nom_couche="legende_classes")
        )
      }
    }
    
    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende des classes sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
    return(map)
  }


calcul_ratio <-
  function(data,var1,var2)
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(!any(names(data) %in% var1))  msg_error2 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% var2))  msg_error3 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    if(any(data[,var2] %in% 0))
    {
      data[data[,var2] %in% 0,var2] <- 0.0001
    }
    
    data$RATIO <- (data[,var1]/data[,var2])*100
    
    return(data)
  }


calcul_tx_evol_global <-
  function(data,var1,var2)
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(!any(names(data) %in% var1))  msg_error2 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% var2))  msg_error3 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    if(any(data[,var1] %in% 0))
    {
      data[data[,var1] %in% 0,var1] <- 0.0001
    }
    
    data$TEG <- ((data[,var2]-data[,var1])/data[,var1])*100
    
    return(data)
  }


calcul_tx_evol_ann_moy <-
  function(data,var1,var2,nbAnnees)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(!any(names(data) %in% var1))  msg_error2 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% var2))  msg_error3 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(any(class(nbAnnees)!="numeric")) msg_error4 <- "La variable doit etre de type numerique / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(any(data[,var1] %in% 0))
    {
      data[data[,var1] %in% 0,var1] <- 0.0001
    }
    
    data$TEAM <- ((data[,var2]/data[,var1])^(1/nbAnnees)-1)*100
    
    return(data)
  }


calcul_part_ens <-
  function(data,var)
  {
    msg_error1<-msg_error2 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(!any(names(data) %in% var))  msg_error2 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }
    
    if(sum(data[,var],na.rm = TRUE)==0)
    {
      data[data[,var] %in% 0,var] <- 0.0001
    }
    
    data$PART <- (data[,var]/sum(data[,var],na.rm = TRUE))*100
    
    return(data)
  }

calculette <-
  function(data,formule=NULL)
  {
    msg_error1<-msg_error2 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(class(formule)!="character")) msg_error2 <- "La formule doit etre une chaC.ne de caracteres / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }
    
    if(is.null(formule)) return(data)
    suppressWarnings(test_formule <- try(with(data,eval(parse(text=formule))),silent=TRUE))
    
    if(class(test_formule) %in% "try-error")
    {
      stop(simpleError("La formule n'est pas correcte."))
    }else
    {
      data$CALCUL <- with(data,eval(parse(text=formule)))
    }
    
    return(data)
  }


set_couleur_classes <-
  function(map,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL,colBorder="white")
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(any(class(stylePalette)!="character")) msg_error2 <- "Le style de la palette doit etre de type caractere ('InseeFlash', 'InseeAnalyse', 'InseeDossier', 'InseePremiere' ou 'defaut') / "
    if(!is.null(palettePos)) if(any(class(palettePos)!="character")) msg_error3 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(!is.null(paletteNeg)) if(any(class(paletteNeg)!="character")) msg_error4 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error5 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5)))
    }
    
    if(is.null(palettePos) & is.null(paletteNeg))
    {
      palette <- recup_palette(stylePalette)
      inseePos <- palette[[1]]
      inseeNeg <- palette[[2]]
    }else
    {
      inseePos <- NULL
      inseeNeg <- NULL
      if(!is.null(palettePos)) inseePos <- palettePos
      if(!is.null(paletteNeg)) inseeNeg <- paletteNeg
    }
    
    idx_carte <- NULL
    idx_legende <- NULL
    ronds <- F
    legende <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[[3]]$nom_couche %in% c("carte_classes","carte_ronds_classes","carte_classes_ronds")))
        {
          if(any(map$x$calls[[i]]$args[[3]]$nom_fond %in% c("fond_maille_carte","fond_maille_elargi_carte")))
            idx_carte <- c(idx_carte,i)
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% "carte_ronds_classes")
        {
          idx_carte <- c(idx_carte,i)
          ronds <- T
        }
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") legende <- T
      }
      if(legende) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    for(i in 1:length(idx_carte))
    {
      if(!ronds)
      {
        bornes <- map$x$calls[[idx_carte[i]]]$args[[3]]$bornes
        arg <- 4
        style_anc <- map$x$calls[[idx_carte[i]]]$args[[3]]$style
        map$x$calls[[idx_carte[i]]]$args[[3]]$style <- stylePalette
      }else
      {
        bornes <- map$x$calls[[idx_carte[i]]]$args[[5]]$bornes
        arg <- 6
        style_anc <- map$x$calls[[idx_carte[i]]]$args[[5]]$style
        map$x$calls[[idx_carte[i]]]$args[[5]]$style <- stylePalette
      }
      
      palette <- recup_palette(stylePalette=style_anc)
      inseePos_anc <- palette[[1]]
      inseeNeg_anc <- palette[[2]]
      
      nb_col_pos <- length(bornes[bornes>0])
      nb_col_neg <- length(bornes[bornes<0])
      
      couleur_pos <- NULL
      couleur_neg <- NULL
      if(!is.null(inseePos) & nb_col_pos>0) couleur_pos <- inseePos[c((length(inseePos)-nb_col_pos+1):length(inseePos))]
      if(!is.null(inseeNeg) & nb_col_neg>0) couleur_neg <- inseeNeg[c(1:nb_col_neg)]
      pal_new <- c(couleur_pos,couleur_neg)
      
      couleur_pos_anc <- NULL
      couleur_neg_anc <- NULL
      if(!is.null(inseePos_anc) & nb_col_pos>0) couleur_pos_anc <- inseePos_anc[c((length(inseePos_anc)-nb_col_pos+1):length(inseePos_anc))]
      if(!is.null(inseeNeg_anc) & nb_col_neg>0) couleur_neg_anc <- inseeNeg_anc[c(1:nb_col_neg)]
      pal_anc <- data.frame(col=c(couleur_pos_anc,couleur_neg_anc))
      
      couleur_analyse <- data.frame(col=map$x$calls[[idx_carte[i]]]$args[[arg]]$fillColor)
      couleur_analyse$id1 <- c(1:nrow(couleur_analyse))
      
      pal_anc$id2 <- c(1:nrow(pal_anc))
      couleur_analyse <- merge(couleur_analyse,pal_anc,by="col")
      
      aa <- sapply(1:(length(pal_new)), function(x) couleur_analyse[couleur_analyse$id2==x,"col"] <<- pal_new[x])
      rm(aa)
      couleur_analyse <- couleur_analyse[order(couleur_analyse$id1),]
      couleur_analyse <- couleur_analyse$col
      
      map$x$calls[[idx_carte[i]]]$args[[arg]]$fillColor <- couleur_analyse
      
      map$x$calls[[idx_carte[i]]]$args[[arg]]$color <- colBorder
    }
    
    if(!is.null(idx_legende))
    {
      for(i in 1:length(idx_legende))
      {
        map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor <- pal_new[i]
      }
    }
    return(map)
  }


set_opacite_elargi <-
  function(map,opacite=0.6)
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(any(class(opacite)!="numeric")) msg_error2 <- "L'opacite doit etre de type numerique (entre 0 et 1) / "
    if(opacite<0 | opacite>1) msg_error3 <- "L'opacite doit etre compris entre 0 (transparent) et 1 (opaque) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    idx_carte_classes_elargi <- NULL
    idx_carte_ronds_elargi <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons") # maille et/ou analyse en classes
      {
        if(any(map$x$calls[[i]]$args[[3]]$nom_couche %in% c("carte_ronds","carte_classes","carte_ronds_classes","carte_classes_ronds")))
        {
          if(map$x$calls[[i]]$args[[3]]$nom_fond %in% c("fond_maille_elargi","fond_maille_elargi_carte")) idx_carte_classes_elargi <- c(idx_carte_classes_elargi,i)
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles") # ronds
      {
        if(any(map$x$calls[[i]]$args[[5]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds")))
        {
          if(any(map$x$calls[[i]]$args[[5]]$nom_fond %in% c("fond_ronds_pos_elargi_carte","fond_ronds_neg_elargi_carte","fond_ronds_elargi_carte"))) idx_carte_ronds_elargi <- c(idx_carte_ronds_elargi,i)
        }
      }
    }
    
    if(!is.null(idx_carte_classes_elargi))
    {
      for(i in 1:length(idx_carte_classes_elargi))
      {
        map$x$calls[[idx_carte_classes_elargi]]$args[[4]]$opacity <- opacite
        map$x$calls[[idx_carte_classes_elargi]]$args[[4]]$fillOpacity <- opacite
      }
    }
    if(!is.null(idx_carte_ronds_elargi))
    {
      for(i in 1:length(idx_carte_ronds_elargi))
      {
        map$x$calls[[idx_carte_ronds_elargi]]$args[[6]]$opacity <- opacite
        map$x$calls[[idx_carte_ronds_elargi]]$args[[6]]$fillOpacity <- opacite
      }
    }
    
    return(map)
  }


##########
### RP ###
##########

set_couleur_ronds <-
  function(map,colorPos="#CD853F",colorNeg="#6495ED",colBorder="white")
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(colorPos)) if(any(class(colorPos)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(!is.null(colorNeg)) if(any(class(colorNeg)!="character")) msg_error3 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error4 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    idx_carte <- NULL
    classes <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% c("carte_ronds","carte_ronds_classes","carte_classes_ronds"))
        {
          idx_carte <- c(idx_carte,i)
        }
        if(map$x$calls[[i]]$args[[5]]$nom_couche %in% c("carte_ronds_classes","carte_classes_ronds"))
        {
          classes <- T
        }
      }
    }
    
    for(i in 1:length(idx_carte))
    {
      if(!classes)
      {
        val_pos <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],max(str_locate_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11),">")[[1]])+1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11)," ",""))>=0)
        
        if(length(val_pos)>0)
        {
          map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor[1:length(map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor)] <- colorPos
        }
        
        val_neg <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],max(str_locate_all(substring(map$x$calls[[idx_carte[i]]]$args[[7]],1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11),">")[[1]])+1,nchar(map$x$calls[[idx_carte[i]]]$args[[7]])-11)," ",""))<0)
        
        if(length(val_neg)>0)
        {
          map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor[1:length(map$x$calls[[idx_carte[i]]]$args[[6]]$fillColor)] <- colorNeg
        }
      }
      
      map$x$calls[[idx_carte[i]]]$args[[6]]$color <- colBorder
    }
    return(map)
  }


extract_fond_leaflet_ronds <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_carte_ronds <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende_ronds <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_ronds") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="carte_ronds") idx_carte_ronds <- c(idx_carte_ronds,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
      }
      
      if(!is.null(idx_legende_ronds)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
        
        if(map$x$calls[[i]]$method %in% "addPolylines")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
      }
    }
    
    if(is.null(idx_legende_ronds))
    {
      return(NULL)
    }else
    {
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- substring(map$x$calls[[idx_carte[i]]]$args[[5]],25,nchar(map$x$calls[[idx_carte[i]]]$args[[5]])-12)
          fond <- cbind(LIBELLE=cc,bb)
        }else
        {
          fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        }
        
        fond <- st_transform(fond,paste0("+init=epsg:",map$x$calls[[idx_carte[i]]]$args[[3]]$code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      for(i in 1:length(idx_carte_ronds))
      {
        dom <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$dom
        
        centres_ronds <- data_frame(lng=map$x$calls[[idx_carte_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_carte_ronds[i]]]$args[[1]])
        aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
        bb <- do.call("rbind",aa)
        cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$code_epsg))
        ronds_pl <- st_buffer(cc, map$x$calls[[idx_carte_ronds[i]]]$args[[3]])
        
        col_bor <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$color
        ronds_pl <- cbind(COL_BOR=col_bor,ronds_pl)
        col <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$fillColor
        ronds_pl <- cbind(COL=col,ronds_pl)
        val <- as.numeric(str_replace_all(str_replace_all(substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]],str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]],">")[[1]][6]+1,nchar(map$x$calls[[idx_carte_ronds[i]]]$args[[7]])-11)," ",""),",","."))
        ronds_pl <- cbind(VAL=val,ronds_pl)
        
        fond_pos <- NULL
        fond_neg <- NULL
        
        if(nrow(ronds_pl[ronds_pl$VAL>0,])>0)
        {
          fond_pos <- ronds_pl[ronds_pl$VAL>0,]
        }
        if(nrow(ronds_pl[ronds_pl$VAL<0,])>0)
        {
          fond_neg <- ronds_pl[ronds_pl$VAL<0,]
        }
        
        if(!is.null(fond_pos))
        {
          list_fonds[[l]] <- fond_pos
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$nom_fond[1])
          l <- l+1
        }
        if(!is.null(fond_neg))
        {
          list_fonds[[l]] <- fond_neg
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$nom_fond[2])
          l <- l+1
        }
      }
      
      for(i in 1:length(idx_legende_ronds))
      {
        if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addCircles")
        {
          centres_ronds <- data_frame(lng=map$x$calls[[idx_legende_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_legende_ronds[i]]]$args[[1]])
          aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
          bb <- do.call("rbind",aa)
          cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$code_epsg))
          ronds_pl <- st_buffer(cc, map$x$calls[[idx_legende_ronds[i]]]$args[[3]])
          
          ronds_pl_leg <- ronds_pl
          val <- c(map$x$calls[[idx_legende_ronds[i]]]$args[[7]][1],map$x$calls[[idx_legende_ronds[i]]]$args[[7]][2])
          ronds_pl_leg <- cbind(VAL=val,ronds_pl_leg)
          
          list_fonds[[l]] <- ronds_pl_leg
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$nom_fond)
          l <- l+1
        }
        
        if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addPolylines")
        {
          # Pour l'export Qgis en projection locale
          x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
          y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
          pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
          x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-min(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"]))/3
          y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
          pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
          ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl)
          
          x1_petit_pl <- x1_grand_pl
          y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
          pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
          x2_petit_pl <- x2_grand_pl
          y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
          pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
          ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl)
          
          lignes_pl <- st_sf(st_geometry(st_multilinestring(list(ligne_grand_pl,ligne_petit_pl))))
          lignes_pl <- st_set_crs(lignes_pl,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$code_epsg))
          
          list_fonds[[l]] <- lignes_pl
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$nom_fond)
          l <- l+1
        }
      }
    }
    
    return(list(list_fonds,nom_fonds,dom))
  }


export_qgis_ronds <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_ronds(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des ronds n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_ronds(map) pour ajouter une legende de ronds a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_ronds_pos_carte")) l <- c(l,"fond_ronds_pos_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_neg_carte")) l <- c(l,"fond_ronds_neg_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_pos_elargi_carte")) l <- c(l,"fond_ronds_pos_elargi_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_neg_elargi_carte")) l <- c(l,"fond_ronds_neg_elargi_carte")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")
    if(any(list_fonds[[2]] %in% "fond_maille_elargi")) l <- c(l,"fond_maille_elargi")
    if(any(list_fonds[[2]] %in% "fond_lignes_leg")) l <- c(l,"fond_lignes_leg")
    if(any(list_fonds[[2]] %in% "fond_ronds_leg_carte")) l <- c(l,"fond_ronds_leg_carte")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    export_projet_qgis_ronds(l,rep_sortie,sortie,titre1,titre2,source,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_ronds <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax) #str_sub(unlist(str_split(sortie,"/"))[length(unlist(str_split(sortie,"/")))],end=-5)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if(nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))
      }else
      {
        idcouche=l[i]
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if(str_sub(l[i][length(l[i])],start=-5)!="carte")
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()      
      #cas ou le fond selectionne est l'analyse en ronds ou les ronds de la legende
      if(str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        analyse_ronds <- read_sf(chemincouche)
        geometrie=attr(analyse_ronds$geometry[[1]],"class")[2]
        projcouche=st_crs(analyse_ronds$geometry)$proj4string
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        #preparation des param du BLOCSYMBOL
        if(str_sub(l[i][length(l[i])],start=-9) %in% c("pos_carte","neg_carte","leg_carte")) #par les fonctions autres que shy
        {
          if(str_sub(l[i][length(l[i])],start=-9)!="leg_carte") couleurbordure=unique(analyse_ronds$COL_BOR)# par defaut : "255,255,255" white
          stylebordure="solid"
          epaisseurbordure=0.26
          if(str_sub(l[i][length(l[i])],start=-9)=="pos_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "205,133,63" orange peru
          if(str_sub(l[i][length(l[i])],start=-9)=="neg_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "100,149,237" cornflowerblue
          if(str_sub(l[i][length(l[i])],start=-9)=="leg_carte")
          {
            couleurbordure="0,0,0"
            couleurfond="transparent"
          }
        }else if(str_sub(l[i][length(l[i])],start=-16) %in% c("pos_elargi_carte","neg_elargi_carte"))
        {
          couleurbordure=unique(analyse_ronds$COL_BOR)# par defaut : "255,255,255" white
          stylebordure="solid"
          epaisseurbordure=0.26
          if(str_sub(l[i][length(l[i])],start=-16)=="pos_elargi_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "205,133,63" orange peru
          if(str_sub(l[i][length(l[i])],start=-16)=="neg_elargi_carte") couleurfond=unique(analyse_ronds$COL)# par defaut : "100,149,237" cornflowerblue
        }else #par les fonctions shy_
        {
          couleurbordure="255,255,255"
          stylebordure="solid"
          epaisseurbordure=0.26
          couleurfond="205,133,63" #orange peru
          if(str_sub(l[i][length(l[i])],start=-10)=="rupt_carte") couleurfond="100,149,237" #cornflowerblue
        }
        remplissagefond="solid"
        BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }else
      {
        #cas ou le fond selectionne n'est pas l'analyse en ronds ni les ronds de la legende
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        couleurfond="255,255,255"
        
        if(l[i]=="fond_ronds")
        {
          couleurfond="255,255,255"
          remplissagefond="yes"
        }else
        {
          remplissagefond="no"
        }
        
        if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
        {
          couleurbordure="128,128,128"
        }else if (l[i]=="fond_maille_elargi")
        {
          couleurbordure="200,200,200"
        }else if (l[i]=="fond_territoire")
        {
          couleurbordure="191,191,191"
        }else
        {
          couleurbordure="0,0,0"
        }
        
        stylebordure="solid"
        if (l[i] %in% c("fond_maille","fond_maille_elargi","fond_ronds","fond_lignes","fond_departement","fond_pays","fond_territoire","fond_lignes_leg"))
        {
          epaisseurbordure=0.26
        }else
        {
          epaisseurbordure=0.5
        }
        
        assign(paste0(l[i]),read_sf(chemincouche))
        projcouche=st_crs(get(l[i]))$proj4string
        geometrie=attr(get(l[i])$geometry[[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(get(l[i])[[1]][[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(get(l[i])[[1]],"class")[2]
        
        if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
        {
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }else
        {
          if (geometrie %in% c("MULTILINESTRING"))
          {
            BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
          }
        }
        
        blocvector=modif_blocvectorClassification(nomcouche)
        blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8") #paste0(str_sub(sortie,end=-5),".qgs")
  }


##########
### AC ###
##########

k_classes <-
  function(fond_carto_k,fond_carto_elargi_k,variable_jointure_fond_carto_k,donnees_k,variable_jointure_donnees_k,variable_a_representer_k,elargi)
  {
    donnees_k[,variable_a_representer_k] <- as.numeric(donnees_k[,variable_a_representer_k])
    suppressWarnings(donnees_k<-donnees_k[!is.na(donnees_k[,variable_a_representer_k]),])
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
    
    base <- merge(as.data.frame(fond_carto_k)[,-length(names(fond_carto_k))],donnees_k,by.x=variable_jointure_fond_carto_k,by.y=variable_jointure_donnees_k)
    if (length(names(base)[grep("..y",names(base))])>0)
    {
      base <- base[,-grep("..y",names(base))]
      names(base)[grep("..x",names(base))]<-sub(".x","",names(base)[grep("..x",names(base))])
    }
    
    base <- base[abs(base[,variable_a_representer_k])>=0,]
    base <- base[order(base[,variable_a_representer_k],decreasing = T),]
    if(nrow(base)>0)
    {
      rownames(base) <- as.character(1:dim(base)[1])
    }else
    {
      return(NULL)
    }
    base$id <- c(1:nrow(base))
    base <- base[order(base[,variable_jointure_fond_carto_k]),]
    fond_carto_k <- merge(base[,c(variable_jointure_fond_carto_k,"id")],fond_carto_k,by=variable_jointure_fond_carto_k)
    fond_carto_k <- fond_carto_k[order(fond_carto_k$id),]
    fond_carto_k <- fond_carto_k[,-2]
    base <- base[order(base$id),]
    base <- base[,-(ncol(base))]
    
    if(elargi)
    {
      fond_carto_elargi_k <- fond_carto_elargi_k[as.data.frame(fond_carto_elargi_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
      
      base_elargi <- merge(as.data.frame(fond_carto_elargi_k)[,-length(names(fond_carto_elargi_k))],donnees_k,by.x=variable_jointure_fond_carto_k,by.y=variable_jointure_donnees_k)
      if (length(names(base_elargi)[grep("..y",names(base_elargi))])>0)
      {
        base_elargi <- base_elargi[,-grep("..y",names(base_elargi))]
        names(base_elargi)[grep("..x",names(base_elargi))]<-sub(".x","",names(base_elargi)[grep("..x",names(base_elargi))])
      }
      
      base_elargi <- base_elargi[abs(base_elargi[,variable_a_representer_k])>=0,]
      base_elargi <- base_elargi[order(base_elargi[,variable_a_representer_k],decreasing = T),]
      rownames(base_elargi) <- as.character(1:dim(base_elargi)[1])
      
      base_elargi$id <- c(1:nrow(base_elargi))
      base_elargi <- base_elargi[order(base_elargi[,variable_jointure_fond_carto_k]),]
      fond_carto_elargi_k <- merge(base_elargi[,c(variable_jointure_fond_carto_k,"id")],fond_carto_elargi_k,by=variable_jointure_fond_carto_k)
      fond_carto_elargi_k <- fond_carto_elargi_k[order(fond_carto_elargi_k$id),]
      fond_carto_elargi_k <- fond_carto_elargi_k[,-2]
      base_elargi <- base_elargi[order(base_elargi$id),]
      base_elargi <- base_elargi[,-(ncol(base_elargi))]
      
      return(list(donnees=base,donnees_elargi=base_elargi))
    }else
    {
      return(list(donnees=base))
    }
  }


extract_fond_leaflet_classes <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_classes") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      var_classes <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$var_ratio
      
      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
      dom <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],25,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(LIBELLE=cc,bb)
          dd <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],">")[[1]][11]+1,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][12]-1))
          dd <- as.numeric(str_replace_all(str_replace_all(dd,",",".")," ",""))
          fond <- cbind(val=dd,fond)
          ee <- map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
          fond <- cbind(classe=ee,fond)
          fond$var <- fond$val
          fond <- fond[,c("LIBELLE","var","val","classe","geometry")]
          names(fond) <- c("LIBELLE",var_classes,"val","classe","geometry")
          
          ff <- lapply(1:length(unique(fond$classe)), function(x) fond[fond$classe %in% rev(unique(fond$classe))[x],"classe"] <<- x)
          rm(cc,dd,ee,ff)
        }else
        {
          fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        }
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }
      
      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }
      
      if(!is.null(idx_legende))
      {
        label <- NULL
        palette <- NULL
        for(i in 1:length(idx_legende))
        {
          if(i==length(idx_legende))
          {
            titre_leg <- map$x$calls[[idx_legende[i]]]$args[11]
          }else
          {
            if(map$x$calls[[idx_legende[i]]]$method %in% "addMarkers")
            {
              label <- c(label,map$x$calls[[idx_legende[i]]]$args[11][[1]])
            }
            if(map$x$calls[[idx_legende[i]]]$method %in% "addPolygons")
            {
              palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[4][[1]]$fillColor)
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }
      
      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_classes,dom))
    }
  }


export_qgis_classes <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_classes(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des classes n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_classes(map) pour ajouter une legende de classes a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_maille_carte")) l <- c(l,"fond_maille_carte")
    if(any(list_fonds[[2]] %in% "fond_maille_elargi_carte")) l <- c(l,"fond_maille_elargi_carte")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    titre_leg <- list_fonds[[6]]
    table_classe <- list_fonds[[5]]
    variable_a_representer <- list_fonds[[7]]
    
    export_projet_qgis_classes(l,rep_sortie,sortie,titre1,titre2,source,titre_leg,table_classe,variable_a_representer,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_classes <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,titre_leg_classes,table_classe,variable_a_representer,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille_carte.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if (nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))
      }else
      {
        idcouche=l[i]  
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if (str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        if (titre_leg_classes=="")
        {
          bloclayeritem=modif_bloclayeritem(variable_a_representer,idcouche,"subgroup")
        }else
        {
          bloclayeritem=modif_bloclayeritem(titre_leg_classes,idcouche,"subgroup")
        }
      }else
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()      
      #cas ou le fond selectionne est la carte ou la legende
      if (str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        analyse_classes <- read_sf(chemincouche)
        geometrie=attr(analyse_classes$geometry[[1]],"class")[2]
        projcouche=st_crs(analyse_classes$geometry)$proj4string
        attr="attr='classe'"
        typeanalyse="categorizedSymbol"
        BLOCSYMBOLS=data.frame()
        BLOCCATEGORIES=data.frame(V1="                <categories>")
        BLOCVECTOR=data.frame()
        
        for (j in 1:dim(table_classe)[1])
        {
          #creer le bloc categories
          symbol=name=as.character(j)
          value=table_classe[j,1]
          label=table_classe[j,2]
          #value=label=table_classe[j,2]
          temp=modif_bloccategories(symbol,value,label)
          BLOCCATEGORIES=rbind(BLOCCATEGORIES,temp)
          #creer autant de bloc symbols que de classes avec le bon canevas symbols
          epaisseurbordure=0.5
          stylebordure="solid"
          
          couleurbordure="255,255,255"
          couleurfond=as.character(table_classe[j,3])
          remplissagefond="solid"
          temp=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
          BLOCSYMBOLS=rbind(BLOCSYMBOLS,temp)
          #MEP
          blocvector=modif_blocvectorClassification(table_classe[j,2])
          blocvector=data.frame(V1=c(blocvector[1:2,],temp[,1],blocvector[4:5,]))
          BLOCVECTOR=rbind(BLOCVECTOR,blocvector)  
        }
        BLOCCATEGORIES=rbind(BLOCCATEGORIES,data.frame(V1="                </categories>"))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],BLOCVECTOR[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }else
      {
        #cas ou le fond selectionne n'est pas la carte ni la legende, ou legende saphir ou rond avec 2classes ou legende ronds qd typana=classes'
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        couleurfond="255,255,255"
        
        remplissagefond="no"
        
        if (l[i] %in% c("fond_departement","fond_region","fond_pays"))
        {
          couleurbordure="128,128,128"
        }else if (l[i]=="fond_territoire")
        {
          couleurbordure="191,191,191"
        }else
        {
          couleurbordure="0,0,0"
        }
        
        stylebordure="solid"
        if (l[i] %in% c("fond_classes","fond_lignes","fond_departement","fond_pays","fond_territoire"))
        {
          epaisseurbordure=0.26
        }else
        {
          epaisseurbordure=0.5
        }
        
        fond <- read_sf(chemincouche)
        projcouche=st_crs(fond)$proj4string
        geometrie=attr(fond$geometry[[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]][[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]],"class")[2]
        
        if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
        {
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }
        
        blocvector=modif_blocvectorClassification(nomcouche)
        blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }


#############
### RP_AC ###
#############

extract_fond_leaflet_ronds_classes <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_carte_ronds <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    idx_legende_ronds <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_ronds_classes") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="carte_ronds_classes")  idx_carte_ronds <- c(idx_carte_ronds,i)
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
      }
      if(!is.null(idx_legende)) # la legende de classes existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
      if(!is.null(idx_legende_ronds)) # la legende de ronds existe
      {
        if(map$x$calls[[i]]$method %in% "addPolylines")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
      }
    }
    
    if(is.null(idx_legende) | is.null(idx_legende_ronds))
    {
      return(NULL)
    }else
    {
      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],25,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(LIBELLE=cc,bb)
          rm(cc)
        }else
        {
          fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        }
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      for(i in 1:length(idx_carte_ronds))
      {
        var_classes <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$var_ratio
        var_ronds <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$var_volume
        dom <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$dom
        
        centres_ronds <- data_frame(lng=map$x$calls[[idx_carte_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_carte_ronds[i]]]$args[[1]])
        aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
        bb <- do.call("rbind",aa)
        cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$code_epsg))
        dd <- st_buffer(cc, map$x$calls[[idx_carte_ronds[i]]]$args[[3]])
        
        ee <- sapply(1:length(map$x$calls[[idx_carte_ronds[i]]]$args[[7]]), function(y) substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],25,str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],"<")[[1]][3]-1))
        fond <- cbind(LIBELLE=ee,dd)
        ff <- sapply(1:length(map$x$calls[[idx_carte_ronds[i]]]$args[[7]]), function(y) substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],">")[[1]][11]+1,str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],"<")[[1]][12]-1))
        ff <- as.numeric(str_replace_all(str_replace_all(ff,",",".")," ",""))
        fond <- cbind(VAR_VOLUME=ff,fond)
        rm(aa,bb,cc,dd,ee,ff)
        
        col_bor <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$color
        fond <- cbind(COL_BOR=col_bor,fond)
        col <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$fillColor
        fond <- cbind(classe=col,fond)
        val <- str_replace_all(substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]],str_locate(map$x$calls[[idx_carte_ronds[i]]]$args[[7]],var_classes)[,2]+40,nchar(map$x$calls[[idx_carte_ronds[i]]]$args[[7]])-11)," ","")
        val <- as.numeric(str_replace_all(val,",","."))
        fond <- cbind(val=val,fond)
        fond$var <- fond$val
        ronds_pl <- fond[,c("LIBELLE","var","VAR_VOLUME","COL_BOR","val","classe","geometry")]
        names(ronds_pl) <- c("LIBELLE",var_classes,var_ronds,"COL_BOR","val","classe","geometry")
        rm(fond)
        
        nb_classes <- length(unique(map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$fillColor))
        pal_classes <- unique(map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$fillColor)
        
        palette <- recup_palette(stylePalette=map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$style)
        pal_classes_pos <- palette[[1]]
        pal_classes_neg <- palette[[2]]
        
        pal_classes_pos <- pal_classes_pos[pal_classes_pos %in% pal_classes]
        pal_classes_neg <- pal_classes_neg[pal_classes_neg %in% pal_classes]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
        
        gg <- lapply(1:length(pal_classes), function(x) ronds_pl[ronds_pl$classe %in% rev(pal_classes)[x],"classe"] <<- x)
        rm(gg)
        
        list_fonds[[l]] <- ronds_pl
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$nom_fond)
        l <- l+1
      }
      
      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }
      
      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }
      
      if(!is.null(idx_legende))
      {
        label <- NULL
        palette <- NULL
        for(i in 1:length(idx_legende))
        {
          if(i==length(idx_legende))
          {
            titre_leg <- map$x$calls[[idx_legende[i]]]$args[[11]]
          }else
          {
            if(map$x$calls[[idx_legende[i]]]$method %in% "addMarkers")
            {
              label <- c(label,map$x$calls[[idx_legende[i]]]$args[[11]])
            }
            if(map$x$calls[[idx_legende[i]]]$method %in% "addPolygons")
            {
              palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor)
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }
      
      if(!is.null(idx_legende_ronds))
      {
        for(i in 1:length(idx_legende_ronds))
        {
          if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addCircles")
          {
            centres_ronds <- data_frame(lng=map$x$calls[[idx_legende_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_legende_ronds[i]]]$args[[1]])
            aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
            bb <- do.call("rbind",aa)
            cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$code_epsg))
            dd <- st_buffer(cc, map$x$calls[[idx_legende_ronds[i]]]$args[[3]])
            
            val <- c(map$x$calls[[idx_legende_ronds[i]]]$args[[7]][1],map$x$calls[[idx_legende_ronds[i]]]$args[[7]][2])
            ronds_pl_leg <- cbind(VAL=val,dd)
            
            list_fonds[[l]] <- ronds_pl_leg
            nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$nom_fond)
            l <- l+1
          }
          
          if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addPolylines")
          {
            # Pour l'export Qgis en projection locale
            x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
            y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
            pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
            x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-min(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"]))/3
            y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
            pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
            ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl)
            
            x1_petit_pl <- x1_grand_pl
            y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
            pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
            x2_petit_pl <- x2_grand_pl
            y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
            pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
            ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl)
            
            lignes_pl <- st_sf(st_geometry(st_multilinestring(list(ligne_grand_pl,ligne_petit_pl))))
            lignes_pl <- st_set_crs(lignes_pl,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$code_epsg))
            
            list_fonds[[l]] <- lignes_pl
            nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$nom_fond)
            l <- l+1
          }
        }
      }
      
      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_classes,var_ronds,dom))
    }
  }


export_qgis_ronds_classes <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_ronds_classes(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des ronds ou des classes n'a pas ete creee. Veuillez svp utiliser les fonctions add_legende_ronds(map) et add_legende_classes(map) pour ajouter une legende a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_ronds_carte")) l <- c(l,"fond_ronds_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_elargi_carte")) l <- c(l,"fond_ronds_elargi_carte")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")
    if(any(list_fonds[[2]] %in% "fond_maille_elargi")) l <- c(l,"fond_maille_elargi")
    if(any(list_fonds[[2]] %in% "fond_lignes_leg")) l <- c(l,"fond_lignes_leg")
    if(any(list_fonds[[2]] %in% "fond_ronds_leg_carte")) l <- c(l,"fond_ronds_leg_carte")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    titre_leg <- list_fonds[[6]]
    table_classe <- list_fonds[[5]]
    variable_a_representer <- list_fonds[[7]]
    
    export_projet_qgis_ronds_classes(l,rep_sortie,sortie,titre1,titre2,source,titre_leg,table_classe,variable_a_representer,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_ronds_classes <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,titre_leg_classes,table_classe,variable_a_representer,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille.shp"))
    
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if (nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))  
      }else
      {
        idcouche=l[i]  
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if(str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        if(titre_leg_classes=="")
        {
          bloclayeritem=modif_bloclayeritem(variable_a_representer,idcouche,"subgroup")
        }else
        {
          bloclayeritem=modif_bloclayeritem(titre_leg_classes,idcouche,"subgroup")
        }
      }else
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()      
      #cas ou le fond selectionne est la carte ou la legende
      if(str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        analyse_classes <- read_sf(chemincouche)
        geometrie=attr(analyse_classes$geometry[[1]],"class")[2]
        projcouche=st_crs(analyse_classes$geometry)$proj4string
        
        #preparation des param du BLOCSYMBOL
        if(str_sub(l[i][length(l[i])],start=-9)=="leg_carte")
        {
          attr=""
          name="0"
          typeanalyse="singleSymbol"
          
          stylebordure="solid"
          epaisseurbordure=0.26
          couleurbordure="0,0,0"
          couleurfond="transparent"
          
          remplissagefond="solid"
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
          
          toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
          toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
          BLOCPROJECT=rbind(BLOCPROJECT,toto)
        }else
        {
          attr="attr='classe'"
          typeanalyse="categorizedSymbol"
          BLOCSYMBOLS=data.frame()
          BLOCCATEGORIES=data.frame(V1="                <categories>")
          BLOCVECTOR=data.frame()
          
          for(j in 1:dim(table_classe)[1])
          {
            #creer le bloc categories
            symbol=name=as.character(j)
            value=table_classe[j,1]
            label=table_classe[j,2]
            #value=label=table_classe[j,2]
            temp=modif_bloccategories(symbol,value,label)
            BLOCCATEGORIES=rbind(BLOCCATEGORIES,temp)
            #creer autant de bloc symbols que de classes avec le bon canevas symbols
            stylebordure="solid"
            epaisseurbordure=0.26
            couleurbordure=unique(analyse_classes$COL_BOR)
            couleurfond=as.character(table_classe[j,3])
            remplissagefond="solid"
            temp=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
            BLOCSYMBOLS=rbind(BLOCSYMBOLS,temp)  
            #MEP
            blocvector=modif_blocvectorClassification(table_classe[j,2])
            blocvector=data.frame(V1=c(blocvector[1:2,],temp[,1],blocvector[4:5,]))
            BLOCVECTOR=rbind(BLOCVECTOR,blocvector)  
          }
          BLOCCATEGORIES=rbind(BLOCCATEGORIES,data.frame(V1="                </categories>"))
          bloclayeritem=data.frame(V1=c(bloclayeritem[1,],BLOCVECTOR[,1],bloclayeritem[3,]))
          BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        }
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }else
      {
        #cas oC9 le fond selectionne n'est pas la carte ni la legende, ou legende saphir ou rond avec 2classes ou legende ronds qd typana=classes'
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        couleurfond="255,255,255"
        
        remplissagefond="no"
        
        if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
        {
          couleurbordure="128,128,128"
        }else if (l[i]=="fond_maille_elargi")
        {
          couleurbordure="200,200,200"
        }else if (l[i]=="fond_territoire")
        {
          couleurbordure="191,191,191"
        }else
        {
          couleurbordure="0,0,0"
        }
        
        stylebordure="solid"
        if (l[i] %in% c("fond_maille","fond_maille_elargi","fond_lignes_leg","fond_departement","fond_pays","fond_territoire"))
        {
          epaisseurbordure=0.26
        }else
        {
          epaisseurbordure=0.5
        }
        
        fond <- read_sf(chemincouche)
        projcouche=st_crs(fond)$proj4string
        geometrie=attr(fond$geometry[[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]][[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]],"class")[2]
        
        if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
        {
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }else
        {
          if (geometrie %in% c("MULTILINESTRING"))
          {
            BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
          }else
          {#inutile??
            BLOCSYMBOLS=balises_qgis()[[6]]  
          }
        }
        
        blocvector=modif_blocvectorClassification(nomcouche)
        blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }


#############
### AC_RP ###
#############

extract_fond_leaflet_classes_ronds <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_carte_ronds <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    idx_legende_ronds <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_classes_ronds") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="carte_classes_ronds") idx_carte_ronds <- c(idx_carte_ronds,i)
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_classes") idx_legende <- c(idx_legende,i)
          if(!is.null(idx_legende_ronds))
          {
            if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
          }
        }
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
        if(!is.null(idx_legende_ronds))
        {
          if(map$x$calls[[i]]$method %in% "addPolylines")
          {
            if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
          }
        }
      }
    }
    
    if(is.null(idx_legende) | is.null(idx_legende_ronds))
    {
      return(NULL)
    }else
    {
      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
      dom <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
        {
          var_classes <- map$x$calls[[idx_carte[i]]]$args[[3]]$var_ratio
          
          cc <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],25,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(LIBELLE=cc,bb)
          dd <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],">")[[1]][11]+1,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][12]-1))
          dd <- as.numeric(str_replace_all(str_replace_all(dd,",",".")," ",""))
          fond <- cbind(val=dd,fond)
          ee <- map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
          fond <- cbind(classe=ee,fond)
          fond$var <- fond$val
          fond <- fond[,c("LIBELLE","var","val","classe","geometry")]
          names(fond) <- c("LIBELLE",var_classes,"val","classe","geometry")
          
          ff <- lapply(1:length(unique(fond$classe)), function(x) fond[fond$classe %in% rev(unique(fond$classe))[x],"classe"] <<- x)
          rm(cc,dd,ee,ff)
        }else
        {
          fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        }
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      for(i in 1:length(idx_carte_ronds))
      {
        var_ronds <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$var_volume
        dom <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$dom
        
        centres_ronds <- data_frame(lng=map$x$calls[[idx_carte_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_carte_ronds[i]]]$args[[1]])
        aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
        bb <- do.call("rbind",aa)
        cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$code_epsg))
        dd <- st_buffer(cc, map$x$calls[[idx_carte_ronds[i]]]$args[[3]])
        
        ee <- sapply(1:length(map$x$calls[[idx_carte_ronds[i]]]$args[[7]]), function(y) substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],25,str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],"<")[[1]][3]-1))
        fond <- cbind(LIBELLE=ee,dd)
        ff <- sapply(1:length(map$x$calls[[idx_carte_ronds[i]]]$args[[7]]), function(y) substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],">")[[1]][11]+1,str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]][[y]],"<")[[1]][12]-1))
        ff <- as.numeric(str_replace_all(str_replace_all(ff,",",".")," ",""))
        fond <- cbind(VAR_VOLUME=ff,fond)
        rm(aa,bb,cc,dd,ee,ff)
        
        col_bor <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$color
        fond <- cbind(COL_BOR=col_bor,fond)
        ronds_pl <- fond[,c("LIBELLE","VAR_VOLUME","COL_BOR","geometry")]
        names(ronds_pl) <- c("LIBELLE",var_ronds,"COL_BOR","geometry")
        rm(fond)
        
        list_fonds[[l]] <- ronds_pl
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$nom_fond)
        l <- l+1
      }
      
      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }
      
      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }
      
      if(!is.null(idx_legende))
      {
        label <- NULL
        palette <- NULL
        for(i in 1:length(idx_legende))
        {
          if(i==length(idx_legende))
          {
            titre_leg <- map$x$calls[[idx_legende[i]]]$args[[11]]
          }else
          {
            if(map$x$calls[[idx_legende[i]]]$method %in% "addMarkers")
            {
              label <- c(label,map$x$calls[[idx_legende[i]]]$args[[11]])
            }
            if(map$x$calls[[idx_legende[i]]]$method %in% "addPolygons")
            {
              palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor)
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }
      
      if(!is.null(idx_legende_ronds))
      {
        for(i in 1:length(idx_legende_ronds))
        {
          if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addCircles")
          {
            centres_ronds <- data_frame(lng=map$x$calls[[idx_legende_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_legende_ronds[i]]]$args[[1]])
            aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
            bb <- do.call("rbind",aa)
            cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$code_epsg))
            dd <- st_buffer(cc, map$x$calls[[idx_legende_ronds[i]]]$args[[3]])
            
            val <- c(map$x$calls[[idx_legende_ronds[i]]]$args[[7]][1],map$x$calls[[idx_legende_ronds[i]]]$args[[7]][2])
            ronds_pl_leg <- cbind(VAL=val,dd)
            
            list_fonds[[l]] <- ronds_pl_leg
            nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$nom_fond)
            l <- l+1
          }
          
          if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addPolylines")
          {
            # Pour l'export Qgis en projection locale
            x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
            y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
            pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
            x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-min(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"]))/3
            y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
            pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
            ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl)
            
            x1_petit_pl <- x1_grand_pl
            y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
            pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
            x2_petit_pl <- x2_grand_pl
            y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
            pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
            ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl)
            
            lignes_pl <- st_sf(st_geometry(st_multilinestring(list(ligne_grand_pl,ligne_petit_pl))))
            lignes_pl <- st_set_crs(lignes_pl,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$code_epsg))
            
            list_fonds[[l]] <- lignes_pl
            nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$nom_fond)
            l <- l+1
          }
        }
      }
      
      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_classes,var_ronds,dom))
    }
  }


export_qgis_classes_ronds <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_classes_ronds(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des ronds ou des classes n'a pas ete creee. Veuillez svp utiliser les fonctions add_legende_ronds(map) et add_legende_classes(map) pour ajouter une legende a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_ronds_carte")) l <- c(l,"fond_ronds_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_elargi_carte")) l <- c(l,"fond_ronds_elargi_carte")
    
    if(any(list_fonds[[2]] %in% "fond_lignes_leg")) l <- c(l,"fond_lignes_leg")
    if(any(list_fonds[[2]] %in% "fond_ronds_leg_carte")) l <- c(l,"fond_ronds_leg_carte")
    
    if(any(list_fonds[[2]] %in% "fond_maille_carte")) l <- c(l,"fond_maille_carte")
    if(any(list_fonds[[2]] %in% "fond_maille_elargi_carte")) l <- c(l,"fond_maille_elargi_carte")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    titre_leg <- list_fonds[[6]]
    table_classe <- list_fonds[[5]]
    variable_a_representer <- list_fonds[[7]]
    
    export_projet_qgis_classes_ronds(l,rep_sortie,sortie,titre1,titre2,source,titre_leg,table_classe,variable_a_representer,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_classes_ronds <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,titre_leg_classes,table_classe,variable_a_representer,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille_carte.shp"))
    
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if (nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))  
      }else
      {
        idcouche=l[i]  
      }
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if (str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        if (titre_leg_classes=="")
        {
          bloclayeritem=modif_bloclayeritem(variable_a_representer,idcouche,"subgroup")
        }else
        {
          bloclayeritem=modif_bloclayeritem(titre_leg_classes,idcouche,"subgroup")
        }
      }else
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()      
      #cas oC9 le fond selectionne est la carte ou la legende, sauf legendes saphir et rond si 2classes et sauf legende ronds quand typana=classes'
      if (str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        analyse_classes <- read_sf(chemincouche)
        geometrie=attr(analyse_classes$geometry[[1]],"class")[2]
        projcouche=st_crs(analyse_classes$geometry)$proj4string
        if (str_sub(l[i][length(l[i])],start=-11)=="ronds_carte" | str_sub(l[i][length(l[i])],start=-18)=="ronds_elargi_carte")
        {
          attr=""
          name="0"
          typeanalyse="singleSymbol"
          #preparation des param du BLOCSYMBOL
          couleurbordure=unique(analyse_classes$COL_BOR)
          stylebordure="solid"
          epaisseurbordure=0.26
          couleurfond="transparent"
          
          remplissagefond="solid"
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }else if (str_sub(l[i][length(l[i])],start=-9)=="leg_carte")
        {
          attr=""
          name="0"
          typeanalyse="singleSymbol"
          
          stylebordure="solid"
          epaisseurbordure=0.26
          couleurbordure="0,0,0"
          couleurfond="transparent"
          
          remplissagefond="solid"
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
          
          toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
          toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
          BLOCPROJECT=rbind(BLOCPROJECT,toto)
        }else
        {
          attr="attr='classe'"
          typeanalyse="categorizedSymbol"
          BLOCSYMBOLS=data.frame()
          BLOCCATEGORIES=data.frame(V1="                <categories>")
          BLOCVECTOR=data.frame()
          
          for (j in 1:dim(table_classe)[1])
          {
            #creer le bloc categories
            symbol=name=as.character(j)
            value=table_classe[j,1]
            label=table_classe[j,2]
            #value=label=table_classe[j,2]
            temp=modif_bloccategories(symbol,value,label)
            BLOCCATEGORIES=rbind(BLOCCATEGORIES,temp)
            #creer autant de bloc symbols que de classes avec le bon canevas symbols
            epaisseurbordure=0
            stylebordure="no"
            
            couleurbordure="0,0,0"
            couleurfond=as.character(table_classe[j,3])
            remplissagefond="solid"
            temp=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
            BLOCSYMBOLS=rbind(BLOCSYMBOLS,temp)  
            #MEP
            blocvector=modif_blocvectorClassification(table_classe[j,2])
            blocvector=data.frame(V1=c(blocvector[1:2,],temp[,1],blocvector[4:5,]))
            BLOCVECTOR=rbind(BLOCVECTOR,blocvector)  
          }
          BLOCCATEGORIES=rbind(BLOCCATEGORIES,data.frame(V1="                </categories>"))
          bloclayeritem=data.frame(V1=c(bloclayeritem[1,],BLOCVECTOR[,1],bloclayeritem[3,]))
          BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        }
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }else
      {
        #cas ou le fond selectionne n'est pas la carte ni la legende
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        couleurfond="255,255,255"
        
        remplissagefond="no"
        
        if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
        {
          couleurbordure="128,128,128"
        }else if (l[i]=="fond_maille_elargi")
        {
          couleurbordure="200,200,200"
        }else if (l[i]=="fond_territoire")
        {
          couleurbordure="191,191,191"
        }else
        {
          couleurbordure="0,0,0"
        }
        
        stylebordure="solid"
        if (l[i] %in% c("fond_maille","fond_maille_elargi","fond_lignes","fond_departement","fond_pays","fond_territoire"))
        {
          epaisseurbordure=0.26
        }else
        {
          epaisseurbordure=0.5
        }
        
        fond <- read_sf(chemincouche)
        projcouche=st_crs(fond)$proj4string
        geometrie=attr(fond$geometry[[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]][[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]],"class")[2]
        
        if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
        {
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }else
        {
          if (geometrie %in% c("MULTILINESTRING"))
          {
            BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
          }else
          {#inutile??
            BLOCSYMBOLS=balises_qgis()[[6]]  
          }
        }
        
        blocvector=modif_blocvectorClassification(nomcouche)
        blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }


##########
### TY ###
##########

k_typo <-
  function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,variable_jointure_donnees_k,variable_a_representer_k)
  {
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% donnees_k[,variable_jointure_donnees_k],]
    
    if(is.numeric(donnees_k[,variable_a_representer_k])==T)
    {
      donnees_k[is.na(donnees_k)] <- 9999
      donnees_k <- donnees_k[match(as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],donnees_k[,variable_jointure_donnees_k]),]
      
      fond_carto_k$valeur <- donnees_k[,variable_a_representer_k]
      fond_carto_k[,variable_a_representer_k] <- as.numeric(donnees_k[,variable_a_representer_k])
      fond_carto_k$classe <- fond_carto_k$valeur
      fond_carto_k <- fond_carto_k[order(as.data.frame(fond_carto_k)[,variable_a_representer_k]),]
      
      return(list(analyse=fond_carto_k))
    }else
    {
      donnees_k[is.na(donnees_k)] <- "Non classe"
      
      donnees_k <- donnees_k[match(as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],donnees_k[,variable_jointure_donnees_k]),]
      valeurs <- cbind(var=as.character(sort(unique(donnees_k[,variable_a_representer_k]))), classe=seq(1,length(unique(donnees_k[,variable_a_representer_k])),1))
      donnees_k <- merge(donnees_k,valeurs,by.x=variable_a_representer_k,by.y="var")
      donnees_k <- donnees_k[order(donnees_k[,variable_a_representer_k]),]
      
      fond_carto_k_data <- unique(merge(donnees_k[,c(variable_jointure_donnees_k,variable_a_representer_k,"classe")],fond_carto_k,by.x=variable_jointure_donnees_k,by.y=variable_jointure_fond_carto_k))
      nb_y <- length(names(fond_carto_k_data)[grep("..y",names(fond_carto_k_data)[-grep("geom",names(fond_carto_k_data))])])
      if(nb_y>0)
      {
        idx_y <- grep("..y",names(fond_carto_k_data))
        idx_geom <- grep("geom",names(fond_carto_k_data))
        idx_y <- idx_y[-which(idx_y==idx_geom)]
        
        fond_carto_k_data <- fond_carto_k_data[,-idx_y[length(idx_y)]]
        names(fond_carto_k_data)[grep("..x",names(fond_carto_k_data))]<-sub(".x","",names(fond_carto_k_data)[grep("..x",names(fond_carto_k_data))])
      }
      fond_carto_k_data$valeur <- fond_carto_k_data[,variable_a_representer_k]
      fond_carto_k <- st_sf(fond_carto_k_data, crs=st_crs(fond_carto_k))
      fond_carto_k <- fond_carto_k[order(fond_carto_k$valeur),]
      
      return(list(analyse=fond_carto_k))
    }
  }


add_legende_typo <-
  function(map,titre=NULL,lng=NULL,lat=NULL,labels=NULL,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(labels)) if(any(class(labels)!="character")) msg_error4 <- "Les labels doivent etre un vecteur de type caractere / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    if(!is.null(labels))
    {
      labels<-iconv(labels,"latin1","utf8")
    }
    
    idx_carte <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche=="carte_typo") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
    
    coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
    
    lng_init <- lng
    lat_init <- lat
    if(is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init))) # La legende n'a pas encore ete creee, on la cree avec une position par defaut
    {
      lng <- map$x$fitBounds[[4]]
      lat <- map$x$fitBounds[[3]]
    }else if(is.null(idx_legende)) # La legende n'a pas encore ete creee, on la cree avec la position definie par l'utilisateur
    {
      # voir plus loin
    }else # l'utilisateur veut modifier la legende existante, on la supprime pour la recreer
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }
    
    if(is.null(idx_legende) | !is.null(idx_legende) & !(is.null(lng_init) | is.null(lat_init))) # Si la legende doit etre creee ou recreee
    {
      # on calcule idx_carte au cas oC9 la legende ait ete supprimee, c'est le nombre de polygons dans le leaflet
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          idx_carte <- c(idx_carte,i)
        }
      }
      
      nb_classes <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
      pal_classes <- unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor)
      
      # Coordonnees du point haut/gauche des rectangles de la legende
      decalage <- 0.7
      x_coord_rectangle <- lng
      for(i in 1:nb_classes)
      {
        if(i==1) #1er rectangle
        {
          y_coord_rectangle <- lat-coeff
        }else
        {
          y_coord_rectangle <- y_coord_rectangle-coeff*decalage
        }
        assign(paste0("rectangle_",i),list(matrix(c(x_coord_rectangle,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle,x_coord_rectangle+coeff*1,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle+coeff*0.5,x_coord_rectangle,y_coord_rectangle),ncol=2, byrow=TRUE)))
      }
      
      # On ajoute un cadre blanc autour de la legende
      y_coord_rectangle <- min(get(paste0("rectangle_",nb_classes))[[1]][,2])
      
      # leaflet du cadre blanc en 1er
      map <- addRectangles(map = map,
                           lng1 = lng-coeff*0.5, lat1 = lat+coeff*0.5,
                           lng2 = x_coord_rectangle+coeff*10, lat2 = y_coord_rectangle-coeff*0.8,
                           stroke = TRUE,
                           color = paste0("#2B3E50", ";background: #ffffff;
                                          border-left:2px solid #2B3E50;
                                          border-right:2px solid #2B3E50;
                                          border-top:2px solid #2B3E50;
                                          border-bottom:2px solid #2B3E50;
                                          border-radius: 5%"),
                           weight = 1,
                           options = pathOptions(clickable = F),
                           fill = T,
                           fillColor = "white",
                           fillOpacity = 0.5,
                           group=list(nom_couche="legende_typo")
                           )
      
      # leaflet rectangles et valeurs classes
      label_rectangle <- NULL
      txt <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[5]]
      aa <- sapply(1:length(txt), function(x) label_rectangle <<- c(label_rectangle,substring(txt[x],str_locate_all(txt[x],">")[[1]][11]+1,nchar(txt[x])-15)))
      label_rectangle <- unique(label_rectangle)
      
      if(is.null(labels))
      {
        labels <- label_rectangle
      }
      
      for(i in 1:nb_classes)
      {
        map <- addLabelOnlyMarkers(map = map,
                                   lng = (max(get(paste0("rectangle_",i))[[1]][,1])+coeff*0.1), lat = mean(get(paste0("rectangle_",i))[[1]][,2]),
                                   label = labels[i],
                                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                               style = list(
                                                                 "color" = "black",
                                                                 "font-size" = "12px"
                                                               )),
                                   group=list(nom_couche="legende_typo")
        )
      }
      
      # On cree les polygons ensemble a la fin de l'objet leaflet juste avant le titre
      for(i in 1:nb_classes)
      {
        map <- addPolygons(map = map, data = st_polygon(get(paste0("rectangle_",i))),
                           stroke = FALSE,
                           options = pathOptions(clickable = F),
                           fill = T,
                           fillColor = pal_classes[i],
                           fillOpacity = 1,
                           group=list(nom_couche="legende_typo")
        )
      }
      
      # leaflet titre
      x_titre <- min(st_coordinates(st_polygon(get("rectangle_1")))[,"X"])
      y_titre <- max(st_coordinates(st_polygon(get("rectangle_1")))[,"Y"])+coeff*0.4
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = x_titre, lat = y_titre,
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group=list(nom_couche="legende_typo")
      )
    }
    
    message(simpleMessage(paste0("[INFO] Les coordonn","\u00e9","es de la l\u00e9gende de la typologie sont : longitude (x) = ",lng," degr\u00e9 ; latitude (y) = ",lat," degr\u00e9")))
    return(map)
  }


set_couleur_typo <-
  function(map,paletteTypo=NULL,colBorder="white")
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(paletteTypo)) if(any(class(paletteTypo)!="character")) msg_error2 <- "La palette de la typologie doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error3 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    if(is.null(paletteTypo))
    {
      idx_carte <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="carte_typo") idx_carte <- c(idx_carte,i)
        }
      }
      nb_col <- length(unique(map$x$calls[[idx_carte[length(idx_carte)]]]$args[[4]]$fillColor))
      paletteTypo <- substr(rainbow(256)[nb_opposes(256)[1:nb_col]],1,7)
    }
    
    idx_carte <- NULL
    idx_legende <- NULL
    legende <- F
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche=="carte_typo")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_maille_typo_carte") idx_carte <- i
        }
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_typo") legende <- T
      }
      if(legende) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(!is.null(idx_carte))
    {
      couleur_analyse <- data.frame(col=map$x$calls[[idx_carte]]$args[[4]]$fillColor)
      couleur_analyse$id1 <- c(1:nrow(couleur_analyse))
      pal_anc <- data.frame(col=unique(couleur_analyse$col))
      pal_anc$id2 <- c(1:nrow(pal_anc))
      couleur_analyse <- merge(couleur_analyse,pal_anc,by="col")
      
      aa <- sapply(1:(length(paletteTypo)), function(x) couleur_analyse[couleur_analyse$id2==x,"col"] <<- paletteTypo[x])
      rm(aa)
      couleur_analyse <- couleur_analyse[order(couleur_analyse$id1),]
      couleur_analyse <- couleur_analyse$col
      
      map$x$calls[[idx_carte]]$args[[4]]$fillColor <- couleur_analyse
      
      map$x$calls[[idx_carte]]$args[[4]]$color <- colBorder
    }
    
    for(i in 1:length(idx_legende))
    {
      map$x$calls[[idx_legende[i]]]$args[[4]]$fillColor <- paletteTypo[i]
    }
    
    return(map)
  }


nb_opposes <-
  function(nb)
  {
    list_res <- list()
    if(round(log(nb)/log(2),0)!=log(nb)/log(2)) stop(simpleError("Le nombre doit etre de la forme 2^n"))
    for(m in 1:2)
    {
      res <- c(nb-nb/m,nb/2*m)
      if(m==1) n <- 4
      if(m==2) n <- 4/3
      ajout <- nb/n
      while(!ajout[1] %in% c(1,nb/2+1))
      {
        new_ajout <- NULL
        for(i in 1:length(ajout))
        {
          for(j in 1:length(res))
            new_ajout <- c(new_ajout,(ajout[i]+res[j])/2)
        }
        res <- c(res,ajout)
        ajout <- unique(new_ajout)
      }
      res <- c(res,ajout)
      res <- res+1
      res <- res[-2]
      
      list_res[[m]] <- res
    }
    
    res <- vector()
    aa <- sapply(c(1:length(list_res[[1]])), function(x) res <<- c(res,list_res[[1]][x],list_res[[2]][x]))
    
    return(res)
  }


extract_fond_leaflet_typo <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_typo") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_typo") idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      var_typo <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$var_typo
      
      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$code_epsg
      dom <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[3]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],25,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(LIBELLE=cc,bb)
          dd <- sapply(1:length(map$x$calls[[idx_carte[i]]]$args[[5]]), function(y) substring(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],">")[[1]][11]+1,str_locate_all(map$x$calls[[idx_carte[i]]]$args[[5]][[y]],"<")[[1]][12]-1))
          dd <- as.numeric(str_replace_all(str_replace_all(dd,",",".")," ",""))
          fond <- cbind(val=dd,fond)
          ee <- map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
          fond <- cbind(classe=ee,fond)
          fond$var <- fond$val
          fond <- fond[,c("LIBELLE","var","val","classe","geometry")]
          names(fond) <- c("LIBELLE",var_typo,"val","classe","geometry")
          
          ff <- lapply(1:length(unique(fond$classe)), function(x) fond[fond$classe %in% rev(unique(fond$classe))[x],"classe"] <<- x)
          rm(cc,dd,ee,ff)
        }else
        {
          fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        }
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }
      
      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }
      
      if(!is.null(idx_legende))
      {
        label <- NULL
        palette <- NULL
        for(i in 1:length(idx_legende))
        {
          if(i==length(idx_legende))
          {
            titre_leg <- map$x$calls[[idx_legende[i]]]$args[11]
          }else
          {
            if(map$x$calls[[idx_legende[i]]]$method %in% "addMarkers")
            {
              label <- c(label,map$x$calls[[idx_legende[i]]]$args[11][[1]])
            }
            if(map$x$calls[[idx_legende[i]]]$method %in% "addPolygons")
            {
              palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[4][[1]]$fillColor)
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }
      
      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_typo,dom))
    }
  }


export_qgis_typo <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_typo(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende de la typologie n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_typo(map) pour ajouter une legende de typologie a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_maille_typo_carte")) l <- c(l,"fond_maille_typo_carte")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    titre_leg <- list_fonds[[6]]
    table_typo <- list_fonds[[5]]
    variable_a_representer <- list_fonds[[7]]
    
    export_projet_qgis_typo(l,rep_sortie,sortie,titre1,titre2,source,titre_leg,table_typo,variable_a_representer,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_typo <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,titre_leg_classes,table_typo,variable_a_representer,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille_typo_carte.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if (nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))  
      }else
      {
        idcouche=l[i]  
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if (str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        if (titre_leg_classes=="")
        {
          bloclayeritem=modif_bloclayeritem(variable_a_representer,idcouche,"subgroup")
        }else
        {
          bloclayeritem=modif_bloclayeritem(titre_leg_classes,idcouche,"subgroup")
        }
      }else
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()      
      #cas oC9 le fond selectionne est la carte ou la legende, sauf legendes saphir et rond si 2classes et sauf legende ronds quand typana=classes'
      if (str_sub(l[i][length(l[i])],start=-5)=="carte")
      {
        analyse_typo <- read_sf(chemincouche)
        geometrie=attr(analyse_typo$geometry[[1]],"class")[2]
        projcouche=st_crs(analyse_typo$geometry)$proj4string
        attr="attr='classe'"
        typeanalyse="categorizedSymbol"
        BLOCSYMBOLS=data.frame()
        BLOCCATEGORIES=data.frame(V1="                <categories>")
        BLOCVECTOR=data.frame()
        
        for (j in 1:nrow(table_typo))
        {
          #creer le bloc categories
          symbol=name=as.character(j)
          value=j
          label=table_typo$label[j]
          #value=label=table_classe[j,2]
          temp=modif_bloccategories(symbol,value,label)
          BLOCCATEGORIES=rbind(BLOCCATEGORIES,temp)
          #creer autant de bloc symbols que de classes avec le bon canevas symbols
          epaisseurbordure=0.5
          stylebordure="solid"
          
          couleurbordure="255,255,255"
          couleurfond=unique(table_typo$couleurs[order(table_typo$label)])[j]
          remplissagefond="solid"
          temp=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
          BLOCSYMBOLS=rbind(BLOCSYMBOLS,temp)  
          #MEP
          blocvector=modif_blocvectorClassification(table_typo$label[j])
          blocvector=data.frame(V1=c(blocvector[1:2,],temp[,1],blocvector[4:5,]))
          BLOCVECTOR=rbind(BLOCVECTOR,blocvector)  
        }
        BLOCCATEGORIES=rbind(BLOCCATEGORIES,data.frame(V1="                </categories>"))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],BLOCVECTOR[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }else
      {
        #cas oC9 le fond selectionne n'est pas la carte ni la legende, ou legende saphir ou rond avec 2classes ou legende ronds qd typana=classes'
        attr=""
        name="0"
        typeanalyse="singleSymbol"
        couleurfond="255,255,255"
        
        remplissagefond="no"
        
        if (l[i] %in% c("fond_departement","fond_region","fond_pays"))
        {
          couleurbordure="128,128,128"
        }else if (l[i]=="fond_territoire")
        {
          couleurbordure="191,191,191"
        }else
        {
          couleurbordure="0,0,0"
        }
        
        stylebordure="solid"
        if (l[i] %in% c("fond_typo","fond_departement","fond_pays","fond_territoire"))
        {
          epaisseurbordure=0.26
        }else
        {
          epaisseurbordure=0.5
        }
        
        fond <- read_sf(chemincouche)
        projcouche=st_crs(fond)$proj4string
        geometrie=attr(fond$geometry[[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]][[1]],"class")[2]
        
        if(is.null(geometrie))
          geometrie=attr(fond[[1]],"class")[2]
        
        if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
        {
          BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
        }
        
        blocvector=modif_blocvectorClassification(nomcouche)
        blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
        bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
        BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
        
        toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
        toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
        BLOCPROJECT=rbind(BLOCPROJECT,toto)
      }
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }


##########
### OU ###
##########

k_oursins <-
  function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,var_depart_k,var_arrivee_k,var_flux_k,decalage_aller_retour_k,decalage_centroid_k)
  {
    suppressWarnings(donnees_k <- donnees_k[!is.na(donnees_k[,var_flux_k]),])
    
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% c(donnees_k[,var_depart_k],donnees_k[,var_arrivee_k]),]
    suppressWarnings(coord <- data.frame(CODGEO=as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],X=st_coordinates(st_centroid(fond_carto_k))[,1],Y=st_coordinates(st_centroid(fond_carto_k))[,2]))
    
    donnees_k <- donnees_k[donnees_k[,var_depart_k] %in% coord$CODGEO,]
    donnees_k <- donnees_k[donnees_k[,var_arrivee_k] %in% coord$CODGEO,]
    donnees_k <- donnees_k[donnees_k[,var_depart_k]!=donnees_k[,var_arrivee_k],]
    
    donnees_k$AR <- 0
    nb_ar <- 1
    for(i in 1:nrow(donnees_k))
    {
      if(donnees_k[i,"AR"]==0)
      {
        doublon <- which(donnees_k[i,"CODE1"]==donnees_k$CODE2 & donnees_k[i,"CODE2"]==donnees_k$CODE1)
        if(length(doublon)>0)
        {
          donnees_k[i,"AR"] <- nb_ar
          donnees_k[doublon,"AR"] <- nb_ar
          nb_ar <- nb_ar + 1
        }
      }
    }
    
    base <- merge(merge(donnees_k,coord,by.x=var_depart_k,by.y="CODGEO"),coord,by.x=var_arrivee_k,by.y="CODGEO",suffixes = c(".R",".T"))
    
    # ETAPE 1 : decalage des lignes du centroid
    
    for(i in 1:nrow(base))
    {
      nb_ar <- base[i,"AR"]
      
      # Coordonnees des points A et B
      Ax <- base[i,"X.R"]
      Ay <- base[i,"Y.R"]
      Bx <- base[i,"X.T"]
      By <- base[i,"Y.T"]
      
      if(decalage_centroid_k>0)
      {
        rapport <- (((Ax-Bx)^2+(Ay-By)^2)^(1/2))/(decalage_centroid_k*1000)
        
        #(Ax-Bx)/(Ax-Cx)=rapport
        Ax <- Ax-((Ax-Bx)/rapport)
        Ay <- Ay-((Ay-By)/rapport)
        #(Bx-Ax)/(Bx-Dx)=rapport
        Bx <- Bx-((Bx-Ax)/rapport)
        By <- By-((By-Ay)/rapport)
      }
      
      base[i,"X.R"] <- Ax
      base[i,"Y.R"] <- Ay
      base[i,"X.T"] <- Bx
      base[i,"Y.T"] <- By
    }
    
    # ETAPE 2 : decalage des lignes aller et retour
    
    base_0 <- base[base$AR==0,] # sans aller-retour
    base_AR <- base[base$AR!=0,] # avec aller-retour
    decalage <- (decalage_aller_retour_k/2)*1000
    
    l <- list()
    for(i in 1:nrow(base_AR))
    {
      nb_ar <- base_AR[i,"AR"]
      
      # Coordonnees des points A et B
      Ax <- base_AR[i,"X.R"]
      Ay <- base_AR[i,"Y.R"]
      Bx <- base_AR[i,"X.T"]
      By <- base_AR[i,"Y.T"]
      
      if(Ax==Bx) # La ligne est verticale
      {
        A1x <- Ax
        A1y <- Ay+decalage
        A2x <- Ax
        A2y <- Ay-decalage
        B1x <- Bx
        B1y <- By+decalage
        B2x <- Bx
        B2y <- By-decalage
      }else if(Ay==By) # La ligne est horizontale
      {
        A1x <- Ax+decalage
        A1y <- Ay
        A2x <- Ax+decalage
        A2y <- Ay
        B1x <- Bx+decalage
        B1y <- By
        B2x <- Bx+decalage
        B2y <- By
      }else
      {
        # On applique le produit scalaire pour calculer les nouvelles coordonnees des points A1, B1 et A2, B2
        
        # On fixe le vecteur S1 pour calculer un coeff multiplicateur M
        S1 <- 1 # Sx-Ax
        # S2 = Sy-Ay
        
        #(Sx-Ax)*(Ax-Bx)+S2*(Ay-By)=0 : produit scalaire
        #S1*(Ax-Bx)+S2*(Ay-By)=0
        S2 <- -S1*(Ax-Bx)/(Ay-By)
        N <- (1+S2^2)^(1/2)
        M <- decalage/N
        # (M^2+(M*S2)^2)^(1/2) = decalage_aller_retour_k/2 : verif
        
        A1x <- Ax+M
        A1y <- Ay+M*S2
        A2x <- Ax-M
        A2y <- Ay-M*S2
        B1x <- Bx+M
        B1y <- By+M*S2
        B2x <- Bx-M
        B2y <- By-M*S2
      }
      
      # Nouvelles coordonnees des points de A vers B puis de B vers A
      doublon <- which(base_AR[,"AR"]==nb_ar)
      
      base_AR[doublon[1],"X.R"] <- A1x
      base_AR[doublon[1],"Y.R"] <- A1y
      base_AR[doublon[1],"X.T"] <- B1x
      base_AR[doublon[1],"Y.T"] <- B1y
      
      base_AR[doublon[2],"X.R"] <- B2x
      base_AR[doublon[2],"Y.R"] <- B2y
      base_AR[doublon[2],"X.T"] <- A2x
      base_AR[doublon[2],"Y.T"] <- A2y
    }
    
    base <- rbind(base_0,base_AR)
    
    temp <- base[,c(var_depart_k,var_arrivee_k,var_flux_k)]
    rownames(temp) <- as.character(1:dim(temp)[1])
    
    l=list()
    for (i in 1:dim(base_AR)[1]) 
    {
      vec <- (matrix(as.numeric(base[i,c("X.R","X.T","Y.R","Y.T")]),2,2,F))
      l[[i]] <- st_linestring(vec)
    }
    suppressWarnings(resultat <- st_sf(cbind(temp,geometry=st_sfc(l)), crs=st_crs(fond_carto_k)))
    
    return(list(analyse=resultat))
  }


set_style_oursins <-
  function(map,epaisseur=2,colTrait="black")
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(any(class(epaisseur)!="numeric")) msg_error2 <- "L'epaisseur du trait doit etre de type numerique / "
    if(any(class(colTrait)!="character")) msg_error3 <- "La couleur du trait doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    idx_fleche <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolylines")
      {
        idx_fleche <- i
      }
    }
    
    map$x$calls[[idx_fleche]]$args[4][[1]]$weight <- epaisseur
    map$x$calls[[idx_fleche]]$args[4][[1]]$color <- colTrait
    
    return(map)
  }


extract_fond_leaflet_oursins <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_fleche <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_oursins") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addPolylines")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_oursins") idx_fleche <- i
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
    }
    
    var_flux <- map$x$calls[[idx_fleche]]$args[[3]]$var_flux
    
    code_epsg <- map$x$calls[[idx_fleche]]$args[[3]]$code_epsg
    dom <- map$x$calls[[idx_fleche]]$args[[3]]$dom
    
    list_fonds <- list()
    nom_fonds <- c()
    l <- 1
    for(i in 1:length(idx_carte))
    {
      aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
      
      bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
      for(j in 1:length(aa))
      {
        bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
      }
      bb <- bb[-1,]
      
      fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
      rm(aa,bb)
      
      fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
      
      list_fonds[[l]] <- fond
      
      nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
      
      l <- l+1
    }
    
    if(!is.null(idx_fleche))
    {
      aa <- lapply(1:length(map$x$calls[[idx_fleche]]$args[[1]]), function(x) st_linestring(as.matrix(map$x$calls[[idx_fleche]]$args[[1]][[x]][[1]][[1]])))
      
      bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
      for(i in 1:length(aa))
      {
        bb <- rbind(bb,st_sf(geometry=st_sfc(st_linestring(aa[[i]]),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
      }
      bb <- bb[-1,]
      
      if(any(substring(map$x$calls[[idx_fleche]]$args[[5]],1,3) %in% "<b>"))
      {
        cc <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],":")[[1]][1]+2,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][4]-1))
        cc <- as.numeric(str_replace_all(str_replace_all(cc,",",".")," ",""))
        fond <- cbind(var=cc,bb)
        
        dd <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]+5,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][3]-1))
        fond <- cbind(CODE2=dd,fond)
        ee <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],">")[[1]][2]+1,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]-1))
        fond <- cbind(CODE1=ee,fond)
        
        names(fond) <- c("CODE1","CODE2",var_flux,"geometry")
        rm(aa,bb,cc,dd,ee)
      }
      
      fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
      
      list_fonds[[l]] <- fond
      
      nom_fonds <- c(nom_fonds,map$x$calls[[idx_fleche]]$args[[3]]$nom_fond)
      
      l <- l+1
    }
    
    if(!is.null(idx_titre))
    {
      titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
    }else
    {
      titre <- ""
    }
    
    if(!is.null(idx_source))
    {
      source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
    }else
    {
      source <- ""
    }
    
    epaisseur <- map$x$calls[[idx_fleche]]$args[4][[1]]$weight
    colTrait <- map$x$calls[[idx_fleche]]$args[4][[1]]$color
    
    return(list(list_fonds,nom_fonds,titre,source,epaisseur,colTrait,dom))
  }


export_qgis_oursins <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_oursins(map)
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_flux")) l <- c(l,"fond_flux")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    epaisseur <- list_fonds[[5]]
    colTrait <- list_fonds[[6]]
    
    export_projet_qgis_oursins(l,rep_sortie,sortie,titre1,titre2,source,epaisseur,colTrait,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_oursins <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,epaisseur,colTrait,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if(nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))
      }else
      {
        idcouche=l[i]
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if(str_sub(l[i][length(l[i])],start=-5)!="carte")
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      
      BLOCCATEGORIES=data.frame()
      
      attr=""
      name="0"
      typeanalyse="singleSymbol"
      couleurfond="255,255,255"
      
      remplissagefond="no"
      
      if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
      {
        couleurbordure="128,128,128"
      }else if (l[i]=="fond_territoire")
      {
        couleurbordure="191,191,191"
      }else
      {
        couleurbordure="0,0,0"
      }
      
      stylebordure="solid"
      if (l[i] %in% c("fond_flux","fond_maille","fond_departement","fond_pays","fond_territoire"))
      {
        epaisseurbordure=0.26
      }else
      {
        epaisseurbordure=0.5
      }
      
      if (l[i]=="fond_flux")
      {
        epaisseurbordure=epaisseur
        couleurbordure=colTrait
      }
      
      analyse_ou <- read_sf(chemincouche)
      projcouche <- st_crs(analyse_ou)$proj4string
      geometrie <- attr(analyse_ou$geometry[[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_ou[[1]][[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_ou[[1]],"class")[2]
      
      if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
      {
        BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
      }else
      {
        if (geometrie %in% c("LINESTRING","MULTILINESTRING"))
        {
          BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
        }
      }
      
      blocvector=modif_blocvectorClassification(nomcouche)
      blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
      bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
      BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
      
      toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
      toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
      BLOCPROJECT=rbind(BLOCPROJECT,toto)
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }


#############
### FJ/FS ###
#############

largeur_fleche <-
  function(map)
  {
    msg_error1 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1)))
    {
      stop(simpleError(msg_error1))
    }
    
    idx_fleche <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux") idx_fleche <- i
      }
    }
    
    if(!is.null(idx_fleche))
    {
      largeur <- map$x$calls[[idx_fleche]]$args[[3]]$largeur
    }else
    {
      stop(simpleError("Il n'y a pas d'analyse en fleches joignantes ou saphirs dans la map"))
    }
    
    return(largeur)
  }


##########
### FJ ###
##########

k_joignantes <-
  function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,var_depart_k,var_arrivee_k,var_flux_k,largeur_k,decalage_aller_retour_k,decalage_centroid_k)
  {
    donnees_k[,var_flux_k] <- as.numeric(donnees_k[,var_flux_k])
    
    donnees_k <- donnees_k[!is.na(donnees_k[,var_flux_k]),]
    if(is.null(largeur_k)) largeur_k <- 0
    
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% c(donnees_k[,var_depart_k],donnees_k[,var_arrivee_k]),]
    suppressWarnings(centroid <- st_centroid(fond_carto_k))
    suppressWarnings(coord <- data.frame(CODGEO=as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],X=st_coordinates(st_centroid(fond_carto_k))[,1],Y=st_coordinates(st_centroid(fond_carto_k))[,2]))
    names(coord) <- c("CODGEO","X","Y")
    
    donnees_k <- donnees_k[donnees_k[,var_depart_k] %in% coord$CODGEO,]
    donnees_k <- donnees_k[donnees_k[,var_arrivee_k] %in% coord$CODGEO,]
    donnees_k <- donnees_k[donnees_k[,var_depart_k]!=donnees_k[,var_arrivee_k],]
    
    donnees_k$AR <- 0
    nb_ar <- 1
    for(i in 1:nrow(donnees_k))
    {
      if(donnees_k[i,"AR"]==0)
      {
        doublon <- which(donnees_k[i,"CODE1"]==donnees_k$CODE2 & donnees_k[i,"CODE2"]==donnees_k$CODE1)
        if(length(doublon)>0)
        {
          donnees_k[i,"AR"] <- nb_ar
          donnees_k[doublon,"AR"] <- nb_ar
          nb_ar <- nb_ar + 1
        }
      }
    }
    
    base <- merge(merge(donnees_k,coord,by.x=var_depart_k,by.y="CODGEO"),coord,by.x=var_arrivee_k,by.y="CODGEO",suffixes = c(".R",".T"))
    
    # ETAPE 1 : decalage des lignes du centroid
    
    for(i in 1:nrow(base))
    {
      # Coordonnees des points A et B
      Ax <- base[i,"X.R"]
      Ay <- base[i,"Y.R"]
      Bx <- base[i,"X.T"]
      By <- base[i,"Y.T"]
      
      if(decalage_centroid_k>0)
      {
        rapport <- (((Ax-Bx)^2+(Ay-By)^2)^(1/2))/(decalage_centroid_k*1000)
        
        #(Ax-Bx)/(Ax-Cx)=rapport
        Cx <- Ax-((Ax-Bx)/rapport)
        Cy <- Ay-((Ay-By)/rapport)
        #(Bx-Ax)/(Bx-Dx)=rapport
        Dx <- Bx-((Bx-Ax)/rapport)
        Dy <- By-((By-Ay)/rapport)
      }else
      {
        Cx <- Ax
        Cy <- Ay
        Dx <- Bx
        Dy <- By
      }
      
      base[i,"X.R"] <- Cx
      base[i,"Y.R"] <- Cy
      base[i,"X.T"] <- Dx
      base[i,"Y.T"] <- Dy
    }
    
    # ETAPE 2 : decalage des lignes aller et retour
    
    base_0 <- base[base$AR==0,] # sans aller-retour
    base_AR <- base[base$AR!=0,] # avec aller-retour
    
    l <- list()
    for(i in 1:(nrow(base_AR)/2))
    {
      doublon <- which(base_AR[,"AR"]==i)
      
      decalage1 <- base_AR[doublon[1],var_flux_k]*(largeur_k/2)/max(base[,var_flux_k]) + (decalage_aller_retour_k/2)*1000
      decalage2 <- base_AR[doublon[2],var_flux_k]*(largeur_k/2)/max(base[,var_flux_k]) + (decalage_aller_retour_k/2)*1000
      
      # Coordonnees des points A et B
      Ax <- base_AR[doublon[1],"X.R"]
      Ay <- base_AR[doublon[1],"Y.R"]
      Bx <- base_AR[doublon[1],"X.T"]
      By <- base_AR[doublon[1],"Y.T"]
      
      if(Ax==Bx) # La ligne est verticale
      {
        A1x <- Ax
        A1y <- Ay+decalage1
        A2x <- Ax
        A2y <- Ay-decalage1
        B1x <- Bx
        B1y <- By+decalage2
        B2x <- Bx
        B2y <- By-decalage2
      }else if(Ay==By) # La ligne est horizontale
      {
        A1x <- Ax+decalage1
        A1y <- Ay
        A2x <- Ax+decalage1
        A2y <- Ay
        B1x <- Bx+decalage2
        B1y <- By
        B2x <- Bx+decalage2
        B2y <- By
      }else
      {
        # On applique le produit scalaire pour calculer les nouvelles coordonnees des points A1, B1 et A2, B2
        
        # On fixe le vecteur S1 pour calculer un coeff multiplicateur M
        S1 <- 1 # Sx-Ax
        # S2 = Sy-Ay
        
        #(Sx-Ax)*(Ax-Bx)+S2*(Ay-By)=0 : produit scalaire
        #S1*(Ax-Bx)+S2*(Ay-By)=0
        S2 <- -S1*(Ax-Bx)/(Ay-By)
        N <- (1+S2^2)^(1/2)
        M1 <- decalage1/N
        M2 <- decalage2/N
        # (M^2+(M*S2)^2)^(1/2) = decalage : verif
        
        A1x <- Ax+M1
        A1y <- Ay+M1*S2
        A2x <- Ax-M2
        A2y <- Ay-M2*S2
        B1x <- Bx+M1
        B1y <- By+M1*S2
        B2x <- Bx-M2
        B2y <- By-M2*S2
      }
      
      # Nouvelles coordonnees des points de A vers B puis de B vers A
      
      base_AR[doublon[1],"X.R"] <- A1x
      base_AR[doublon[1],"Y.R"] <- A1y
      base_AR[doublon[1],"X.T"] <- B1x
      base_AR[doublon[1],"Y.T"] <- B1y
      
      base_AR[doublon[2],"X.R"] <- B2x
      base_AR[doublon[2],"Y.R"] <- B2y
      base_AR[doublon[2],"X.T"] <- A2x
      base_AR[doublon[2],"Y.T"] <- A2y
    }
    
    base <- rbind(base_0,base_AR)
    base <- base[order(base[,var_flux_k],decreasing=T),]
    base <- base[base[,var_flux_k]>0,]
    
    # Construction des fleches joignantes
    
    l <- list()
    for(i in 1:nrow(base))
    {
      RTx <- base[i,"X.T"]-base[i,"X.R"]
      RTy <- base[i,"Y.T"]-base[i,"Y.R"]
      
      # Etape 1 : on considere T a l'origine et R sur l'absisse
      
      Tx <- 0
      Ty <- 0
      
      RT <- (RTx^2+RTy^2)^0.5
      AR <- base[i,var_flux_k]*(largeur_k/2)/max(base[,var_flux_k])
      Rx <- RT
      Ry <- 0
      
      Ax <- RT
      Ay <- AR
      
      # Etape 2 : on applique la rotation d'angle A autour de T
      
      # angle de rotation en radian
      theta <- acos(abs(RTx)/RT)
      
      # 4 cas possibles selon le cadran du cercle trigo dans lequel on tombe
      if(RTx<0 & RTy<0) {theta <- theta}
      if(RTx>0 & RTy<0) {theta <- pi-theta}
      if(RTx>0 & RTy>0) {theta <- pi+theta}
      if(RTx<0 & RTy>0) {theta <- 2*pi-theta}
      # theta_degre_pour_verif<-theta*180/pi
      
      R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Rx,Ry))
      A <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Ax,Ay))
      
      # Etape 3 : on decale le triangle de Tx et Ty
      
      Tx <- base[i,"X.T"]
      Ty <- base[i,"Y.T"]
      
      A <- A + c(Tx,Ty)
      Ax <- A[1]
      Ay <- A[2]
      
      R <- R + c(Tx,Ty)
      Rx <- R[1]
      Ry <- R[2]
      
      # Etape 4 : construction de la fleche
      
      Bx <- RT/5 #la cote de la fleche est de 4/5 de la longueur totale
      By <- AR
      B <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Bx,By))
      B <- B + c(Tx,Ty)
      Bx <- B[1]
      By <- B[2]
      
      Dx <- RT/5
      Dy <- -AR
      D <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Dx,Dy))
      D <- D + c(Tx,Ty)
      Dx <- D[1]
      Dy <- D[2]
      
      Ex <- RT
      Ey <- -AR
      E <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Ex,Ey))
      E <- E + c(Tx,Ty)
      Ex <- E[1]
      Ey <- E[2]
      
      fleche<-matrix(c(Rx,Ax,Bx,Tx,Dx,Ex,Rx,Ry,Ay,By,Ty,Dy,Ey,Ry),nrow=7)
      l[[i]] <- st_polygon(list(fleche))
    }
    
    fleche <- st_sf(cbind(base[,c(var_depart_k,var_arrivee_k,var_flux_k)],geometry=st_sfc(l)), crs=st_crs(fond_carto_k))
    
    return(list(analyse=fleche))
  }


add_legende_joignantes <-
  function(map,titre=NULL,lng=NULL,lat=NULL,dom="0",precision=0,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error4 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    idx_carte <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_joignantes"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_joignantes") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[3][[1]]$nom_couche=="legende_joignantes") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende)) # la legende existe
        {
          if(map$x$calls[[i]]$method %in% "addMarkers")
          {
            if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_joignantes") idx_legende <- c(idx_legende,i)
          }
        }
      }
    }
    idx_fleche <- idx_carte[length(idx_carte)]
    idx_carte <- idx_carte[-length(idx_carte)]
    
    code_epsg <- map$x$calls[[idx_fleche]]$args[[3]]$code_epsg
    var_flux <- map$x$calls[[idx_fleche]]$args[[3]]$var_flux
    
    lng_init <- lng
    lat_init <- lat
    if(!is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }else
    {
      if(!is.null(idx_legende)) map$x$calls <- map$x$calls[-idx_legende] # Si la legende existe, on la supprime pour la recreer
      
      coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
      
      lng_init <- lng
      lat_init <- lat
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map$x$fitBounds[[4]]
        lat <- map$x$fitBounds[[3]]-coeff*8
      }
      
      vmax <- map$x$calls[[idx_fleche]]$args[[3]]$max_var
      
      coord_fleche_max <- data.frame(lng=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lng,lat=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lat)
      
      large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[2,1],coord_fleche_max[2,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
      long <- coeff*2
      
      flux_leg <- flux_legende_joignantes(lng,lat,long,large)
      flux_legWGS84 <- flux_leg[[1]]
      flux_legWGS84 <- cbind(flux_legWGS84,VALEUR=c(vmax,vmax/3))
      pointe1 <- flux_leg[[2]]
      pointe2 <- flux_leg[[3]]
      
      # leaflet du cadre blanc en 1er
      map <- addRectangles(map = map,
                           lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                           lng2 = st_bbox(flux_legWGS84)[3]+coeff*3, lat2 = st_bbox(flux_legWGS84)[4]+coeff*1.2,
                           stroke = FALSE,
                           options = pathOptions(clickable = F),
                           fill = T,
                           fillColor = "white",
                           fillOpacity = 0.8,
                           group=list(nom_couche="legende_joignantes")
      )
      
      map <- addPolygons(map = map,
                         data=flux_legWGS84,
                         stroke = TRUE,
                         opacity = 1,
                         color = "#303030",
                         weight = 1,
                         options = pathOptions(clickable = F),
                         fill = T,
                         fillColor = "#CD853F",
                         fillOpacity = 1,
                         group=list(nom_couche="legende_joignantes",nom_fond="fond_flux_leg")
      )
      
      # leaflet valeur flux
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe1[1], lat = pointe1[2], #grande fleche
                                 label = as.character(format(round(vmax,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group=list(nom_couche="legende_joignantes")
      )
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe2[1], lat = pointe2[2], #petite fleche
                                 label = as.character(format(round(vmax/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group=list(nom_couche="legende_joignantes")
      )
      
      #leaflet titre 1
      map <- addLabelOnlyMarkers(map = map,
                                 lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff/2,
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group=list(nom_couche="legende_joignantes")
      )
      
      
    }
    
    message(simpleMessage(paste0("Les coordonnees de la legende des fleches joignantes sont : longitude (x) = ",lng," degre ; latitude (y) = ",lat," degre")))
    
    return(map)
  }


set_couleur_joignantes <-
  function(map,colFleche="#CD853F",colBorder="black")
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(colFleche)) if(any(class(colFleche)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error3 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }
    
    idx_fleche <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux") idx_fleche <- i
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux_leg") idx_legende <- i
      }
    }
    
    if(!is.null(idx_fleche))
    {
      map$x$calls[[idx_fleche]]$args[[4]]$fillColor <- colFleche
      map$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder
    }
    
    if(!is.null(idx_legende))
    {
      map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colFleche
      map$x$calls[[idx_legende]]$args[[4]]$color <- colBorder
    }
    
    return(map)
  }


flux_legende_joignantes <-
  function(x,y,long,large)
  {
    l <- list()
    
    xp <- x
    yp <- y
    xg <- x
    yg <- y+large/1.5
    
    vec <- matrix(c(xg,yg, xg+long,yg, xg+long+large*0.655,yg+large*0.655/2, xg+long,yg+large*0.655,   xg,yg+large*0.655,   xg,yg),6,2,byrow=T)
    l[[1]] <- st_polygon(list(vec))
    vec <- matrix(c(xp,yp, xp+long,yp, xp+long+large*0.655,yp+large*0.655/6, xp+long,yp+large*0.655/3, xp,yp+large*0.655/3, xp,yp),6,2,byrow=T)
    l[[2]] <- st_polygon(list(vec))
    
    flux_legWGS84 <- st_sf(geometry=st_sfc(l), crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    pointe1 <- c(xg+long+large*0.655,yg+large*0.655/2)
    pointe2 <- c(xp+long+large*0.655,yp+large*0.655/6)
    
    return(list(flux_legWGS84,pointe1,pointe2))
  }


flux_legende_joignantes_pl <-
  function(x,y,long_pl,large_pl,code_epsg)
  {
    l_pl <- list()
    
    xp <- x
    yp <- y
    
    xpyp_pl <- st_sf(geometry=st_sfc(st_point(c(xp,yp))), crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
    xpyp_pl <- st_transform(xpyp_pl,paste0("+init=epsg:",code_epsg))
    
    xp_pl <- st_coordinates(xpyp_pl)[1]
    yp_pl <- st_coordinates(xpyp_pl)[2]
    xg_pl <- xp_pl
    yg_pl <- yp_pl+large_pl
    
    vec <- matrix(c(xg_pl,yg_pl, xg_pl+long_pl,yg_pl, xg_pl+long_pl+large_pl/2,yg_pl+large_pl/2, xg_pl+long_pl,yg_pl+large_pl,   xg_pl,yg_pl+large_pl,   xg_pl,yg_pl),6,2,byrow=T)
    l_pl[[1]] <- st_polygon(list(vec))
    vec <- matrix(c(xp_pl,yp_pl, xp_pl+long_pl,yp_pl, xp_pl+long_pl+large_pl/2,yp_pl+large_pl/6, xp_pl+long_pl,yp_pl+large_pl/3, xp_pl,yp_pl+large_pl/3, xp_pl,yp_pl),6,2,byrow=T)
    l_pl[[2]] <- st_polygon(list(vec))
    
    flux_leg_pl <- st_sf(geometry=st_sfc(l_pl), crs=paste0("+init=epsg:",code_epsg))
    
    return(flux_leg_pl)
  }


extract_fond_leaflet_joignantes <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_fleche <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_joignantes"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("legende_joignantes"))) idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(any(map$x$calls[[i]]$args[5][[1]]$nom_couche %in% c("legende_joignantes"))) idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      idx_fleche <- idx_carte[length(idx_carte)]
      idx_carte <- idx_carte[-length(idx_carte)]
      
      var_flux <- map$x$calls[[idx_fleche]]$args[[3]]$var_flux
      
      code_epsg <- map$x$calls[[idx_fleche]]$args[[3]]$code_epsg
      dom <- map$x$calls[[idx_fleche]]$args[[3]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_fleche))
      {
        aa <- lapply(1:length(map$x$calls[[idx_fleche]]$args[[1]]), function(x) st_polygon(list(as.matrix(map$x$calls[[idx_fleche]]$args[[1]][[x]][[1]][[1]]))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(i in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(list(aa[[i]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_fleche]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],":")[[1]][1]+2,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][4]-1))
          cc <- as.numeric(str_replace_all(str_replace_all(cc,",",".")," ",""))
          fond <- cbind(var=cc,bb)
          
          dd <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]+5,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(CODE2=dd,fond)
          ee <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],">")[[1]][2]+1,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]-1))
          fond <- cbind(CODE1=ee,fond)
          
          names(fond) <- c("CODE1","CODE2",var_flux,"geometry")
          rm(aa,bb,cc,dd,ee)
        }
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_fleche]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }
      
      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }
      
      if(!is.null(idx_legende))
      {
        large <- map$x$calls[[idx_fleche[1]]]$args[[3]]$distance
        long <- large*2
        
        gf <- st_sf(geometry=st_sfc(st_polygon(list(as.matrix(map$x$calls[[idx_legende[1]]]$args[[1]][[1]][[1]][[1]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        x <- st_bbox(gf)$xmin
        y <- st_bbox(gf)$ymin
        flux_leg_pl <- flux_legende_joignantes_pl(x,y,long,large,code_epsg)
        
        max_var <- map$x$calls[[idx_fleche]]$args[[3]]$max_var
        flux_leg_pl <- cbind(VAR=c(max_var,max_var/3),flux_leg_pl)
        names(flux_leg_pl) <- c(var_flux,"geometry")
        
        list_fonds[[l]] <- flux_leg_pl
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende[[1]]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      colFleche <- map$x$calls[[idx_fleche]]$args[4][[1]]$fillColor
      colBorder <- map$x$calls[[idx_fleche]]$args[4][[1]]$color
      
      return(list(list_fonds,nom_fonds,titre,source,colFleche,colBorder,dom))
    }
  }


export_qgis_joignantes <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_joignantes(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des fleches joignantes n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_joignantes(map) pour ajouter une legende de fleches a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_flux")) l <- c(l,"fond_flux")
    if(any(list_fonds[[2]] %in% "fond_flux_leg")) l <- c(l,"fond_flux_leg")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    colFleche <- list_fonds[[5]]
    colBorder <- list_fonds[[6]]
    
    export_projet_qgis_fleches_joignantes(l,rep_sortie,sortie,titre1,titre2,source,colFleche,colBorder,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_fleches_joignantes <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,colFleche,colBorder,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if(nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))
      }else
      {
        idcouche=l[i]
      }
      
      toto=modif_blocleg(l[i],idcouche)  
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if(str_sub(l[i][length(l[i])],start=-5)!="carte")
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      BLOCCATEGORIES=data.frame()      
      
      attr=""
      name="0"
      typeanalyse="singleSymbol"
      couleurfond="255,255,255"
      
      if (l[i] %in% c("fond_flux","fond_flux_leg"))
      {
        couleurfond=colFleche
        remplissagefond="yes"
      }else if(l[i]=="fond_flux_rupt")
      {
        couleurfond="100,149,237"
        remplissagefond="yes"
      }else
      {
        remplissagefond="no"
      }
      
      if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
      {
        couleurbordure="128,128,128"
      }else if (l[i]=="fond_territoire")
      {
        couleurbordure="191,191,191"
      }else if (l[i] %in% c("fond_flux","fond_flux_leg"))
      {
        couleurbordure=colBorder
      }else
      {
        couleurbordure="0,0,0"
      }
      
      stylebordure="solid"
      if (l[i] %in% c("fond_flux","fond_flux_leg","fond_flux_rupt","fond_maille","fond_departement","fond_pays","fond_territoire"))
      {
        epaisseurbordure=0.26
      }else
      {
        epaisseurbordure=0.5
      }
      
      analyse_fj_fs <- read_sf(chemincouche)
      projcouche=st_crs(analyse_fj_fs)$proj4string
      geometrie=attr(analyse_fj_fs$geometry[[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_fj_fs[[1]][[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_fj_fs[[1]],"class")[2]
      
      if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
      {
        BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
      }else
      {
        if (geometrie %in% c("LINESTRING","MULTILINESTRING"))
        {
          BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
        }
      }
      
      blocvector=modif_blocvectorClassification(nomcouche)
      blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
      bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
      BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
      
      toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
      toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
      BLOCPROJECT=rbind(BLOCPROJECT,toto)
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }


##########
### FS ###
##########

k_saphir <-
  function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,var_depart_k,var_arrivee_k,var_flux_k,largeur_k,longueur_k,type)
  {
    if(is.null(type)) type <- "Ent"
    
    donnees_k[,var_flux_k] <- as.numeric(donnees_k[,var_flux_k])
    suppressWarnings(donnees_k <- donnees_k[!is.na(donnees_k[,var_flux_k]),])
    
    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% c(donnees_k[,var_depart_k],donnees_k[,var_arrivee_k]),]
    suppressWarnings(centroid <- st_centroid(fond_carto_k))
    suppressWarnings(coord <- data.frame(CODGEO=as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],X=st_coordinates(st_centroid(fond_carto_k))[,1],Y=st_coordinates(st_centroid(fond_carto_k))[,2]))
    names(coord) <- c("CODGEO","X","Y")
    
    donnees_k <- donnees_k[donnees_k[,var_depart_k] %in% coord$CODGEO,]
    donnees_k <- donnees_k[donnees_k[,var_arrivee_k] %in% coord$CODGEO,]
    donnees_k <- donnees_k[donnees_k[,var_depart_k]!=donnees_k[,var_arrivee_k],]
    
    base <- merge(merge(donnees_k,coord,by.x=var_depart_k,by.y="CODGEO"),coord,by.x=var_arrivee_k,by.y="CODGEO",suffixes = c(".Z",".T"))
    base <- base[order(abs(base[,var_flux_k]),decreasing=T),]
    
    if (type=="Sor")
    {
      unix <- (base$X.Z-base$X.T)/((base$X.Z-base$X.T)^2+(base$Y.Z-base$Y.T)^2)^0.5
      uniy <- (base$Y.Z-base$Y.T)/((base$X.Z-base$X.T)^2+(base$Y.Z-base$Y.T)^2)^0.5
      
      base$X.R <- base$X.T+longueur_k*unix
      base$Y.R <- base$Y.T+longueur_k*uniy
      
      base$typeFlux <- -1
      
      base <- base[,c(var_depart_k,var_arrivee_k,var_flux_k,"X.R","Y.R","X.T","Y.T","typeFlux")]
    }
    if (type=="Ent")
    {
      base$X.R <- base$X.Z
      base$Y.R <- base$Y.Z
      
      unix <- (base$X.T-base$X.R)/((base$X.T-base$X.R)^2+(base$Y.T-base$Y.R)^2)^0.5
      uniy <- (base$Y.T-base$Y.R)/((base$X.T-base$X.R)^2+(base$Y.T-base$Y.R)^2)^0.5
      
      base$X.T <- base$X.R+longueur_k*unix
      base$Y.T <- base$Y.R+longueur_k*uniy
      
      base$typeFlux <- 1
      
      base <- base[,c(var_depart_k,var_arrivee_k,var_flux_k,"X.R","Y.R","X.T","Y.T","typeFlux")]
    }
    
    if (type=="Sol")
    {
      temp1 <- base[base[,var_flux_k]>0,]
      
      temp1$X.R <- temp1$X.Z
      temp1$Y.R <- temp1$Y.Z
      
      unix <- (temp1$X.T-temp1$X.R)/((temp1$X.T-temp1$X.R)^2+(temp1$Y.T-temp1$Y.R)^2)^0.5
      uniy <- (temp1$Y.T-temp1$Y.R)/((temp1$X.T-temp1$X.R)^2+(temp1$Y.T-temp1$Y.R)^2)^0.5
      
      temp1$X.T <- temp1$X.R+longueur_k*unix
      temp1$Y.T <- temp1$Y.R+longueur_k*uniy
      
      if(nrow(temp1)>0)
      {
        temp1$typeFlux <- 1
        temp1 <- temp1[,c(var_depart_k,var_arrivee_k,var_flux_k,"X.R","Y.R","X.T","Y.T","typeFlux")]
      }
      
      temp2 <- base[base[,var_flux_k]<0,]
      
      temp2$toto1 <- temp2$X.Z
      temp2$toto2 <- temp2$Y.Z
      temp2$X.Z <- temp2$X.T
      temp2$Y.Z <- temp2$Y.T
      temp2$X.T <- temp2$toto1
      temp2$Y.T <- temp2$toto2
      
      unix <- (temp2$X.Z-temp2$X.T)/((temp2$X.Z-temp2$X.T)^2+(temp2$Y.Z-temp2$Y.T)^2)^0.5
      uniy <- (temp2$Y.Z-temp2$Y.T)/((temp2$X.Z-temp2$X.T)^2+(temp2$Y.Z-temp2$Y.T)^2)^0.5
      
      temp2$X.R <- temp2$X.T+longueur_k*unix
      temp2$Y.R <- temp2$Y.T+longueur_k*uniy
      
      if(nrow(temp2)>0)
      {
        temp2$typeFlux <- -1
        temp2 <- temp2[,c(var_depart_k,var_arrivee_k,var_flux_k,"X.R","Y.R","X.T","Y.T","typeFlux")]
      }
      
      temp2 <- temp2[rev(order(temp2[,var_flux_k])),]
      
      base <- rbind(temp1,temp2)
    }  
    
    l <- list()
    for(i in 1:nrow(base))
    {
      RTx <- base[i,"X.T"]-base[i,"X.R"]
      RTy <- base[i,"Y.T"]-base[i,"Y.R"]
      
      # Etape 1 : on considere T a l'origine et R sur l'absisse
      
      Tx <- 0
      Ty <- 0
      
      RT <- (RTx^2+RTy^2)^0.5
      AR <- base[i,var_flux_k]*(largeur_k/2)/max(base[,var_flux_k])
      Rx <- RT
      Ry <- 0
      
      Ax <- RT
      Ay <- AR
      
      # Etape 2 : on applique la rotation d'angle A autour de T
      
      # angle de rotation en radian
      theta <- acos(abs(RTx)/RT)
      
      # 4 cas possibles selon le cadran du cercle trigo dans lequel on tombe
      if(RTx<0 & RTy<0) {theta <- theta}
      if(RTx>0 & RTy<0) {theta <- pi-theta}
      if(RTx>0 & RTy>0) {theta <- pi+theta}
      if(RTx<0 & RTy>0) {theta <- 2*pi-theta}
      # theta_degre_pour_verif<-theta*180/pi
      
      R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Rx,Ry))
      A <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Ax,Ay))
      
      # Etape 3 : on decale le triangle de Tx et Ty
      
      Tx <- base[i,"X.T"]
      Ty <- base[i,"Y.T"]
      
      A <- A + c(Tx,Ty)
      Ax <- A[1]
      Ay <- A[2]
      
      R <- R + c(Tx,Ty)
      Rx <- R[1]
      Ry <- R[2]
      
      # Etape 4 : construction de la fleche
      
      Bx <- RT/5 #le cote de la fleche est de 4/5 de la longueur totale
      By <- AR
      B <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Bx,By))
      B <- B + c(Tx,Ty)
      Bx <- B[1]
      By <- B[2]
      
      Dx <- RT/5
      Dy <- -AR
      D <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Dx,Dy))
      D <- D + c(Tx,Ty)
      Dx <- D[1]
      Dy <- D[2]
      
      Ex <- RT
      Ey <- -AR
      E <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2) %*% matrix(c(Ex,Ey))
      E <- E + c(Tx,Ty)
      Ex <- E[1]
      Ey <- E[2]
      
      fleche<-matrix(c(Rx,Ax,Bx,Tx,Dx,Ex,Rx,Ry,Ay,By,Ty,Dy,Ey,Ry),nrow=7)
      l[[i]] <- st_polygon(list(fleche))
    }
    
    fleche <- st_sf(cbind(base[,c(var_depart_k,var_arrivee_k,var_flux_k)],geometry=st_sfc(l)), crs=st_crs(fond_carto_k))
    
    return(list(analyse=fleche))
  }


add_legende_saphirs <-
  function(map,titre=NULL,lng=NULL,lat=NULL,dom="0",precision=0,zoom=8)
  {
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(lng)) if(any(class(lng)!="numeric")) msg_error2 <- "La longitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!is.null(lat)) if(any(class(lat)!="numeric")) msg_error3 <- "La latitude doit etre de type numerique (en coordonnees WGS84) / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error4 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    if(is.null(titre)) titre <- " "
    titre<-iconv(titre,"latin1","utf8")
    
    idx_carte <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_saphirs"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addRectangles")
      {
        if(map$x$calls[[i]]$args[[6]]$nom_couche=="legende_saphirs") idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende))
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[3][[1]]$nom_couche=="legende_saphirs") idx_legende <- c(idx_legende,i)
        }
        if(!is.null(idx_legende)) # la legende existe
        {
          if(map$x$calls[[i]]$method %in% "addMarkers")
          {
            if(map$x$calls[[i]]$args[5][[1]]$nom_couche=="legende_saphirs") idx_legende <- c(idx_legende,i)
          }
        }
      }
    }
    idx_fleche <- idx_carte[length(idx_carte)]
    idx_carte <- idx_carte[-length(idx_carte)]
    
    code_epsg <- map$x$calls[[idx_fleche]]$args[[3]]$code_epsg
    var_flux <- map$x$calls[[idx_fleche]]$args[[3]]$var_flux
    
    lng_init <- lng
    lat_init <- lat
    if(!is.null(idx_legende) & (is.null(lng_init) | is.null(lat_init)))# l'utilisateur veut juste supprimer la legende existante
    {
      map$x$calls <- map$x$calls[-idx_legende]
    }else
    {
      if(!is.null(idx_legende)) map$x$calls <- map$x$calls[-idx_legende] # Si la legende existe, on la supprime pour la recreer
      
      coeff <- ((360/(2^zoom))/7.2) # Permet de fixer une distance sur l'ecran. Il s'agit en gros d'une conversion des degres en pixels. Reste constant a longitude egale mais varie un peu selon la latitude
      
      if(is.null(lng_init) | is.null(lat_init))
      {
        lng <- map$x$fitBounds[[4]]
        lat <- map$x$fitBounds[[3]]-coeff*8
      }
      
      vmax <- map$x$calls[[idx_fleche]]$args[[3]]$max_var
      
      coord_fleche_max <- data.frame(lng=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lng,lat=map$x$calls[[idx_fleche]]$args[[1]][[1]][[1]][[1]]$lat)
      
      large <- max(st_distance(st_sfc(st_point(c(coord_fleche_max[5,1],coord_fleche_max[5,2])),st_point(c(coord_fleche_max[6,1],coord_fleche_max[6,2])))))
      long <- coeff*3
      
      flux_leg <- flux_legende_saphirs(lng,lat,long,large)
      flux_legWGS84 <- flux_leg[[1]]
      flux_legWGS84 <- cbind(flux_legWGS84,VALEUR=c(vmax,vmax/3))
      pointe1 <- flux_leg[[2]]
      pointe2 <- flux_leg[[3]]
      
      # leaflet du cadre blanc en 1er
      map <- addRectangles(map = map,
                           lng1 = st_bbox(flux_legWGS84)[1]-coeff/2, lat1 = st_bbox(flux_legWGS84)[2]-coeff/2,
                           lng2 = st_bbox(flux_legWGS84)[3]+coeff*8, lat2 = st_bbox(flux_legWGS84)[4]+coeff*4,
                           stroke = FALSE,
                           options = pathOptions(clickable = F),
                           fill = T,
                           fillColor = "white",
                           fillOpacity = 0.8,
                           group=list(nom_couche="legende_saphirs")
      )
      
      map <- addPolygons(map = map,
                         data=flux_legWGS84,
                         stroke = TRUE,
                         opacity = 1,
                         color = "#303030",
                         weight = 1,
                         options = pathOptions(clickable = F),
                         fill = T,
                         fillColor = "#CD853F",
                         fillOpacity = 1,
                         group=list(nom_couche="legende_saphirs",nom_fond="fond_flux_leg")
      )
      
      # leaflet valeur flux
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe1[1], lat = pointe1[2], #grande fleche
                                 label = as.character(format(round(vmax,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group=list(nom_couche="legende_saphirs")
      )
      
      map <- addLabelOnlyMarkers(map = map,
                                 lng = pointe2[1], lat = pointe2[2], #petite fleche
                                 label = as.character(format(round(vmax/3,precision),big.mark=" ",decimal.mark=",",nsmall=0)),
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "12px"
                                                             )),
                                 group=list(nom_couche="legende_saphirs")
      )
      
      #leaflet titre 1
      map <- addLabelOnlyMarkers(map = map,
                                 lng = st_bbox(flux_legWGS84)[1]-coeff/3, lat = st_bbox(flux_legWGS84)[4]+coeff*2,
                                 label = titre,
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "right",
                                                             style = list(
                                                               "color" = "black",
                                                               "font-size" = "14px"
                                                             )),
                                 group=list(nom_couche="legende_saphirs")
      )
    }
    
    message(simpleMessage(paste0("Les coordonnees de la legende des fleches saphirs sont : longitude (x) = ",lng," degre ; latitude (y) = ",lat," degre")))
    
    return(map)
  }


set_couleur_saphirs <-
  function(map,colEntree="#CD853F",colSortie="#6495ED",colBorder="black")
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(any(class(colEntree)!="character")) msg_error2 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colSortie)!="character")) msg_error3 <- "La couleur doit etre de type caractere (nommee ou hexadecimal) / "
    if(any(class(colBorder)!="character")) msg_error4 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
    }
    
    idx_fleche <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux") idx_fleche <- i
        if(map$x$calls[[i]]$args[[3]]$nom_fond=="fond_flux_leg") idx_legende <- i
      }
    }
    
    val_ent <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_fleche]]$args[[5]],str_locate(map$x$calls[[idx_fleche]]$args[[5]],":")[[1]]+2,nchar(map$x$calls[[idx_fleche]]$args[[5]])-11)," ",""))>=0)
    
    if(length(val_ent)>0)
    {
      map$x$calls[[idx_fleche]]$args[[4]]$fillColor[1:length(val_ent)] <- colEntree
    }
    
    val_sor <- which(as.numeric(str_replace_all(substring(map$x$calls[[idx_fleche]]$args[[5]],str_locate(map$x$calls[[idx_fleche]]$args[[5]],":")[[1]]+2,nchar(map$x$calls[[idx_fleche]]$args[[5]])-11)," ",""))<0)
    
    if(length(val_sor)>0)
    {
      map$x$calls[[idx_fleche]]$args[[4]]$fillColor[length(val_ent)+1:length(val_sor)] <- colSortie
    }
    
    map$x$calls[[idx_fleche]]$args[[4]]$color <- colBorder
    
    if(!is.null(idx_legende))
    {
      if(length(val_ent)>0)
      {
        map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colEntree
      }else
      {
        map$x$calls[[idx_legende]]$args[[4]]$fillColor <- colSortie
      }
      map$x$calls[[idx_legende]]$args[[4]]$color <- colBorder
    }
    
    return(map)
  }


flux_legende_saphirs <-
  function(x,y,long,large)
  {
    l <- list()
    
    xp <- x
    yp <- y
    xg <- x
    yg <- y+large/1.5
    
    vec <- matrix(c(xg,yg, xg+long,yg, xg+long+large*0.655,yg+large*0.655/2, xg+long,yg+large*0.655,   xg,yg+large*0.655,   xg,yg),6,2,byrow=T)
    l[[1]] <- st_polygon(list(vec))
    vec <- matrix(c(xp,yp, xp+long,yp, xp+long+large*0.655,yp+large*0.655/6, xp+long,yp+large*0.655/3, xp,yp+large*0.655/3, xp,yp),6,2,byrow=T)
    l[[2]] <- st_polygon(list(vec))
    
    flux_legWGS84 <- st_sf(geometry=st_sfc(l), crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
    
    pointe1 <- c(xg+long+large*0.655,yg+large*0.655/2)
    pointe2 <- c(xp+long+large*0.655,yp+large*0.655/6)
    
    return(list(flux_legWGS84,pointe1,pointe2))
  }


flux_legende_saphirs_pl <-
  function(x,y,long_pl,large_pl,code_epsg)
  {
    l_pl <- list()
    long_pl <- long_pl/2
    
    xp <- x
    yp <- y
    
    xpyp_pl <- st_sf(geometry=st_sfc(st_point(c(xp,yp))), crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
    xpyp_pl <- st_transform(xpyp_pl,paste0("+init=epsg:",code_epsg))
    
    xp_pl <- st_coordinates(xpyp_pl)[1]
    yp_pl <- st_coordinates(xpyp_pl)[2]
    xg_pl <- xp_pl
    yg_pl <- yp_pl+large_pl/2
    
    vec <- matrix(c(xg_pl,yg_pl, xg_pl+long_pl,yg_pl, xg_pl+long_pl+large_pl/2,yg_pl+large_pl/2, xg_pl+long_pl,yg_pl+large_pl,   xg_pl,yg_pl+large_pl,   xg_pl,yg_pl),6,2,byrow=T)
    l_pl[[1]] <- st_polygon(list(vec))
    vec <- matrix(c(xp_pl,yp_pl, xp_pl+long_pl,yp_pl, xp_pl+long_pl+large_pl/2,yp_pl+large_pl/6, xp_pl+long_pl,yp_pl+large_pl/3, xp_pl,yp_pl+large_pl/3, xp_pl,yp_pl),6,2,byrow=T)
    l_pl[[2]] <- st_polygon(list(vec))
    
    flux_leg_pl <- st_sf(geometry=st_sfc(l_pl), crs=paste0("+init=epsg:",code_epsg))
    
    return(flux_leg_pl)
  }


extract_fond_leaflet_saphirs <-
  function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_fleche <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("carte_saphirs"))) idx_carte <- c(idx_carte,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(any(map$x$calls[[i]]$args[3][[1]]$nom_couche %in% c("legende_saphirs"))) idx_legende <- c(idx_legende,i)
      }
      if(!is.null(idx_legende)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(any(map$x$calls[[i]]$args[5][[1]]$nom_couche %in% c("legende_saphirs"))) idx_legende <- c(idx_legende,i)
        }
      }
    }
    
    if(is.null(idx_legende))
    {
      return(NULL)
    }else
    {
      idx_fleche <- idx_carte[length(idx_carte)]
      idx_carte <- idx_carte[-length(idx_carte)]
      
      var_flux <- map$x$calls[[idx_fleche]]$args[[3]]$var_flux
      
      code_epsg <- map$x$calls[[idx_fleche]]$args[[3]]$code_epsg
      dom <- map$x$calls[[idx_fleche]]$args[[3]]$dom
      
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        rm(aa,bb)
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      if(!is.null(idx_fleche))
      {
        aa <- lapply(1:length(map$x$calls[[idx_fleche]]$args[[1]]), function(x) st_polygon(list(as.matrix(map$x$calls[[idx_fleche]]$args[[1]][[x]][[1]][[1]]))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(i in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(list(aa[[i]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_fleche]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],":")[[1]][1]+2,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][4]-1))
          cc <- as.numeric(str_replace_all(str_replace_all(cc,",",".")," ",""))
          fond <- cbind(var=cc,bb)
          
          dd <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]+5,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"<")[[1]][3]-1))
          fond <- cbind(CODE2=dd,fond)
          ee <- sapply(1:length(map$x$calls[[idx_fleche]]$args[[5]]), function(y) substring(map$x$calls[[idx_fleche]]$args[[5]][[y]],str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],">")[[1]][2]+1,str_locate_all(map$x$calls[[idx_fleche]]$args[[5]][[y]],"vers")[[1]][1]-1))
          fond <- cbind(CODE1=ee,fond)
          
          names(fond) <- c("CODE1","CODE2",var_flux,"geometry")
          rm(aa,bb,cc,dd,ee)
        }
        
        fond <- st_transform(fond,paste0("+init=epsg:",code_epsg))
        
        fond_entree <- fond[data.frame(fond)[,var_flux]>=0,]
        fond_sortie <- fond[data.frame(fond)[,var_flux]<0,]
        
        if(nrow(fond_entree)>0)
        {
          list_fonds[[l]] <- fond_entree
          nom_fonds <- c(nom_fonds,paste0(map$x$calls[[idx_fleche]]$args[[3]]$nom_fond,"_entree"))
          l <- l+1
        }
        if(nrow(fond_sortie)>0)
        {
          list_fonds[[l]] <- fond_sortie
          nom_fonds <- c(nom_fonds,paste0(map$x$calls[[idx_fleche]]$args[[3]]$nom_fond,"_sortie"))
          l <- l+1
        }
      }
      
      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }
      
      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }
      
      if(!is.null(idx_legende))
      {
        large <- map$x$calls[[idx_fleche[1]]]$args[[3]]$distance
        long <- large
        
        gf <- st_sf(geometry=st_sfc(st_polygon(list(as.matrix(map$x$calls[[idx_legende[1]]]$args[[1]][[2]][[1]][[1]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        x <- st_bbox(gf)$xmin
        y <- st_bbox(gf)$ymin
        flux_leg_pl <- flux_legende_saphirs_pl(x,y,long,large,code_epsg)
        
        max_var <- map$x$calls[[idx_fleche]]$args[[3]]$max_var
        flux_leg_pl <- cbind(VAR=c(max_var,max_var/3),flux_leg_pl)
        names(flux_leg_pl) <- c(var_flux,"geometry")
        
        list_fonds[[l]] <- flux_leg_pl
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende[[1]]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      colFleche <- unique(map$x$calls[[idx_fleche]]$args[4][[1]]$fillColor)
      
      if(length(colFleche)>1)
      {
        colEntree <- colFleche[1]
        colSortie <- colFleche[2]
      }else
      {
        colEntree <- colFleche
        colSortie <- colFleche
      }
      colBorder <- map$x$calls[[idx_fleche]]$args[4][[1]]$color
      
      return(list(list_fonds,nom_fonds,titre,source,colEntree,colSortie,colBorder,dom))
    }
  }


export_qgis_saphirs <-
  function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_saphirs(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des fleches saphirs n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_saphirs(map) pour ajouter une legende de fleches a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_flux_entree")) l <- c(l,"fond_flux_entree")
    if(any(list_fonds[[2]] %in% "fond_flux_sortie")) l <- c(l,"fond_flux_sortie")
    if(any(list_fonds[[2]] %in% "fond_flux_leg")) l <- c(l,"fond_flux_leg")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    colEntree <- list_fonds[[5]]
    colSortie <- list_fonds[[6]]
    colBorder <- list_fonds[[7]]
    
    export_projet_qgis_fleches_saphirs(l,rep_sortie,sortie,titre1,titre2,source,colEntree,colSortie,colBorder,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }


export_projet_qgis_fleches_saphirs <-
  function(liste_fonds,chemin_fonds,nom_projet,titre,titre2,sourc,colEntree,colSortie,colBorder,annee)
  {
    chemin_fonds <- paste0(chemin_fonds,"/")
    
    fond_maille <- read_sf(paste0(chemin_fonds,"fond_maille.shp"))
    xmin=st_bbox(fond_maille)[1]-0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    xmax=st_bbox(fond_maille)[3]+0.10*(st_bbox(fond_maille)[3]-st_bbox(fond_maille)[1])
    ymin=st_bbox(fond_maille)[2]-0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    ymax=st_bbox(fond_maille)[4]+0.10*(st_bbox(fond_maille)[4]-st_bbox(fond_maille)[2])
    
    #on reajuste la fenetre pour que (xmax-xmin)=1.65(ymax-ymin)
    dif_x=xmax-xmin
    dif_y=ymax-ymin
    
    if (dif_x>dif_y)
    {
      if (dif_x/dif_y<1.65)
      {
        xmin=xmin-((1.65*dif_y-dif_x)/2)
        xmax=xmax+((1.65*dif_y-dif_x)/2)
      }else
      {
        ymin=ymin-((dif_x/1.65)-dif_y)/2
        ymax=ymax+((dif_x/1.65)-dif_y)/2
      }   
    }else
    {
      xmin=xmin-((1.65*dif_y-dif_x)/2)
      xmax=xmax+((1.65*dif_y-dif_x)/2)
    }
    
    BLOCCOMPOSER=modif_canevas_MEP(nom_projet,str_sub(titre,end=-2),titre2,annee,sourc,xmin,xmax,ymin,ymax)
    
    BLOCLEG=data.frame()
    BLOCPROJECT=data.frame()
    BLOCLAYERITEM=data.frame()
    
    l <- liste_fonds
    for (i in 1:length(l))
    {
      #BLOCLEG 
      if(nchar(l[i])<11)
      {
        idcouche=paste0(l[i],str_c(rep("0",11-nchar(l[i])), collapse = ""))
      }else
      {
        idcouche=l[i]
      }
      
      toto=modif_blocleg(l[i],idcouche)
      BLOCLEG=rbind(BLOCLEG,toto)
      
      #BLOCLAYERITEM                  
      if(str_sub(l[i][length(l[i])],start=-5)!="carte")
      {
        bloclayeritem=modif_bloclayeritem(l[i],idcouche,"hidden")
      }
      
      #BLOCPROJECT
      #param idcouche, chemincouche, nomcouche
      nomcouche=l[i]
      chemincouche=paste0(chemin_fonds,nomcouche,".shp")
      BLOCCATEGORIES=data.frame()      
      
      attr=""
      name="0"
      typeanalyse="singleSymbol"
      couleurfond="255,255,255"
      
      if (l[i] %in% c("fond_flux_entree","fond_flux_leg"))
      {
        couleurfond=colEntree
        remplissagefond="yes"
      }else if (l[i] %in% c("fond_flux_sortie"))
      {
        couleurfond=colSortie
        remplissagefond="yes"
      }else
      {
        remplissagefond="no"
      }
      
      if (l[i] %in% c("fond_maille","fond_departement","fond_region","fond_pays"))
      {
        couleurbordure="128,128,128"
      }else if (l[i]=="fond_territoire")
      {
        couleurbordure="191,191,191"
      }else if (l[i] %in% c("fond_flux_entree","fond_flux_sortie","fond_flux_leg"))
      {
        couleurbordure=colBorder
      }else
      {
        couleurbordure="0,0,0"
      }
      
      stylebordure="solid"
      if (l[i] %in% c("fond_flux_entree","fond_flux_sortie","fond_flux_leg","fond_maille","fond_departement","fond_pays","fond_territoire"))
      {
        epaisseurbordure=0.26
      }else
      {
        epaisseurbordure=0.5
      }
      
      analyse_fj_fs <- read_sf(chemincouche)
      projcouche=st_crs(analyse_fj_fs)$proj4string
      geometrie=attr(analyse_fj_fs$geometry[[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_fj_fs[[1]][[1]],"class")[2]
      
      if(is.null(geometrie))
        geometrie=attr(analyse_fj_fs[[1]],"class")[2]
      
      if (geometrie %in% c("POLYGON","MULTIPOLYGON"))
      {
        BLOCSYMBOLS=modif_blocsymbolsPolygon(couleurfond,couleurbordure,remplissagefond,stylebordure,epaisseurbordure,name)
      }else
      {
        if (geometrie %in% c("LINESTRING","MULTILINESTRING"))
        {
          BLOCSYMBOLS=modif_blocsymbolsLine(couleurbordure,stylebordure,epaisseurbordure,name)
        }
      }
      
      blocvector=modif_blocvectorClassification(nomcouche)
      blocvector=data.frame(V1=c(blocvector[1:2,],BLOCSYMBOLS[,1],blocvector[4:5,]))
      bloclayeritem=data.frame(V1=c(bloclayeritem[1,],blocvector[,1],bloclayeritem[3,]))
      BLOCLAYERITEM=rbind(BLOCLAYERITEM,bloclayeritem)
      
      toto=modif_blocprojectlayers(geometrie,idcouche,chemincouche,nomcouche,projcouche,attr,typeanalyse)
      toto=rbind(data.frame(V1=toto[1:13,]),BLOCCATEGORIES,data.frame(V1=toto[15,]),BLOCSYMBOLS,data.frame(V1=toto[17:23,]))
      BLOCPROJECT=rbind(BLOCPROJECT,toto)
    }
    projproj=projcouche
    qgs1=modif_canevas(xmin,xmax,ymin,ymax,projproj,length(l))
    #etape finale
    BLOCCOMPOSER=data.frame(V1=c(BLOCCOMPOSER[1:43,],BLOCLAYERITEM[,1],BLOCCOMPOSER[45:94,]))
    canevas_final=data.frame(V1=c(qgs1[1:19,],BLOCLEG[,1],qgs1[21,],BLOCCOMPOSER[,1],qgs1[23,],BLOCPROJECT[,1],qgs1[25:26,]))
    colnames(canevas_final)=NULL
    write.csv(canevas_final,paste0(chemin_fonds,nom_projet,".qgs"),row.names = F, quote = F, fileEncoding = "UTF-8")
  }

