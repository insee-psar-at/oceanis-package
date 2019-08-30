leafletVerifParamClassesRonds <-
function(data,fondMaille,fondMailleElargi,fondSuppl,idData,varVolume,varRatio,rayonRond,rapportRond,methode,nbClasses,bornes,stylePalette,opacityElargi,colBorderClasses,colBorderRonds,precision,dom,fondChx,map_proxy){
  msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24<-msg_error25<-msg_error26<-msg_error27<-msg_error28<-msg_error29 <- NULL
  
  if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
  if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
  if(!is.null(fondMailleElargi)) if(any(!any(class(fondMailleElargi) %in% "sf"),!any(class(fondMailleElargi) %in% "data.frame"))) msg_error3 <- "Le fond de maille elargie doit etre un objet sf / "
  if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error4 <- "Le fond supplementaire doit etre un objet sf / "
  if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
  if(any(class(varVolume)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
  if(!is.null(rayonRond)) if(any(class(rayonRond)!="numeric")) msg_error7 <- "La variable doit etre de type numerique / "
  if(!is.null(rapportRond)) if(any(class(rapportRond)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
  if(any(class(varRatio)!="character")) msg_error9 <- "Le nom de la variable doit etre de type caractere / "
  if(any(class(methode)!="character")) msg_error10 <- "La nom de la methode doit etre de type caractere / "
  if(any(class(nbClasses)!="numeric")) msg_error11 <- "La variable doit etre de type numerique / "
  if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error12 <- "La variable doit etre un vecteur numerique / "
  if(any(class(stylePalette)!="character")) msg_error13 <- "Le style de palette doit etre un vecteur de type caractere / "
  if(any(class(opacityElargi)!="numeric")) msg_error14 <- "La variable doit etre de type numerique / "
  if(any(class(dom)!="character")) msg_error15 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
  if(any(class(colBorderClasses)!="character")) msg_error16 <- "Le style de couleur doit etre de type caractere / "
  if(any(class(colBorderRonds)!="character")) msg_error17 <- "Le style de couleur doit etre de type caractere / "
  if(any(class(precision)!="numeric")) msg_error18 <- "La variable doit etre de type numerique / "
  if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error19 <- "Le fond des chx doit etre un objet sf / "
  
  if(length(names(data))<2) msg_error20 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
  if(length(names(fondMaille))<3) msg_error21 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  if(!is.null(fondMailleElargi)) if(length(names(fondMailleElargi))<3) msg_error22 <- "Le fond de maille elargie n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error23 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  
  if(!any(names(data) %in% idData))  msg_error24 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
  if(!any(names(data) %in% varVolume))  msg_error25 <- "La variable a representer n'existe pas dans la table des donnees / "
  if(!any(names(data) %in% varRatio))  msg_error26 <- "La variable a representer n'existe pas dans la table des donnees / "
  if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error27 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
  if(!dom %in% c("0","971","972","973","974","976")) msg_error28 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
  
  if (!is.null(map_proxy)) if (any(class(map_proxy)!="character")) if(!any(class(map_proxy) %in% "leaflet_proxy")) msg_error29 <- "Le parametre map_proxy doit etre un objet leaflet_proxy ou une chaine de caractere (voir aide) / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
         !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
         !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
         !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
         !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
         !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24),
         !is.null(msg_error25),!is.null(msg_error26),!is.null(msg_error27),!is.null(msg_error28),!is.null(msg_error29)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                            msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                            msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,msg_error23,msg_error24,
                            msg_error25,msg_error26,msg_error27,msg_error28,msg_error29)))
  }
}
