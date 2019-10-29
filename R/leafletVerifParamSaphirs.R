leafletVerifParamSaphirs <-
function(data,fondMaille,typeMaille,fondSuppl,idDataDepart,idDataArrivee,varFlux,largeurFlecheMax,direction,filtreVol,colEntree,colSortie,colBorder,emprise,fondEtranger,map_proxy){
  msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24<-msg_error25 <- NULL
  
  if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
  if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
  if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error3 <- "Le fond supplementaire doit etre un objet sf / "
  if(any(class(typeMaille)!="character")) msg_error4 <- "La valeur doit etre de type caractere ('REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'COM') / "
  if(any(class(idDataDepart)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
  if(any(class(idDataArrivee)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
  if(any(class(varFlux)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
  if(!is.null(largeurFlecheMax)) if(any(class(largeurFlecheMax)!="numeric")) msg_error8 <- "La largeur de la fleche max doit etre de type numerique (en km) / "
  if(any(class(direction)!="character")) msg_error9 <- "La direction des fleches doit etre de type caractere / "
  if(any(class(filtreVol)!="numeric")) msg_error10 <- "Le filtre doit etre de type numerique / "
  if(any(class(colEntree)!="character")) msg_error11 <- "Le style de couleur doit etre de type caractere / "
  if(any(class(colSortie)!="character")) msg_error12 <- "Le style de couleur doit etre de type caractere / "
  if(any(class(colBorder)!="character")) msg_error13 <- "Le style de couleur doit etre de type caractere / "
  if(any(class(emprise)!="character")) msg_error14 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "
  if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error15 <- "Le fond etranger doit etre un objet sf / "
  
  if(length(names(data))<3) msg_error16 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable de depart, une variable d'arrivee et la variable a representer / "
  if(length(names(fondMaille))<3) msg_error17 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error18 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error19 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  
  if(!typeMaille %in% c("REG", "DEP", "ZE", "AU", "BV", "UU", "EPCI", "DEPCOM")) msg_error20 <- "La variable typeMaille doit etre 'REG', 'DEP', 'ZE', 'AU', 'BV', 'UU', 'EPCI' ou 'COM' / "
  if(!any(names(data) %in% idDataDepart))  msg_error21 <- "La variable de depart n'existe pas dans la table des donnees / "
  if(!any(names(data) %in% idDataArrivee))  msg_error22 <- "La variable d'arrivee n'existe pas dans la table des donnees / "
  if(!any(names(data) %in% varFlux))  msg_error23 <- "La variable a representer n'existe pas dans la table des donnees / "
  if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error24 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976' ou '999' / "
  
  if(!is.null(map_proxy)) if (any(class(map_proxy)!="character")) if(!any(class(map_proxy) %in% "leaflet_proxy")) msg_error25 <- "Le parametre map_proxy doit etre un objet leaflet_proxy ou une chaine de caractere (voir aide) / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
         !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
         !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
         !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
         !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
         !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24),!is.null(msg_error25)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                            msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,
                            msg_error16,msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,msg_error23,msg_error24,msg_error25)))
  }
}
