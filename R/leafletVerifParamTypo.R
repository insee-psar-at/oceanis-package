leafletVerifParamTypo <-
function(data,fondMaille,fondSuppl,idData,varTypo,emprise,fondEtranger,map_proxy){
  msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15 <- NULL
  
  if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
  if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
  if(!is.null(fondSuppl)) if(any(!any(class(fondSuppl) %in% "sf"),!any(class(fondSuppl) %in% "data.frame"))) msg_error3 <- "Le fond supplementaire doit etre un objet sf / "
  if(any(class(idData)!="character")) msg_error4 <- "Le nom de la variable doit etre de type caractere / "
  if(any(class(varTypo)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
  if(any(class(emprise)!="character")) msg_error6 <- "La valeur doit etre de type caractere ('FRM', '971', '972', '973', '974', '976' ou '999') / "
  if(!is.null(fondEtranger)) if(any(!any(class(fondEtranger) %in% "sf"),!any(class(fondEtranger) %in% "data.frame"))) msg_error7 <- "Le fond etranger doit etre un objet sf / "
  
  if(length(names(data))<2) msg_error8 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
  if(length(names(fondMaille))<3) msg_error9 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  if(!is.null(fondSuppl)) if(length(names(fondSuppl))<3) msg_error10 <- "Le fond supplementaire n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  if(!is.null(fondEtranger)) if(length(names(fondEtranger))<3) msg_error11 <- "Le fond etranger n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
  
  if(!any(names(data) %in% idData))  msg_error12 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
  if(!any(names(data) %in% varTypo))  msg_error13 <- "La variable a representer n'existe pas dans la table des donnees / "
  if(!emprise %in% c("FRM","971","972","973","974","976","999")) msg_error14 <- "La variable emprise doit etre 'FRM', '971', '972', '973', '974', '976' ou '999' / "
  
  if(!is.null(map_proxy)) if (any(class(map_proxy)!="character")) if(!any(class(map_proxy) %in% "leaflet_proxy")) msg_error15 <- "Le parametre map_proxy doit etre un objet leaflet_proxy ou une chaine de caractere (voir aide) / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
         !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
         !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
         !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                            msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15)))
  }
}
