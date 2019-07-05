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
