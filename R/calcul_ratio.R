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
