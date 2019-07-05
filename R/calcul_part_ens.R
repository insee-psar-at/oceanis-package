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
