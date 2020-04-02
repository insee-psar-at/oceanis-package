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
