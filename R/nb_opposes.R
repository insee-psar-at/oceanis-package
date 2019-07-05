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
