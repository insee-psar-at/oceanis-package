k_saphir <-
function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,var_depart_k,var_arrivee_k,var_flux_k,largeur_k,longueur_k,type)
  {
    if(is.null(type)) type <- "Ent"
    
    donnees_k$save <- donnees_k[,var_flux_k]
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
    
    if(type=="Sor")
    {
      unix <- (base$X.Z-base$X.T)/((base$X.Z-base$X.T)^2+(base$Y.Z-base$Y.T)^2)^0.5
      uniy <- (base$Y.Z-base$Y.T)/((base$X.Z-base$X.T)^2+(base$Y.Z-base$Y.T)^2)^0.5
      
      base$X.R <- base$X.T+longueur_k*unix
      base$Y.R <- base$Y.T+longueur_k*uniy
      
      base$typeFlux <- -1
      
      base <- base[,c(var_depart_k,var_arrivee_k,var_flux_k,"save","X.R","Y.R","X.T","Y.T","typeFlux")]
    }
    if(type=="Ent")
    {
      base$X.R <- base$X.Z
      base$Y.R <- base$Y.Z
      
      unix <- (base$X.T-base$X.R)/((base$X.T-base$X.R)^2+(base$Y.T-base$Y.R)^2)^0.5
      uniy <- (base$Y.T-base$Y.R)/((base$X.T-base$X.R)^2+(base$Y.T-base$Y.R)^2)^0.5
      
      base$X.T <- base$X.R+longueur_k*unix
      base$Y.T <- base$Y.R+longueur_k*uniy
      
      base$typeFlux <- 1
      
      base <- base[,c(var_depart_k,var_arrivee_k,var_flux_k,"save","X.R","Y.R","X.T","Y.T","typeFlux")]
    }
    
    if(type=="Sol")
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
        temp1 <- temp1[,c(var_depart_k,var_arrivee_k,var_flux_k,"save","X.R","Y.R","X.T","Y.T","typeFlux")]
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
        temp2 <- temp2[,c(var_depart_k,var_arrivee_k,var_flux_k,"save","X.R","Y.R","X.T","Y.T","typeFlux")]
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
    
    fleche <- st_sf(cbind(base[,c(var_depart_k,var_arrivee_k,var_flux_k,"save")],geometry=st_sfc(l)), crs=st_crs(fond_carto_k))
    
    return(list(analyse=fleche))
  }
