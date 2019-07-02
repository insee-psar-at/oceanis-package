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
