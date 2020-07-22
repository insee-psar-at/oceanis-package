k_oursins <-
function(fond_carto_k,variable_jointure_fond_carto_k,donnees_k,var_depart_k,var_arrivee_k,var_flux_k,decalage_aller_retour_k,decalage_centroid_k)
  {
    suppressWarnings(donnees_k <- donnees_k[!is.na(donnees_k[,var_flux_k]),])

    fond_carto_k <- fond_carto_k[as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k] %in% c(donnees_k[,var_depart_k],donnees_k[,var_arrivee_k]),]
    suppressWarnings(coord <- data.frame(CODGEO=as.data.frame(fond_carto_k)[,variable_jointure_fond_carto_k],X=st_coordinates(st_centroid(fond_carto_k))[,1],Y=st_coordinates(st_centroid(fond_carto_k))[,2]))

    donnees_k$idx_oceanis <- 1:nrow(donnees_k)
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
    base <- base[order(base[,"idx_oceanis"]),]
    base$idx_oceanis <- 1:nrow(base)

    # ETAPE 1 : decalage des lignes du centroid

    for(i in 1:nrow(base))
    {
      nb_ar <- base[i,"AR"]

      # Coordonnees des points A et B
      Ax <- base[i,"X.R"]
      Ay <- base[i,"Y.R"]
      Bx <- base[i,"X.T"]
      By <- base[i,"Y.T"]

      if(decalage_centroid_k>0)
      {
        rapport <- (((Ax-Bx)^2+(Ay-By)^2)^(1/2))/(decalage_centroid_k*1000)

        #(Ax-Bx)/(Ax-Cx)=rapport
        Ax <- Ax-((Ax-Bx)/rapport)
        Ay <- Ay-((Ay-By)/rapport)
        #(Bx-Ax)/(Bx-Dx)=rapport
        Bx <- Bx-((Bx-Ax)/rapport)
        By <- By-((By-Ay)/rapport)
      }

      base[i,"X.R"] <- Ax
      base[i,"Y.R"] <- Ay
      base[i,"X.T"] <- Bx
      base[i,"Y.T"] <- By
    }

    # ETAPE 2 : decalage des lignes aller et retour

    base_0 <- base[base$AR==0,] # sans aller-retour
    base_AR <- base[base$AR!=0,] # avec aller-retour
    decalage <- (decalage_aller_retour_k/2)*1000

    l <- list()
    for(i in 1:nrow(base_AR))
    {
      nb_ar <- base_AR[i,"AR"]

      # Coordonnees des points A et B
      Ax <- base_AR[i,"X.R"]
      Ay <- base_AR[i,"Y.R"]
      Bx <- base_AR[i,"X.T"]
      By <- base_AR[i,"Y.T"]

      if(Ax==Bx) # La ligne est verticale
      {
        A1x <- Ax
        A1y <- Ay+decalage
        A2x <- Ax
        A2y <- Ay-decalage
        B1x <- Bx
        B1y <- By+decalage
        B2x <- Bx
        B2y <- By-decalage
      }else if(Ay==By) # La ligne est horizontale
      {
        A1x <- Ax+decalage
        A1y <- Ay
        A2x <- Ax+decalage
        A2y <- Ay
        B1x <- Bx+decalage
        B1y <- By
        B2x <- Bx+decalage
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
        M <- decalage/N
        # (M^2+(M*S2)^2)^(1/2) = decalage_aller_retour_k/2 : verif

        A1x <- Ax+M
        A1y <- Ay+M*S2
        A2x <- Ax-M
        A2y <- Ay-M*S2
        B1x <- Bx+M
        B1y <- By+M*S2
        B2x <- Bx-M
        B2y <- By-M*S2
      }

      # Nouvelles coordonnees des points de A vers B puis de B vers A
      doublon <- which(base_AR[,"AR"]==nb_ar)

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

    temp <- base[,c(var_depart_k,var_arrivee_k,var_flux_k)]
    rownames(temp) <- as.character(1:dim(temp)[1])

    l=list()
    for (i in 1:dim(base_AR)[1])
    {
      vec <- (matrix(as.numeric(base[i,c("X.R","X.T","Y.R","Y.T")]),2,2,F))
      l[[i]] <- st_linestring(vec)
    }
    suppressWarnings(resultat <- st_sf(cbind(temp,geometry=st_sfc(l)), crs=st_crs(fond_carto_k)))

    return(list(analyse=resultat))
  }
