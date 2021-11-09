construction_lignes_legende <-
function(ronds_leg,coeff,code_epsg)
  {
    ronds_pl_leg <- ronds_leg[[2]]
    
    x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
    y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
    pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
    x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+coeff*30000 #on est ici en Lambert93 pour l'export en Qgis
    y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
    pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
    ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl)

    x1_petit_pl <- x1_grand_pl
    y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
    pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
    x2_petit_pl <- x2_grand_pl
    y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
    pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
    ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl)

    ligne_grand_pl <- st_sf(geometry = st_geometry(st_linestring(ligne_grand_pl)))
    ligne_petit_pl <- st_sf(geometry = st_geometry(st_linestring(ligne_petit_pl)))
    lignes_pl <- rbind(ligne_grand_pl, ligne_petit_pl)
    lignes_pl <- st_set_crs(lignes_pl,as.numeric(code_epsg))

    lignes <- st_transform(lignes_pl, crs = 4326)

    return(list(lignes,lignes_pl))
  }
