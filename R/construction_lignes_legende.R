construction_lignes_legende <-
function(ronds_leg,coeff,code_epsg)
{
  ronds_sf_leg <- ronds_leg[[1]]
  ronds_pl_leg <- ronds_leg[[2]]
  
  # Pour le leaflet en WGS84
  x1_grand <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])]
  y1_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
  pts1_grand <- c(x1_grand,y1_grand)
  x2_grand <- x1_grand+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_grand)+coeff*0.5
  y2_grand <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"Y"])
  pts2_grand <- c(x2_grand,y2_grand)
  ligne_grand <- rbind(pts1_grand,pts2_grand)
  
  x1_petit <- st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"X"][which.max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])]
  y1_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
  pts1_petit <- c(x1_petit,y1_petit)
  x2_petit <- x1_petit+(max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])-x1_petit)+coeff*0.5
  y2_petit <- max(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==2),"Y"])
  pts2_petit <- c(x2_petit,y2_petit)
  ligne_petit <- rbind(pts1_petit,pts2_petit)
  
  lignes <- st_multilinestring(list(ligne_grand,ligne_petit))
  
  # Pour l'export Qgis en projection locale
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
  
  lignes_pl <- st_sf(st_geometry(st_multilinestring(list(ligne_grand_pl,ligne_petit_pl))))
  lignes_pl <- st_set_crs(lignes_pl,paste0("+init=epsg:",code_epsg))
  
  # On ajoute un titre a la legende
  x_titre_1 <- min(st_coordinates(ronds_sf_leg)[which(st_coordinates(ronds_sf_leg)[,4]==1),"X"])
  y_titre_1 <- y1_grand+coeff*0.7
  
  return(list(lignes,ligne_grand,ligne_petit,x_titre_1,y_titre_1,ronds_pl_leg,lignes_pl))
}
