export_jpeg <-
function(map,chemin,nomFichier)
  {
    mapshot(map, file = paste0(chemin,"/",nomFichier,".jpeg"), selfcontained = FALSE)
  }
