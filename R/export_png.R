export_png <-
function(map,chemin,nomFichier)
  {
    mapshot(map, file = paste0(chemin,"/",nomFichier,".png"), selfcontained = FALSE)
  }
