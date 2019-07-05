export_pdf <-
function(map,chemin,nomFichier)
  {
    mapshot(map, file = paste0(chemin,"/",nomFichier,".pdf"), selfcontained = FALSE)
  }
