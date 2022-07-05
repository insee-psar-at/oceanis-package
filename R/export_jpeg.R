export_jpeg <-
function(map,chemin,nomFichier)
{
  url <- gsub("\\\\", "/", tempfile(fileext = ".html"))
  
  htmlwidgets::saveWidget(widget = map,
                          file = url)
  
  webshot::webshot(url = url,
                   file = paste0(chemin,"/",nomFichier,".jpeg"))
}
