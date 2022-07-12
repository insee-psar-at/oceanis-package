sf_pays973 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","pays973.shp", package = "oceanis")
  # import de l'objet sf
  pays973 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
return(pays973)
}
