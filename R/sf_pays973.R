sf_pays973 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","pays973.shp", package = "oceanis")
  # import de l'objet sf
  pays973 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  pays973 <- st_transform(pays973, crs = 2972)
  
  names(pays973) <- c("LIBGEO","geometry")
  
return(pays973)
}
