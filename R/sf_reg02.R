sf_reg02 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg02.shp", package = "oceanis")
  # import de l'objet sf
  reg02 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  reg02 <- st_transform(reg02, crs = 5490)
  
return(reg02)
}
