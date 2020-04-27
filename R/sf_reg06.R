sf_reg06 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg06.shp", package = "oceanis")
  # import de l'objet sf
  reg06 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  st_crs(reg06)$input <- 7075
  
  reg06 <- st_transform(reg06, crs = 4471)
  
return(reg06)
}
