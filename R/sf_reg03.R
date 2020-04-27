sf_reg03 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg03.shp", package = "oceanis")
  # import de l'objet sf
  reg03 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  st_crs(reg03)$input <- 31922
  
  reg03 <- st_transform(reg03, crs = 2972)
  
return(reg03)
}
