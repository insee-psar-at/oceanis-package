sf_reg01 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg01.shp", package = "oceanis")
  # import de l'objet sf
  reg01 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  reg01 <- st_transform(reg01, crs = 5490)
  
return(reg01)
}
