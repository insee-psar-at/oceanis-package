sf_reg04 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg04.shp", package = "oceanis")
  # import de l'objet sf
  reg04 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  st_crs(reg04)$input <- 7077
  
  reg04 <- st_transform(reg04, crs = 2975)
  
return(reg04)
}
