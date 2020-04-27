sf_paysm <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","paysm.shp", package = "oceanis")
  # import de l'objet sf
  paysm <- st_read(dsn = path_to_shp, quiet = TRUE)
  
  st_crs(paysm)$input <- 102110
  
  paysm <- st_transform(paysm, crs = 2154)
  
return(paysm)
}
