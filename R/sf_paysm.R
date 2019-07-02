sf_paysm <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","paysm.shp", package = "oceanis")
  # import de l'objet sf
  paysm <- st_read(dsn = path_to_shp, quiet = TRUE)
return(paysm)
}
