sf_reg06 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg06.shp", package = "oceanis")
  # import de l'objet sf
  reg06 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
return(reg06)
}
