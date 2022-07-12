sf_reg02 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg02.shp", package = "oceanis")
  # import de l'objet sf
  reg02 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
return(reg02)
}
