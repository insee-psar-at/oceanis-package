sf_reg04 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg04.shp", package = "oceanis")
  # import de l'objet sf
  reg04 <- st_read(dsn = path_to_shp, quiet = TRUE)
return(reg04)
}
