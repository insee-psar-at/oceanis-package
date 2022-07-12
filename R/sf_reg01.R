sf_reg01 <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg01.shp", package = "oceanis")
  # import de l'objet sf
  reg01 <- st_read(dsn = path_to_shp, quiet = TRUE)
  
return(reg01)
}
