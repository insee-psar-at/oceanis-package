sf_reg03 <- function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg03.shp", package = "oceanis")
  # import de l'objet sf
  reg03 <- st_read(dsn = path_to_shp, quiet = TRUE)
return(reg03)
}