sf_fram <- function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","francemetro_2018.shp", package = "oceanis")
  # import de l'objet sf
  fram <- st_read(dsn = path_to_shp, quiet = TRUE)
  names(fram) <- c("CODE","LIBGEO","SURF","geometry")
  return(fram)
}