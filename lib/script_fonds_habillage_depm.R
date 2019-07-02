sf_depm <- function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","dep_francemetro_2018.shp", package = "oceanis")
  # import de l'objet sf
  depm <- st_read(dsn = path_to_shp, quiet = TRUE)
  names(depm) <- c("CODE","LIBELLE","REG","SURF","geometry")
  return(depm)
}