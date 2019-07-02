sf_regm <-
function()
{
  # chemin du fond de carte .shp
  path_to_shp <- system.file("extdata","reg_francemetro_2018.shp", package = "oceanis")
  # import de l'objet sf
  regm <- st_read(dsn = path_to_shp, quiet = TRUE)
  names(regm) <- c("CODE","LIBELLE","SURF","geometry")
  return(regm)
}
