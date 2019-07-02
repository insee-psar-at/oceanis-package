export_qgis_typo <-
function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
{
  sortie <- nomFichier
  rep_sortie <- cheminDossier
  files <- paste0(rep_sortie,"/",sortie,".qgs")
  
  list_fonds <- extract_fond_leaflet_typo(map)
  
  if(is.null(list_fonds)) stop(simpleError("La legende de la typologie n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_typo(map) pour ajouter une legende de typologie a votre carte."))
  
  for(i in 1:length(list_fonds[[1]]))
  {
    suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
  }
  
  annee <- format(Sys.time(), format = "%Y")
  
  l <- c()
  if(any(list_fonds[[2]] %in% "fond_maille_typo_carte")) l <- c(l,"fond_maille_typo_carte")
  
  if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
  if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
  
  if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
  if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
  if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
  
  if(is.null(titre1)) titre1 <- list_fonds[[3]]
  if(is.null(source)) source <- list_fonds[[4]]
  titre_leg <- list_fonds[[6]]
  table_typo <- list_fonds[[5]]
  variable_a_representer <- list_fonds[[7]]
  
  export_projet_qgis_typo(l,rep_sortie,sortie,titre1,titre2,source,titre_leg,table_typo,variable_a_representer,annee)
  
  message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
}
