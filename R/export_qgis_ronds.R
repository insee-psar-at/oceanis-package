export_qgis_ronds <-
function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")
    
    list_fonds <- extract_fond_leaflet_ronds(map)
    
    if(is.null(list_fonds)) stop(simpleError("La legende des ronds n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_ronds(map) pour ajouter une legende de ronds a votre carte."))
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }
    
    annee <- format(Sys.time(), format = "%Y")
    
    l <- c()
    if(any(list_fonds[[2]] %in% "fond_ronds_pos_carte")) l <- c(l,"fond_ronds_pos_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_neg_carte")) l <- c(l,"fond_ronds_neg_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_pos_elargi_carte")) l <- c(l,"fond_ronds_pos_elargi_carte")
    if(any(list_fonds[[2]] %in% "fond_ronds_neg_elargi_carte")) l <- c(l,"fond_ronds_neg_elargi_carte")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")
    if(any(list_fonds[[2]] %in% "fond_maille_elargi")) l <- c(l,"fond_maille_elargi")
    if(any(list_fonds[[2]] %in% "fond_lignes_leg")) l <- c(l,"fond_lignes_leg")
    if(any(list_fonds[[2]] %in% "fond_ronds_leg_carte")) l <- c(l,"fond_ronds_leg_carte")
    
    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    if(any(list_fonds[[2]] %in% "fond_etranger"))l <- c(l,"fond_etranger")
    
    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")
    
    export_projet_qgis_ronds(l,rep_sortie,sortie,titre1,titre2,source,annee)
    
    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }
