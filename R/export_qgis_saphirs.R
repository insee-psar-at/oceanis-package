export_qgis_saphirs <-
function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")

    list_fonds <- extract_fond_leaflet_saphirs(map)

    if(is.null(list_fonds)) stop(simpleError("La legende des fleches saphirs n'a pas ete creee. Veuillez svp utiliser la fonction add_legende_saphirs(map) pour ajouter une legende de fleches a votre carte."))

    dir.create(paste0(rep_sortie,"/layers"),showWarnings = F)
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/layers/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }

    annee <- format(Sys.time(), format = "%Y")

    l <- c()
    if(any(list_fonds[[2]] %in% "fond_flux_entree")) l <- c(l,"fond_flux_entree")
    if(any(list_fonds[[2]] %in% "fond_flux_sortie")) l <- c(l,"fond_flux_sortie")
    if(any(list_fonds[[2]] %in% "fond_flux_leg")) l <- c(l,"fond_flux_leg")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")

    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    if(any(list_fonds[[2]] %in% "fond_etranger"))l <- c(l,"fond_etranger")

    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")

    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    colEntree <- list_fonds[[5]]
    colSortie <- list_fonds[[6]]
    colBorder <- list_fonds[[7]]

    export_projet_qgis_fleches_saphirs(l,rep_sortie,sortie,titre1,titre2,source,colEntree,colSortie,colBorder,annee)

    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }
