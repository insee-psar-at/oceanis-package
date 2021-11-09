export_qgis_oursins <-
function(map,cheminDossier,nomFichier,titre1="",titre2="",source="")
  {
    sortie <- nomFichier
    rep_sortie <- cheminDossier
    files <- paste0(rep_sortie,"/",sortie,".qgs")

    list_fonds <- extract_fond_leaflet_oursins(map)

    dir.create(paste0(rep_sortie,"/layers"),showWarnings = F)
    
    for(i in 1:length(list_fonds[[1]]))
    {
      suppressWarnings(st_write(list_fonds[[1]][[i]], paste0(rep_sortie,"/layers/",list_fonds[[2]][[i]],".shp"), delete_dsn = TRUE, quiet = TRUE))
    }

    annee <- format(Sys.time(), format = "%Y")

    l <- c()
    if(any(list_fonds[[2]] %in% "fond_flux")) l <- c(l,"fond_flux")
    if(any(list_fonds[[2]] %in% "fond_maille")) l <- c(l,"fond_maille")

    if(any(list_fonds[[2]] %in% "fond_france")) l <- c(l,"fond_france")
    if(any(list_fonds[[2]] %in% "fond_pays"))l <- c(l,"fond_pays")
    if(any(list_fonds[[2]] %in% "fond_etranger"))l <- c(l,"fond_etranger")

    if(any(list_fonds[[2]] %in% "fond_territoire")) l <- c(l,"fond_territoire")
    if(any(list_fonds[[2]] %in% "fond_departement")) l <- c(l,"fond_departement")
    if(any(list_fonds[[2]] %in% "fond_region")) l <- c(l,"fond_region")

    if(is.null(titre1)) titre1 <- list_fonds[[3]]
    if(is.null(source)) source <- list_fonds[[4]]
    epaisseur <- list_fonds[[5]]
    colTrait <- list_fonds[[6]]

    export_projet_qgis_oursins(l,rep_sortie,sortie,titre1,titre2,source,epaisseur,colTrait,annee)

    message(simpleMessage(paste0("[INFO] Le projet .qgs se trouve dans ",files)))
  }
