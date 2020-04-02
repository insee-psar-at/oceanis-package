#' @title Add a title on a 'leaflet' map
#'
#' @description  Add a title on a 'leaflet' map
#'
#' @details Pour supprimer le titre ou le sous-titre, reexecutez la fonction add_titre
#' en laissant le parametre \code{titre} ou \code{sousTitre} a NULL.
#'
#' @usage add_titre(map, titre, sousTitre = NULL)
#'
#' @param map objet leaflet.
#' @param titre chaine de caracteres (character). Titre de la carte.
#' @param sousTitre chaine de caracteres (character). Sous-titre de la carte.
#' Par defaut a NULL.
#'
#' @return Retourne un objet de type leaflet.
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},
#' \link{leaflet_typo},}
#'
#' \code{\link{leaflet_oursins}, \link{leaflet_joignantes},
#' \link{leaflet_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#' data("depm")
#'
#' # Ronds proportionnels
#' map <- leaflet_ronds(data = donnees_monoloc, fondMaille = depm, idData = "COD_DEP",
#' varVolume = "POP_2015")
#' map <- add_titre(map = map,
#' titre = "Population des departements de France metropolitaine en 2015")
#' \donttest{
#'  map
#' }
#'
#' @import leaflet
#' @rawNamespace import(shiny, except = runExample)
#'
#' @export add_titre
#'
add_titre <-
function(map, titre, sousTitre=NULL)
  {
    msg_error1<-msg_error2 <- NULL

    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(titre)) if(any(class(titre)!="character")) msg_error2 <- "Le titre doit etre de type caractere / "

    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }

    tag.map.title <- tags$style(HTML("
                                       .leaflet-control.map-title {
                                       left: 8%;
                                       text-align: center;
                                       background: rgba(255,255,255,0.75);
                                       font-weight: bold;
                                       font-size: 16px;
                                       }
                                       "))
    #position: fixed !important;
    tag.map.subtitle <- tags$style(HTML("
                                          .leaflet-control.map-subtitle {
                                          left: 15%;
                                          top: 10%;
                                          text-align: center;
                                          background: rgba(255,255,255,0.75);
                                          font-size: 14px;
                                          }
                                          "))

    if(!is.null(titre))
    {
      title <- tags$div(
        tag.map.title, HTML(titre)
      )

      if(!is.null(sousTitre))
      {
        subtitle <- tags$div(
          tag.map.subtitle, HTML(sousTitre)
        )
      }
    }

    if(any(class(map) %in% "leaflet"))
    {
      idx_titre <- NULL
      idx_soustitre <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addControl")
        {
          if(any(map$x$calls[[i]]$args[[4]]=="map-title")) idx_titre <- i
          if(any(map$x$calls[[i]]$args[[4]]=="map-subtitle")) idx_soustitre <- i
        }
      }

      if(!is.null(titre))
      {
        if(is.null(idx_titre)) # on n'a pas encore mis de titre
        {
          map <- addControl(map = map, title, position = "topleft", className="map-title", layerId = 1)
        }else # un titre existe deja, on le remplace
        {
          ancien_titre <- map$x$calls[[idx_titre]]$args[[1]]
          map$x$calls[[idx_titre]]$args[[1]] <- paste0(
            substr(ancien_titre,1,str_locate(ancien_titre,"</style>\n")[2]+2),
            titre,
            substr(ancien_titre,str_locate(ancien_titre,"\n</div>")[1],nchar(ancien_titre))
          )
        }

        if(is.null(idx_soustitre)) # on n'a pas mis de sous-titre
        {
          if(!is.null(sousTitre))
          {
            map <- addControl(map = map, subtitle, position = "topleft", className="map-subtitle", layerId = 2)
          }
        }else # un sous-titre existe deja
        {
          if(!is.null(sousTitre)) # on le remplace
          {
            ancien_soustitre <- map$x$calls[[idx_soustitre]]$args[[1]]
            map$x$calls[[idx_soustitre]]$args[[1]] <- paste0(
              substr(ancien_soustitre,1,str_locate(ancien_soustitre,"</style>\n")[2]+2),
              sousTitre,
              substr(ancien_soustitre,str_locate(ancien_soustitre,"\n</div>")[1],nchar(ancien_soustitre))
            )
          }else # on supprime le sous-titre
          {
            map$x$calls[[idx_soustitre]]$args[[1]] <- ""
            map$x$calls[[idx_soustitre]]$args[[4]] <- ""
          }
        }
      }else # le titre est NULL, on le supprime si il existe
      {
        if(!is.null(idx_titre)) # un titre existe deja, on le supprime
        {
          map$x$calls[[idx_titre]]$args[[1]] <- ""
          map$x$calls[[idx_titre]]$args[[4]] <- ""
          if(!is.null(idx_soustitre))
          {
            map$x$calls[[idx_soustitre]]$args[[1]] <- ""
            map$x$calls[[idx_soustitre]]$args[[4]] <- ""
          }
        }
      }
    }else if(any(class(map) %in% "leaflet_proxy"))
    {
      removeControl(map,layerId = 1)
      map <- addControl(map = map, title, position = "topleft", className="map-title", layerId = 1)

      if(!is.null(sousTitre))
      {
        removeControl(map,layerId = 2)
        map <- addControl(map = map, subtitle, position = "topleft", className="map-subtitle", layerId = 2)
      }
    }
    return(map)
  }
