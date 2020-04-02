#' @title Add a source on a 'leaflet' map
#'
#' @description  Add a source on a 'leaflet' map
#'
#' @details  Pour supprimer la source, reexecutez la fonction add_source en laissant le
#' parametre \code{source} a NULL.
#'
#' @usage add_source(map, source)
#'
#' @param map objet leaflet.
#' @param source chaine de caracteres (character). Source de la carte.
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
#' map <- add_source(map = map, source = "Source : INSEE - RP2016")
#' \donttest{
#'  map
#' }
#'
#' @import leaflet
#' @rawNamespace import(shiny, except = runExample)
#'
#' @export add_source
#'
add_source <-
function(map, source)
  {
    msg_error1<-msg_error2 <- NULL

    if (any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) if(!any(class(map) %in% "leaflet_proxy")) msg_error1 <- "La carte doit etre un objet leaflet ou leaflet_proxy / "
    if(!is.null(source)) if(any(class(source)!="character")) msg_error2 <- "La source doit etre de type caractere / "

    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }

    tag.map.sourc <- tags$style(HTML("
                                       .leaflet-control.map-sourc {
                                       position: fixed !important;
                                       left: 2%;
                                       text-align: center;
                                       font-size: 12px;
                                       }
                                       "))

    if(!is.null(source))
    {
      sourc <- tags$div(
        tag.map.sourc, HTML(source)
      )
    }

    if(any(class(map) %in% "leaflet"))
    {
      idx_source <- NULL
      for(i in 1:length(map$x$calls))
      {
        if(map$x$calls[[i]]$method %in% "addControl")
        {
          if(any(map$x$calls[[i]]$args[[4]]=="map-sourc")) idx_source <- i
        }
      }

      if(!is.null(source))
      {
        if(is.null(idx_source)) # on n'a pas encore mis de source
        {
          map <- addControl(map = map, sourc, position = "bottomright", className="map-sourc", layerId = 3)
        }else # une source existe deja, on la remplace
        {
          ancienne_source <- map$x$calls[[idx_source]]$args[[1]]
          map$x$calls[[idx_source]]$args[[1]] <- paste0(
            substr(ancienne_source,1,str_locate(ancienne_source,"</style>\n")[2]+2),
            source,
            substr(ancienne_source,str_locate(ancienne_source,"\n</div>")[1],nchar(ancienne_source))
          )
        }
      }else # la source est NULL, on la supprime si elle existe
      {
        if(!is.null(idx_source))# une source existe deja, on la supprime
        {
          map$x$calls[[idx_source]]$args[[1]] <- ""
          map$x$calls[[idx_source]]$args[[4]] <- ""
        }
      }
    }else if(any(class(map) %in% "leaflet_proxy"))
    {
      removeControl(map,layerId = 3)
      map <- addControl(map = map, sourc, position = "bottomright", className="map-sourc", layerId = 3)
    }

    return(map)
  }
