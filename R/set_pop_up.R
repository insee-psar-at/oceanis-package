#' @title Modify the pop_up of 'leaflet' map's analysis
#'
#' @description Modify the pop_up of 'leaflet' map's analysis.
#'
#' @details Le nouveau texte pour le pop-up peut etre du contenu HTML.  Par exemple :
#' "<b> <font color=#2B3E50>Greater City of Paris</font> </b><br><b><font
#' color=#2B3E50>PART : </font></b><b> <font color=#2B3E50>21,2</font></b>"
#' Attention : conserver le meme ordre des pop-up dans le vecteur que celui des
#' donnees pour que les valeurs correspondent bien a l'analyse.
#'
#' Ne fonctionne que pour les ronds proportionnels, les analyses en classes et les typologies.
#'
#' Pour supprimer les pop-up, laisser le parametre a NULL.
#'
#' @usage set_pop_up(map, popup = NULL, popupRonds = NULL)
#'
#' @param map objet leaflet.
#' @param popup vecteur de caracteres (character). Peut etre du HTML pour
#' mettre en forme le texte. Par defaut a NULL.
#' @param popupRonds vecteur de caracteres (character). Peut etre du HTML pour
#' mettre en forme le texte. Par defaut a NULL.
#'
#' @return Retourne un objet de type leaflet.
#'
#' @seealso \code{\link{leaflet_ronds}, \link{leaflet_classes},
#' \link{leaflet_ronds_classes}, \link{leaflet_classes_ronds},
#' \link{leaflet_typo}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("depm")
#'
#' data("donnees_monoloc")
#'
#' # Ronds proportionnels sur une analyse en classes
#' map <- leaflet_ronds_classes(data = donnees_monoloc[donnees_monoloc$REG=="93",],
#' fondMaille = depm[depm$REG=="93",], idData = "COD_DEP", varVolume = "POP_2015",
#' varRatio = "VAR_AN_MOY", nbClasses = 4)
#'
#' new_popup <- c(paste0("<b><font color=#2B3E50>Bouches-du-Rhone</font></b><br>",
#' "<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>2 016,6</font><br>",
#' "<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,4</font>"),
#' paste0("<b><font color=#2B3E50>Alpes-Maritimes</font></b><br>",
#' "<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>1 082,4</font><br>",
#' "<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,1</font>"),
#' paste0("<b><font color=#2B3E50>Var</font></b><br>",
#' "<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>1 048,7</font><br>",
#' "<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,8</font>"),
#' paste0("<b><font color=#2B3E50>Vaucluse</font></b><br>",
#' "<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>  557,5</font><br>",
#' "<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,6</font>"),
#' paste0("<b><font color=#2B3E50>Alpes-de-Haute-Provence</font></b><br>",
#' "<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>  161,8</font><br>",
#' "<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,2</font>"),
#' paste0("<b><font color=#2B3E50>Hautes-Alpes</font></b><br>",
#' "<font color=#2B3E50>Population 2015 : </font><font color=#2B3E50>  140,9</font><br>",
#' "<font color=#2B3E50>Variation annuelle moyenne : </font><font color=#2B3E50>0,6</font>"))
#'
#' map <- set_pop_up(map = map, popupRonds = new_popup)
#' \donttest{
#'  map
#' }
#'
#' @import leaflet
#'
#' @export set_pop_up
#'
set_pop_up <-
function(map,popup=NULL,popupRonds=NULL)
  {
    msg_error1<-msg_error2<-msg_error3 <- NULL

    if(any(!any(class(map) %in% "leaflet"), !any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    if(!is.null(popup)) if(any(class(popup)!="character")) msg_error2 <- "Le parametre popup doit etre un vecteur de caractere (contenu html possible) / "
    if(!is.null(popupRonds)) if(any(class(popupRonds)!="character")) msg_error3 <- "Le parametre popupRonds doit etre un vecteur de caractere (contenu html possible) / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3)))
    }

    idx_carte_classes <- NULL
    idx_carte_ronds <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% c("addPolygons","addPolylines"))
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes","carte_classes_elargi","carte_typo"))
        {
          idx_carte_classes <- c(idx_carte_classes,i)
        }
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds","carte_ronds_elargi"))
        {
          idx_carte_ronds <- c(idx_carte_ronds,i)
        }
      }
    }

    if(length(idx_carte_classes)==2)
    {
      map$x$calls[[idx_carte_classes[1]]]$args[[5]] <- popup
      length_analyse <- length(map$x$calls[[idx_carte_classes[2]]]$args[[5]])
      map$x$calls[[idx_carte_classes[2]]]$args[[5]] <- popup[1:length_analyse]
    }else if(length(idx_carte_classes)==1)
    {
      map$x$calls[[idx_carte_classes[1]]]$args[[5]] <- popup
    }

    if(length(idx_carte_ronds)==2)
    {
      map$x$calls[[idx_carte_ronds[1]]]$args[[7]] <- popupRonds
      length_analyse <- length(map$x$calls[[idx_carte_ronds[2]]]$args[[7]])
      map$x$calls[[idx_carte_ronds[2]]]$args[[7]] <- popupRonds[1:length_analyse]
    }else if(length(idx_carte_ronds)==1)
    {
      map$x$calls[[idx_carte_ronds[1]]]$args[[7]] <- popupRonds
    }

    return(map)
  }
