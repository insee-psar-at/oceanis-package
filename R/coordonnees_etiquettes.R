#' @title Data table of labels
#'
#' @description Returns a data table for formatting labels and their position for a map in
#' plot.
#'
#' @details Le tableau des etiquettes contient le code, le libelle a afficher, les
#' coordonnees x et y du centroid de la maille donc des etiquettes, la taille,
#' le style et la couleur de la police. Le style de police (colonne
#' \code{FONT}) est un entier : 1 (normal), 2 (gras, par defaut), 3 (italique)
#' et 4 (gras italique).
#'
#' Ce tableau peut etre modifie pour changer le style d'une ou plusieurs
#' etiquettes mais aussi leur position (x et y) pour eviter le chevauchement.
#'
#' Pour afficher la carte avec les etiquettes formatees, il faut passer ce
#' tableau dans le parametre "etiquettes" d'une fonction plot.
#'
#' @usage coordonnees_etiquettes(fondMaille, listeCode)
#'
#' @param fondMaille objet sf. Fond de carte.
#' @param listeCode vecteur de caracteres (character). Liste des codes de la
#' maille a afficher sur la carte.
#'
#' @return Retourne un objet data.frame.
#'
#' @seealso \code{\link{plot_ronds}, \link{plot_classes},
#' \link{plot_ronds_classes}, \link{plot_classes_ronds}, \link{plot_typo},}
#'
#' \code{\link{plot_oursins}, \link{plot_joignantes}, \link{plot_saphirs}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#' data("depm")
#'
#' etiquettes <- coordonnees_etiquettes(fondMaille = depm,
#' listeCode = c("06","13","31","33","44","67","69","59","75"))
#' etiquettes$LIBELLE <- c("Nice","Marseille","Toulouse","Bordeaux","Nantes",
#' "Lille","Strasbourg","Lyon","Paris")
#' etiquettes[etiquettes$CODE=="75","TAILLE"] <- 1.3
#'
#' # Ronds proportionnels sur une analyse en classes
#' fond_ronds <- plot_classes_ronds(data = donnees_monoloc, fondMaille = depm,
#' idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4,
#' titreLegRonds = "POP_2015", titreLegClasses = "VAR_AN_MOY",
#' xLegClasses = 1150000, yLegClasses = 6600000, etiquettes = etiquettes)
#'
#' @import sf
#'
#' @export coordonnees_etiquettes
#'
coordonnees_etiquettes <-
function(fondMaille,listeCode)
  {
    options("stringsAsFactors"=FALSE)

    # Verification des parametres

    msg_error1<-msg_error2 <- NULL

    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error1 <- "Le fond de maille doit etre un objet sf / "
    if(any(class(listeCode)!="character")) msg_error2 <- "La liste de code doit etre un vecteur de type caractere / "

    if(any(!is.null(msg_error1),!is.null(msg_error2)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2)))
    }

    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(any(Encoding(fondMaille$LIBELLE) %in% "latin1")){
      fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","UTF-8")
    }

    fondEtiquettes <- fondMaille[fondMaille$CODE %in% listeCode,c("CODE","LIBELLE")]
    if(nrow(fondEtiquettes)==0) stop(simpleError("Aucun CODE a etiqueter n'a ete trouve dans fondMaille."))
    suppressWarnings(centroid <- st_coordinates(st_centroid(st_geometry(fondEtiquettes))))
    tableEtiquettes <- as.data.frame(fondEtiquettes)[,c("CODE","LIBELLE")]
    tableEtiquettes <- cbind(tableEtiquettes,centroid)
    tableEtiquettes$TAILLE <- 0.9
    tableEtiquettes$FONT <- 2
    tableEtiquettes$COL <- "black"

    return(tableEtiquettes)
  }
