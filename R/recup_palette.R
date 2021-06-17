#' @title Returns a palette of the graphic chart of INSEE
#'
#' @description Returns a palette of the graphic chart of INSEE.
#'
#' @details Les palettes de couleurs disponibles sont celles de la Charte Graphique
#' INSEE. En exécutant la fonction affiche_palette(nomPalette), il est possible de
#' visualiser les couleurs de chaque palette disponible. Les modalités pour
#' l'argument 'nomPalette' sont "Bleu_Jaune", "Bleu_Rouge", "Bleu", "Jaune",
#' "Gris", "Turquoise", "Vert" et "Violet".
#'
#' La fonction renvoie une liste d'un vecteur composé du nom de la palette au format
#' nomPalette_xN_yP et des valeurs hexadécimales de la palette.
#'
#' La liste des valeurs hexadécimales de toutes les palettes est disponible en
#' chargeant les données suivantes : data("palettes_insee").
#' 
#' Il est obligatoire de spécifier un nombre de classes négatives ou positives
#' supérieur à 0.
#'
#' @usage recup_palette(stylePalette, nbNeg = 0, nbPos = 0)
#'
#' @param stylePalette chaine de caracteres (character). A choisir parmi
#' "Bleu_Jaune", "Bleu_Rouge", "Bleu", "Jaune", "Gris", "Turquoise", "Vert"
#' ou "Violet".
#' @param nbNeg numeric. Nombre de classes negatives. Par defaut 0.
#' @param nbPos numeric. Nombre de classes positives. Par defaut 0.
#'
#' @return Retourne une liste d'un vecteur caracteres.
#'
#' @seealso \code{\link{set_couleur_classes}}
#'
#' @references Un convertisseur de couleurs pour visualiser une couleur a
#' partir de son nom, son code hexadecimal ou RGB :
#' http://www.proftnj.com/RGB3.htm
#'
#' @keywords documentation
#'
#' @examples
#'
#' recup_palette(stylePalette = "Bleu_Jaune", nbNeg = 3, nbPos = 3)
#'
#' # $Bleu_Jaune_3N3P
#' # [1] "#0F417A" "#286AC7" "#8DB0E1" "#FBD616" "#FFC300" "#E89E0B"
#'
#' @export recup_palette
#'
recup_palette <-
function(stylePalette, nbNeg = 0, nbPos = 0)
{
  msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6 <- NULL
  
  if(!is.null(stylePalette)) if(any(class(stylePalette)!="character")) msg_error1 <- "Le style de palette est a choisir parmi 'Bleu_Jaune', 'Bleu_Rouge', 'Bleu', 'Jaune', 'Gris', 'Turquoise', 'Vert' ou 'Violet' / "
  if(nbNeg < 0) msg_error2 <- "Le nombre de classes negative doit etre superieur ou egal a zero / "
  if(nbPos < 0) msg_error3 <- "Le nombre de classes positive doit etre superieur ou egal a zero / "
  if(nbNeg == 0 & nbPos == 0) msg_error4 <- "Le nombre de classes positive ou negative doit etre superieur a zero / "
  if(nbNeg > 6 | nbPos > 6) msg_error5 <- "Le nombre de classes positive ou negative doit etre inferieur ou egal a 6 / "
  if(!is.null(stylePalette)) if(!stylePalette %in% c("Bleu_Jaune", "Bleu_Rouge", "Bleu", "Jaune", "Gris", "Turquoise", "Vert", "Violet")) msg_error6 <- "Le style de palette est a choisir parmi 'Bleu_Jaune', 'Bleu_Rouge', 'Bleu', 'Jaune', 'Gris', 'Turquoise', 'Vert' ou 'Violet' / "
  
  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),!is.null(msg_error6)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6)))
  }
  
  col_palette <- palettes_insee[which(names(palettes_insee) == paste0(stylePalette,"_",nbNeg,"N",nbPos,"P"))]
  
  if(length(col_palette) == 0)
  {
    stop(simpleError("La palette choisie n'est pas adaptee aux classes des donnees."))
  }
  
  return(col_palette)
}
