#' @title Returns a palette colors of the graphic chart of INSEE
#'
#' @description Returns a palette colors of the graphic chart of INSEE.
#'
#' @details Les palettes de couleurs disponibles sont celles de la Charte Graphique
#' INSEE. En exécutant la fonction affiche_palette(nomPalette), il est possible de
#' visualiser les couleurs de chaque palette disponible. Les modalités pour
#' l'argument 'nomPalette' sont "Bleu_Jaune", "Bleu_Rouge", "Bleu", "Jaune",
#' "Gris", "Turquoise", "Vert" et "Violet".
#'
#' La fonction permet de visualiser les couleurs de chaque palette.
#'
#' La liste des valeurs hexadécimales de toutes les palettes est dipsonible en
#' chargeant les données suivantes : data("palettes_insee")
#'
#' @usage affiche_palette(nomPalette = "Bleu_Jaune")
#'
#' @param nomPalette chaine de caracteres (character). A choisir parmi
#' "Bleu_Jaune", "Bleu_Rouge", "Bleu", "Jaune", "Gris", "Turquoise", "Vert"
#' ou "Violet".
#'
#' @return Affiche la palette dans l'onglet Plots.
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
#' affiche_palette(nomPalette = "Bleu_Jaune")
#'
#' @export affiche_palette
#'
affiche_palette <- function(nomPalette = "Bleu_Jaune"){
  
  msg_error1 <- NULL
  
  if(!is.null(nomPalette)) if(!nomPalette %in% c("Bleu_Jaune", "Bleu_Rouge", "Bleu", "Jaune", "Gris", "Turquoise", "Vert", "Violet")) msg_error1 <- "Le style de palette est a choisir parmi 'Bleu_Jaune', 'Bleu_Rouge', 'Bleu', 'Jaune', 'Gris', 'Turquoise', 'Vert' ou 'Violet' / "
  
  if(any(!is.null(msg_error1)))
  {
    stop(simpleError(paste0(msg_error1)))
  }
  
  names_palette <- substr(names(palettes_insee), 1, nchar(names(palettes_insee))-5)
  
  colorlist <- palettes_insee[which(names_palette == nomPalette)]
  
  n <- lengths(colorlist)
  
  nr <- length(colorlist)
  nc <- max(n)
  ylim <- c(0, nr)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(-1, nc), ylim = ylim, type = "n", axes = FALSE, 
       bty = "n", xlab = "", ylab = "")
  for (i in 1:nr) {
    nj <- n[i]
    shadi <- colorlist[[i]]
    rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj, 
         ytop = i - 0.2, col = shadi, border = "light grey")
  }
  text(rep(-0.1, nr), (1:nr) - 0.6, labels = names(colorlist), xpd = TRUE, 
       adj = 1)
  
}