#' @title Returns a palette colors of the graphic chart of INSEE
#'
#' @description Returns a palette colors of the graphic chart of INSEE.
#'
#' @details Les couleurs de palettes préfixées par "Insee_" sont celles de la charte graphique
#' INSEE. La fonction affiche_palette(nomPalette) permet de visualiser les couleurs de chaque
#' palette disponible.
#' Les modalités pour l'argument 'nomPalette' sont "Insee_Rouge", "Insee_Jaune", Insee_Bleu",
#' "Insee_Turquoise", "Insee_Vert", "Insee_Violet" et "Insee_Gris" pour les palettes de la
#' charte graphique INSEE, "defaut", "Turquoise_Neg", "Vert_Neg", "Violet_Neg" et "Gris_Neg"
#' pour les palettes hors charte.
#' 
#' La palette "defaut" reprend les couleurs de la palette RdYlBu de ColorBrewer.
#'
#' Seules les palettes "Insee_Rouge" et "Insee_Jaune" proposent des palettes pour des valeurs
#' négatives et positives. Pour ces deux palettes, la palette "Insee_Bleu" est utilisée
#' pour représenter les valeurs négatives. Cette dernière peut également être utilisée seule 
#' pour des valeurs uniquement positives ou négatives.
#'
#' La fonction permet de visualiser tous les dégradés possibles pour une palette donnée.
#'
#' La liste des valeurs hexadécimales de toutes les palettes est dipsonible en chargeant
#' les données suivantes : data("palettes_insee")
#'
#' @usage affiche_palette(nomPalette = "defaut")
#'
#' @param nomPalette chaine de caracteres (character). A choisir parmi
#' "defaut", "Insee_Rouge", "Insee_Jaune", "Insee_Bleu", "Insee_Turquoise",
#' "Insee_Vert", "Insee_Violet", "Insee_Gris", "Turquoise_Neg", "Vert_Neg",
#' "Violet_Neg" ou "Gris_Neg".
#'
#' @return Affiche la palette dans l'onglet Plots.
#'
#' @seealso \code{\link{set_couleur_classes}}
#'
#' @references Un convertisseur de couleurs pour visualiser une couleur a
#' partir de son nom, son code hexadecimal ou RGB :
#' http://www.proftnj.com/RGB3.htm
#' 
#' ColorBrewer 2.0 :
#' https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=10
#'
#' @keywords documentation
#'
#' @examples
#'
#' affiche_palette(nomPalette = "defaut")
#'
#' @export affiche_palette
#'
affiche_palette <- function(nomPalette = "defaut"){
  
  msg_error1 <- NULL
  
  if(!is.null(nomPalette)) if(!nomPalette %in% c("defaut", "Insee_Rouge", "Insee_Jaune", "Insee_Bleu", "Insee_Turquoise", "Insee_Vert", "Insee_Violet", "Insee_Gris", "Turquoise_Neg", "Vert_Neg", "Violet_Neg", "Gris_Neg")) msg_error1 <- "Le style de palette est a choisir parmi 'defaut', 'Insee_Jaune', 'Insee_Rouge', 'Insee_Bleu', 'Insee_Turquoise', 'Insee_Vert', 'Insee_Violet', 'Insee_Gris', 'Turquoise_Neg', 'Vert_Neg', 'Violet_Neg' ou 'Gris_Neg' / "
  
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