#' @title Returns a palette of the graphic chart of INSEE
#'
#' @description Returns a palette of the graphic chart of INSEE.
#'
#' @details Les palettes proposees sont celles utilisees dans les publications Insee.
#' Seule la palette par defaut propose des couleurs supplementaires par rapport
#' a la palette utilisee dans la publication InsesFlash.
#'
#' La fonction renvoie une liste de deux vecteurs. Le premier element de la
#' liste correspond aux couleurs des valeurs positives et le deuxieme aux
#' couleurs des valeurs negatives.
#'
#' Les couleurs sont classees du plus fonce au plus clair pour les palettes des
#' valeurs positives et du plus clair au plus fonce pour les palettes des
#' valeurs negatives.
#'
#' Le code des couleurs est le code hexadecimal.
#'
#' \itemize{ \item InseeFlash positives : "#9B231C", "#B24B1D", "#D47130",
#' "#E4A75A", "#F2CE93" \item InseeFlash negatives :
#' "#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289" \item InseeAnalyse
#' positives : "#5E2057","#853567","#8E5981","#BA97B2","#D7C0CC" \item
#' InseeAnalyse negatives : "#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289"
#' \item InseeDossier positives :
#' "#4F185E","#65317B","#9475A5","#BFA5C6","#E7D1E5" \item InseeDossier
#' negatives : "#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289" \item
#' InseePremiere positives : "#7F0029","#CC1543","#DE635B","#F79C85","#FDE3DE"
#' \item InseePremiere negatives :
#' "#ECF4D8","#CDD78C","#91B778","#549534","#005941" \item defaut positives :
#' "#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93" \item
#' defaut negatives :
#' "#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050" }
#'
#' @usage recup_palette(stylePalette)
#'
#' @param stylePalette chaine de caracteres (character). A choisir parmi
#' "InseeFlash", "InseeAnalyse", "InseeDossier", "InseePremiere" ou "defaut".
#'
#' @return Retourne une liste de deux vecteurs caracteres.
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
#' recup_palette("InseeFlash")
#'
#' #[[1]]
#' #[1] "#9B231C" "#B24B1D" "#D47130" "#E4A75A" "#F2CE93"
#' #[[2]]
#' #[1] "#ECF1FA" "#C9DAF0" "#95BAE2" "#5182B6" "#005289"
#'
#' @export recup_palette
#'
recup_palette <-
function(stylePalette)
  {
    # Palettes
    inseeDefautPos <- c("#5A0A14","#82141B","#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93") # Rouge du +fonce au + clair
    inseeDefautNeg <- c("#C9DAF0","#95BAE2","#5182B6","#005289","#003269","#001E5A","#000050") # Bleu du + clair au + fonce
    inseeFlashPos <- c("#9B231C","#B24B1D","#D47130","#E4A75A","#F2CE93")
    inseeFlashNeg <- c("#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289")
    inseeAnalysePos <- c("#5E2057","#853567","#8E5981","#BA97B2","#D7C0CC")
    inseeAnalyseNeg <- c("#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289")
    inseeDossierPos <- c("#4F185E","#65317B","#9475A5","#BFA5C6","#E7D1E5")
    inseeDossierNeg <- c("#ECF1FA","#C9DAF0","#95BAE2","#5182B6","#005289")
    inseePremierePos <- c("#7F0029","#CC1543","#DE635B","#F79C85","#FDE3DE")
    inseePremiereNeg <- c("#ECF4D8","#CDD78C","#91B778","#549534","#005941")

    if(stylePalette=="InseeFlash")
    {
      inseePos <- inseeFlashPos
      inseeNeg <- inseeFlashNeg
    }
    if(stylePalette=="InseeAnalyse")
    {
      inseePos <- inseeAnalysePos
      inseeNeg <- inseeAnalyseNeg
    }
    if(stylePalette=="InseeDossier")
    {
      inseePos <- inseeDossierPos
      inseeNeg <- inseeDossierNeg
    }
    if(stylePalette=="InseePremiere")
    {
      inseePos <- inseePremierePos
      inseeNeg <- inseePremiereNeg
    }
    if(stylePalette=="defaut")
    {
      inseePos <- inseeDefautPos
      inseeNeg <- inseeDefautNeg
    }

    return(list(inseePos,inseeNeg))
  }
