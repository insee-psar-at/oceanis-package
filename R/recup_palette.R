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
