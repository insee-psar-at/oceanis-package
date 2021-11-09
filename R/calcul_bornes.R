calcul_bornes <-
function(donnees,bornes_analyse,variable_classe,max_classes,methode,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL)
  {
    if(min(bornes_analyse$brks)<0 & max(bornes_analyse$brks)>=0) # Si - et +
    {
      # La gestion de la borne zero est delicate
      # On determine d'abord le nombre de classes requise pour chaque signe
      # On ajoute la borne zero et on relance le calcul des bornes
      
      # Calcul des bornes pour la serie entiere
      if(is.null(variable_classe) | is.null(max_classes) | is.null(methode))
      {
        suppressWarnings(bornes_analyse <- classIntervals(as.numeric(donnees[,names(donnees)[sapply(donnees,is.numeric)][2]]),4,style="kmeans",rtimes=10,intervalClosure="left"))
      }else
      {
        if(methode!="manuel") suppressWarnings(bornes_analyse <- classIntervals(donnees[,variable_classe],as.numeric(max_classes),style=methode,rtimes=10,intervalClosure="left"))
      }
      
      bornes <- bornes_analyse$brks
      bornes <- sort(bornes, decreasing = T)
      
      # On separe les donnees en 2, positives ou nulles d'un cote et negatives de l'autre
      donnees_pos <- donnees[donnees[,variable_classe] >= 0,variable_classe]
      donnees_pos <- donnees_pos[!is.na(donnees_pos)]
      donnees_neg <- donnees[donnees[,variable_classe] < 0,variable_classe]
      donnees_neg <- donnees_neg[!is.na(donnees_neg)]
      
      # On determine les bornes + et - les plus proches de zero
      borne_pos <- min(bornes[bornes > 0])
      borne_neg <- max(bornes[bornes < 0])
      
      if(!methode %in% c("quantile","manuel"))
      {
        if(!any(bornes %in% 0)) # Si la methode kmeans genere une borne 0, c'est ideale la distribution est parfaite pour la methode choisie sinon on doit generer une borne zero
        {
          # Pour les methodes (kmeans, jenks ou fisher (kmeans par defaut)), on privilegie la valeur de la borne. On transforme a zero la borne la plus proche de zero.
          
          if(length(bornes[bornes<0])==1)
          {
            nb_pal_pos <- length(bornes[bornes>0])-1
            nb_pal_neg <- 1
          }else if(length(bornes[bornes>0])==1)
          {
            nb_pal_pos <- 1
            nb_pal_neg <- length(bornes[bornes<0])-1
          }else # On a plus d'une borne - et plus d'une borne +
          {
            # On cherche la valeur la plus proche de zero a remplacer par zero
            if(length(min(abs(bornes)))==1) # il n'y a qu'une valeur plus proche de zero
            {
              bornes[abs(bornes)==min(abs(bornes))] <- 0
              
              nb_pal_pos <- length(bornes[bornes>0])
              nb_pal_neg <- length(bornes[bornes<0])
            }else # il y a 2 valeurs plus proches de zero (-2 et 2 par exemple)
            {
              # On garde la borne ayant les plus d'effectifs
              # On compte les effectifs dans chacune des classes autour de zero
              nb_eff_pos <- length(donnees_pos[0 <= donnees_pos & donnees_pos < borne_pos])
              nb_eff_neg <- length(donnees_neg[borne_neg < donnees_neg & donnees_neg < 0])
              
              if(nb_eff_pos > nb_eff_neg)
              {
                # On conserve la borne + et on enleve une classe -
                nb_pal_pos <- length(bornes[bornes>0])
                nb_pal_neg <- length(bornes[bornes<0])-1
              }else if (nb_eff_pos < nb_eff_neg)
              {
                # On conserve la borne - et on enleve une classe +
                nb_pal_pos <- length(bornes[bornes>0])-1
                nb_pal_neg <- length(bornes[bornes<0])
              }else
              {
                # Les valeurs et les effectifs sont identiques, on decide de conserver la borne +.
                bornes[bornes==max(bornes[bornes<0])] <- 0
                
                nb_pal_pos <- length(bornes[bornes>0])
                nb_pal_neg <- length(bornes[bornes<0])-1
              }
            }
          }
          
          if(nb_pal_pos > 1)
          {
            suppressWarnings(bornes_analyse_pos <- classIntervals(donnees_pos,nb_pal_pos,style=methode,rtimes=10,intervalClosure="left")$brks)
            bornes_analyse_pos <- bornes_analyse_pos[-1]
          }else # nb_pal_pos == 1
          {
            bornes_analyse_pos <- max(donnees_pos)
          }
          
          if(nb_pal_neg > 1)
          {
            suppressWarnings(bornes_analyse_neg <- classIntervals(donnees_neg,nb_pal_neg,style=methode,rtimes=10,intervalClosure="left")$brks)
            bornes_analyse_neg <- bornes_analyse_neg[-length(bornes_analyse_neg)]
          }else # nb_pal_neg == 1
          {
            bornes_analyse_neg <- min(donnees_neg)
          }
          
          bornes <- unique(c(bornes_analyse_neg,0,bornes_analyse_pos))
          bornes <- sort(bornes, decreasing = TRUE)
        }
        
        if(length(bornes[bornes<0]) > 6) nb_pal_neg <- 6 else nb_pal_neg <- length(bornes[bornes<0])
        if(length(bornes[bornes>0]) > 6) nb_pal_pos <- 6 else nb_pal_pos <- length(bornes[bornes>0])
        
        if(!is.null(stylePalette))
        {
          pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg, nbPos = nb_pal_pos)[[1]]
        }else
        {
          palettePos <- palettePos[(length(palettePos)-length(bornes[bornes>0])+1):length(palettePos)]
          paletteNeg <- paletteNeg[1:length(bornes[bornes<0])]
          pal_classes <- c(paletteNeg,palettePos)
        }
        
      }else if(methode != "manuel")# Pour la methode des quantiles, on ne gere pas la borne zero pour ne pas fausser l'equi-distribution des effectifs dans les classes.
      {
        message(simpleMessage("La methode des quantiles ne permet pas de gerer la borne a 0. Vous pouvez passer en mode manuel pour modifier les bornes des classes."))
        
        if(!is.null(stylePalette))
        {
          if(min(bornes) < 0 & max(bornes) >= 0) # Si - et +
          {
            if(!0 %in% bornes)
            {
              col_classe_zero <- recup_palette(stylePalette = "Insee_Gris", nbPos = 6)[[1]][1]
              nb_pal_neg <- length(bornes[bornes < 0]) - 1
              nb_pal_pos <- length(bornes[bornes > 0]) - 1
              if(nb_pal_neg > 6) nb_pal_neg <- 6
              if(nb_pal_pos > 6) nb_pal_pos <- 6
              pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg, nbPos = nb_pal_pos)[[1]]
              pal_classes <- c(pal_classes[0:nb_pal_neg], col_classe_zero, pal_classes[(nb_pal_neg + 1):(length(pal_classes) + 1)])
              pal_classes <- pal_classes[!is.na(pal_classes)]
            }else
            {
              nb_pal_neg <- length(bornes[bornes < 0])
              nb_pal_pos <- length(bornes[bornes > 0])
              if(nb_pal_neg > 6) nb_pal_neg <- 6 
              if(nb_pal_pos > 6) nb_pal_pos <- 6
              pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg, nbPos = nb_pal_pos)[[1]] 
            }
          }
          if(min(bornes) >= 0) # Si +
          {
            nb_pal_neg <- 0
            
            if(!0 %in% bornes)
            {
              nb_pal_pos <- length(bornes[bornes > 0]) - 1
            }else
            {
              nb_pal_pos <- length(bornes[bornes > 0])
            }
            if(nb_pal_pos > 6) nb_pal_pos <- 6
            pal_classes <- recup_palette(stylePalette = stylePalette, nbPos = nb_pal_pos)[[1]]
          }
          if(max(bornes) <= 0) # Si -
          {
            nb_pal_pos <- 0
            
            if(!0 %in% bornes)
            {
              nb_pal_neg <- length(bornes[bornes < 0]) - 1
            }else
            {
              nb_pal_neg <- length(bornes[bornes < 0])
            }
            if(nb_pal_neg > 6) nb_pal_neg <- 6
            pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg)[[1]]
          }
        }else
        {
          if(min(bornes) < 0 & max(bornes) >= 0) # Si - et +
          {
            nb_pal_neg <- length(bornes[bornes<0])
            nb_pal_pos <- length(bornes[bornes>0])
            
            if(!0 %in% bornes)
            {
              palettePos <- palettePos[(length(palettePos)-length(bornes[bornes > 0]) + 1):length(palettePos)]
              paletteNeg <- paletteNeg[1:length(bornes[bornes < 0])]
              pal_classes <- c(paletteNeg,palettePos)
            }else
            {
              palettePos <- palettePos[(length(palettePos)-length(bornes[bornes > 0]) + 1):length(palettePos)]
              paletteNeg <- paletteNeg[1:length(bornes[bornes < 0])]
              pal_classes <- c(paletteNeg,palettePos)
            }
          }
          if(min(bornes) >= 0) # Si +
          {
            nb_pal_neg <- 0
            nb_pal_pos <- length(bornes[bornes>0])
            pal_classes <- palettePos[(length(palettePos)-length(bornes[bornes > 0]) + 1):length(palettePos)]
          }
          if(max(bornes) <= 0) # Si -
          {
            nb_pal_neg <- length(bornes[bornes<0])
            nb_pal_pos <- 0
            pal_classes <- paletteNeg[1:length(bornes[bornes < 0])]
          }
        }
        
      }else # méthode "manuel"
      {
        if(!0 %in% bornes) # Si il n'y a pas de 0, on détermine la couleur de la classe autour de 0
        {
          col_classe_zero <- palettes_insee[which(names(palettes_insee) == paste0("Insee_Gris_0N6P"))][[1]][1]
          nb_pal_neg <- length(bornes[bornes < 0]) - 1
          nb_pal_pos <- length(bornes[bornes > 0]) - 1
          if(nb_pal_neg > 6) nb_pal_neg <- 6 
          if(nb_pal_pos > 6) nb_pal_pos <- 6
          pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg, nbPos = nb_pal_pos)[[1]]
          pal_classes <- c(pal_classes[0:nb_pal_neg], col_classe_zero, pal_classes[(nb_pal_neg + 1):(length(pal_classes) + 1)])
          pal_classes <- pal_classes[!is.na(pal_classes)]
        }else # Si il y a le 0 de présent
        {
          nb_pal_neg <- length(bornes[bornes < 0])
          nb_pal_pos <- length(bornes[bornes > 0])
          if(nb_pal_neg > 6) nb_pal_neg <- 6 
          if(nb_pal_pos > 6) nb_pal_pos <- 6
          pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg, nbPos = nb_pal_pos)[[1]]
        }
      }
      
    }
    
    if(min(bornes_analyse$brks) >= 0) # Si +
    {
      bornes <- sort(c(bornes_analyse$brks[bornes_analyse$brks >= 0]), decreasing = T)
      nb_pal_neg <- 0
      
      if(!is.null(stylePalette))
      {
        if(!0 %in% bornes)
        {
          nb_pal_pos <- length(bornes[bornes > 0]) - 1
        }else
        {
          nb_pal_pos <- length(bornes[bornes > 0])
        }
        if(nb_pal_pos > 6) nb_pal_pos <- 6
        pal_classes <- recup_palette(stylePalette = stylePalette, nbPos = nb_pal_pos)[[1]]
      }else
      {
        if(length(bornes) >= (length(palettePos) + 1))
        {
          pal_classes <- palettePos
        }else
        {
          pal_classes <- palettePos[1:length(bornes)] # On enleve les couleurs fonces inutiles
        }
        nb_pal_pos <- length(pal_classes)
      }
    }
    
    if(max(bornes_analyse$brks) < 0) # Si -
    {
      bornes <- sort(c(bornes_analyse$brks[bornes_analyse$brks < 0]), decreasing = T)
      nb_pal_pos <- 0
      
      if(!is.null(stylePalette))
      {
        if(!0 %in% bornes)
        {
          nb_pal_neg <- length(bornes[bornes < 0]) - 1
        }else
        {
          nb_pal_neg <- length(bornes[bornes < 0])
        }
        if(nb_pal_neg > 6) nb_pal_neg <- 6
        pal_classes <- recup_palette(stylePalette = stylePalette, nbNeg = nb_pal_neg)[[1]]
      }else
      {
        if(length(bornes) >= (length(paletteNeg) + 1))
        {
          pal_classes <- paletteNeg
        }else
        {
          pal_classes <- paletteNeg[(length(paletteNeg)-length(bornes)+1):length(paletteNeg)] # On enleve les couleurs fonces inutiles
        }
        nb_pal_neg <- length(pal_classes)
      }
    }
  
    return(list(bornes,pal_classes,nb_pal_neg,nb_pal_pos))
  }
