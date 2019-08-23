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
        if(methode!="manuel")
          suppressWarnings(bornes_analyse <- classIntervals(donnees[,variable_classe],as.numeric(max_classes),style=methode,rtimes=10,intervalClosure="left"))
        else
          suppressWarnings(bornes_analyse <- classIntervals(donnees[,variable_classe],as.numeric(max_classes),style="kmeans",rtimes=10,intervalClosure="left"))
      }
      
      bornes <- bornes_analyse$brks
      
      # On separe les donnees en 2, positives ou nulles d'un cote et negatives de l'autre
      donnees_pos <- donnees[donnees[,variable_classe]>=0,variable_classe]
      donnees_neg <- donnees[donnees[,variable_classe]<0,variable_classe]
      
      # On determine les bornes + et - les plus proches de zero
      borne_pos <- min(bornes[bornes>0])
      borne_neg <- max(bornes[bornes<0])
      
      if(!is.null(stylePalette))
      {
        palette <- recup_palette(stylePalette=stylePalette)
        pal_classes_pos <- palette[[1]]
        pal_classes_neg <- palette[[2]]
      }else
      {
        pal_classes_pos <- palettePos
        pal_classes_neg <- paletteNeg
      }
      
      if(!methode %in% c("quantile","manuel"))
      {
        if(!any(bornes %in% 0)) # Si la methode kmeans genere une borne 0, c'est ideale la distribution est parfaite pour la methode choisie sinon on doit generer une borne zero
        {
          # Pour les methodes (kmeans, jenks, fisher et manuel (kmeans par defaut)), on privilegie la valeur de la borne. On transforme a zero la borne la plus proche de zero.
          
          if(length(bornes[bornes<0])==1)
          {
            nb_classes_pos <- length(bornes[bornes>0])-1
            nb_classes_neg <- 1
          }else if(length(bornes[bornes>0])==1)
          {
            nb_classes_pos <- 1
            nb_classes_neg <- length(bornes[bornes<0])-1
          }else # On a plus d'une borne - et plus d'une borne +
          {
            # On cherche la valeur la plus proche de zero a remplacer par zero
            if(length(min(abs(bornes)))==1) # il n'y a qu'une valeur plus proche de zero
            {
              bornes[abs(bornes)==min(abs(bornes))] <- 0
              
              nb_classes_pos <- length(bornes[bornes>0])
              nb_classes_neg <- length(bornes[bornes<0])
            }else # il y a 2 valeurs plus proches de zero (-2 et 2 par exemple)
            {
              # On garde la borne ayant les plus d'effectifs
              # On compte les effectifs dans chacune des classes autour de zero
              nb_eff_pos <- length(donnees_pos[0 <= donnees_pos & donnees_pos < borne_pos])
              nb_eff_neg <- length(donnees_neg[borne_neg < donnees_neg & donnees_neg < 0])
              
              if(nb_eff_pos > nb_eff_neg)
              {
                # On conserve la borne + et on enleve une classe -
                nb_classes_pos <- length(bornes[bornes>0])
                nb_classes_neg <- length(bornes[bornes<0])-1
              }else if (nb_eff_pos < nb_eff_neg)
              {
                # On conserve la borne - et on enleve une classe +
                nb_classes_pos <- length(bornes[bornes>0])-1
                nb_classes_neg <- length(bornes[bornes<0])
              }else
              {
                # Les valeurs et les effectifs sont identiques, on decide de conserver la borne +.
                bornes[bornes==max(bornes[bornes<0])] <- 0
                
                nb_classes_pos <- length(bornes[bornes>0])
                nb_classes_neg <- length(bornes[bornes<0])-1
              }
            }
          }
          
          if(nb_classes_pos>1)
          {
            suppressWarnings(bornes_analyse_pos <- classIntervals(donnees_pos,nb_classes_pos,style=methode,rtimes=10,intervalClosure="left")$brks)
            bornes_analyse_pos <- bornes_analyse_pos[-1]
          }else # nb_classes_pos == 1
          {
            bornes_analyse_pos <- max(donnees_pos)
          }
          
          if(nb_classes_neg>1)
          {
            suppressWarnings(bornes_analyse_neg <- classIntervals(donnees_neg,nb_classes_neg,style=methode,rtimes=10,intervalClosure="left")$brks)
            bornes_analyse_neg <- bornes_analyse_neg[-length(bornes_analyse_neg)]
          }else # nb_classes_neg == 1
          {
            bornes_analyse_neg <- min(donnees_neg)
          }
          
          bornes <- unique(c(bornes_analyse_neg,0,bornes_analyse_pos))
          bornes <- sort(bornes, decreasing = TRUE)
        }
        
        pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
        pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
        
      }else if(methode!="manuel")# Pour la methode des quantiles, on ne gere pas la borne zero pour ne pas fausser l'equi-distribution des effectifs dans les classes.
      {
        message(simpleMessage("La methode des quantiles ne permet pas de gerer la borne a 0. Vous pouvez passer en mode manuel pour modifier les bornes des classes."))
        
        # On doit tout de meme determiner la couleur de la classe autour de zero
        
        if(length(bornes[bornes<0])==1) # il n'y a qu'une seule valeur - dc on conserve la classe -
        {
          col_classe <- "NEG"
        }else if(length(bornes[bornes>0])==1) # il n'y a qu'une seule valeur + dc on conserve la classe +
        {
          col_classe <- "POS"
        }else # On a plus d'une borne - et plus d'une borne +
        {
          # On compte les effectifs dans chacune des classes autour de zero
          nb_eff_pos <- length(donnees_pos[0 <= donnees_pos & donnees_pos < borne_pos])
          nb_eff_neg <- length(donnees_neg[borne_neg < donnees_neg & donnees_neg < 0])
          
          if(nb_eff_pos > nb_eff_neg)
          {
            col_classe <- "POS"
          }else if(nb_eff_pos < nb_eff_neg)
          {
            col_classe <- "NEG"
          }else # les effectifs sont identiques, on compare les valeurs
          {
            if(borne_pos-abs(borne_neg)>0)
            {
              col_classe <- "POS"
            }else if(borne_pos-abs(borne_neg)<0)
            {
              col_classe <- "NEG"
            }else # il y a tjs egalite, on conserve par defaut la classe +
            {
              col_classe <- "POS"
            }
          }
        }
        
        if(col_classe=="POS")
        {
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])-1]
        }else #col_classe=="NEG"
        {
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+2):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
        }
        
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }else # methode "manuel"
      {
        if(any(bornes %in% 0)) # Si il y a le 0 de present
        {
          pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
          pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
          pal_classes <- c(pal_classes_pos,pal_classes_neg)
        }else # Si il n'y a pas de 0, on determine la couleur de la classe autour de 0
        {
          if(length(bornes[bornes<0])==1) # il n'y a qu'une seule valeur - dc on conserve la classe -
          {
            col_classe <- "NEG"
          }else # Sinon on garde la classe + par defaut
          {
            col_classe <- "POS"
          }
          
          if(col_classe=="POS")
          {
            pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+1):length(pal_classes_pos)]
            pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])-1]
          }else #col_classe=="NEG"
          {
            pal_classes_pos <- pal_classes_pos[(length(pal_classes_pos)-length(bornes[bornes>0])+2):length(pal_classes_pos)]
            pal_classes_neg <- pal_classes_neg[1:length(bornes[bornes<0])]
          }
          
          pal_classes <- c(pal_classes_pos,pal_classes_neg)
        }
      }
    }
    
    if(min(bornes_analyse$brks)>=0) # Si +
    {
      bornes <- c(bornes_analyse$brks[bornes_analyse$brks>=0])
      bornes <- sort(bornes, decreasing = TRUE)
      
      if(!is.null(stylePalette))
      {
        palette <- recup_palette(stylePalette=stylePalette)
        pal_classes_pos <- palette[[1]]
      }else
      {
        pal_classes_pos <- palettePos
      }
      
      if(length(bornes)>=(length(pal_classes_pos)+1))
      {
        pal_classes <- pal_classes_pos
      }else
      {
        pal_classes <- pal_classes_pos[-c(1:(length(pal_classes_pos)-length(bornes)+1))] # On enleve les couleurs fonces inutiles
      }
    }
    
    if(max(bornes_analyse$brks)<0) # Si -
    {
      bornes <- c(bornes_analyse$brks[bornes<0])
      bornes <- sort(bornes, decreasing = TRUE)
      
      if(!is.null(stylePalette))
      {
        palette <- recup_palette(stylePalette=stylePalette)
        pal_classes_neg <- palette[[2]]
      }else
      {
        pal_classes_neg <- paletteNeg
      }
      
      if(length(bornes)>=(length(pal_classes_neg)+1))
      {
        pal_classes <- pal_classes_neg
      }else
      {
        pal_classes <- pal_classes_neg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
      }
    }
    return(list(bornes,pal_classes))
  }
