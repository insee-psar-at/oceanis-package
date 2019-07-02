\name{coordonnees_etiquettes}
\alias{coordonnees_etiquettes}
\title{
	Data table of labels
}
\description{
	Returns a data table for formatting labels and their position for a map in plot.
}
\usage{
coordonnees_etiquettes(fondMaille, listeCode)
}
\arguments{
  \item{fondMaille}{
	objet sf. Fond de carte.
}
  \item{listeCode}{
	vecteur de caracteres (character). Liste des codes de la maille a afficher sur la carte.
}
}
\details{
	Le tableau des etiquettes contient le code, le libelle a afficher, les coordonnees x et y du centroid de la maille donc des etiquettes, la taille, le style et la couleur de la police. Le style de police (colonne \code{FONT}) est un entier : 1 (normal), 2 (gras, par defaut), 3 (italique) et 4 (gras italique).
	
	Ce tableau peut etre modifie pour changer le style d'une ou plusieurs etiquettes mais aussi leur position (x et y) pour eviter le chevauchement.
	
	Pour afficher la carte avec les etiquettes formatees, il faut passer ce tableau dans le parametre "etiquettes" d'une fonction plot.
}
\value{
	Retourne un objet data.frame.
}
\seealso{
	\code{\link{plot_ronds}, \link{plot_classes}, \link{plot_ronds_classes}, \link{plot_classes_ronds}, \link{plot_typo},}
	
	\code{\link{plot_oursins}, \link{plot_joignantes}, \link{plot_saphirs}}
}
\examples{
data("donnees_monoloc")
data("depm")

etiquettes <- coordonnees_etiquettes(fondMaille = depm,
listeCode = c("06","13","31","33","44","67","69","59","75"))
etiquettes$LIBELLE <- c("Nice","Marseille","Toulouse","Bordeaux","Nantes",
"Lille","Strasbourg","Lyon","Paris")
etiquettes[etiquettes$CODE=="75","TAILLE"] <- 1.3

# Ronds proportionnels sur une analyse en classes
fond_ronds <- plot_classes_ronds(data = donnees_monoloc, fondMaille = depm,
idData = "COD_DEP", varVolume = "POP_2015", varRatio = "VAR_AN_MOY", nbClasses = 4,
titreLegRonds = "POP_2015", titreLegClasses = "VAR_AN_MOY",
xLegClasses = 1150000, yLegClasses = 6600000, etiquettes = etiquettes)
}
\keyword{documentation}
