\name{calcul_variable_classes}
\alias{calcul_ratio}
\alias{calcul_tx_evol_global}
\alias{calcul_tx_evol_ann_moy}
\alias{calcul_part_ens}
\alias{calculette}

\title{
	Calculating a class variable
}
\description{
	Add to a data table a column whose values can be represented in a class analysis (ratio, part, evolution ...).

	The calculation is based on volume data (from the population, for example).
}
\usage{
calcul_ratio(data, var1, var2)

calcul_tx_evol_global(data, var1, var2)

calcul_tx_evol_ann_moy(data, var1, var2, nbAnnees)

calcul_part_ens(data, var)

calculette(data, formule = NULL)
}
\arguments{
  \item{data}{
	tableau de donnees (data.frame).
}
  \item{var1}{
	chaine de caracteres (character). Variable en volume de la table.
}
  \item{var2}{
	chaine de caracteres (character). Variable en volume de la table.
}
  \item{nbAnnees}{
	nombre (numeric). Nombre d'annees appliquable au taux d'evolution annuel moyen.
}
  \item{var}{
	chaine de caracteres (character). Variable en volume de la table.
}
  \item{formule}{
	chaine de caracteres (character). Formule libre pour le calcul d'une variable de classes. Seuls les caracteres + - * / ^ ( ) sont acceptes.
}
}
\details{
	Les formules utilisees sont :
	\describe{
		\item{ratio}{\code{(data[,var1]/data[,var2])*100}}
		\item{tx_evol_global}{\code{((data[,var2]-data[,var1])/data[,var1])*100}}
		\item{tx_evol_ann_moy}{\code{((data[,var2]/data[,var1])^(1/nbAnnees)-1)*100}}
		\item{part_ens}{\code{(data[,var]/sum(data[,var],na.rm = TRUE))*100}}
	}
}
\value{
	Retourne un objet data.frame.
}
\examples{
data("donnees_monoloc")

ratio <- calcul_ratio(data = donnees_monoloc, var1 = "POP_2010", var2 = "POP_2015")
teg <- calcul_tx_evol_global(data = donnees_monoloc, var1 = "POP_2010", var2 = "POP_2015")
team <- calcul_tx_evol_ann_moy(data = donnees_monoloc, var1 = "POP_2010", var2 = "POP_2015",
nbAnnees = 5)
part <- calcul_part_ens(data = donnees_monoloc, var = "POP_2015")
}
\keyword{documentation}
