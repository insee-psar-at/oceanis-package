\name{distrib_variable}
\alias{distrib_variable}
\title{
	Histogram of the distribution of the class variable
}
\description{
	Displays a histogram of the distribution of the class variable (ratio, part, evolution ...)
	based on the specified method, number of classes or breaks.
}
\usage{
distrib_variable(data, varRatio, methode = "kmeans", nbClasses = 3, bornes = NULL)
}
\arguments{
  \item{data}{
	tableau de donnees (data.frame).
}
  \item{varRatio}{
	chaine de caracteres (character). Variable des classes de la table.
}
  \item{methode}{
	chaine de caracteres (character). A choisir parmi "kmeans" (par defaut), "fisher", "jenks" ou "quantile".
}
  \item{nbClasses}{
	valeur numerique (numeric). Nombre de classes. Par defaut 3 classes.
}
  \item{bornes}{
	vecteur de valeurs numeriques (numeric). Par defaut a NULL.
}
}
\details{
	Si les bornes sont renseignees, aucune methode est appliquee. Elle est consideree comme manuelle.
	
	Si les donnees a representer possedent des valeurs negatives et positives, alors une borne de classe a zero est geree automatiquement pour les methodes "kmeans", "fisher" et "jenks". La distribution des valeurs dans les classes, effectuee par la methode specifiee, est independante entre les valeurs negatives et les valeurs positives.
	
	La methode "quantile" ne gere pas de borne a zero.

	La fonction \code{ggplot2::ggplot} est utilisee pour la creation de l'histogramme.
}
\value{
	Retourne un objet de type \code{ggplot}
	
	Affiche l'histogramme dans le plot.
}
\seealso{
	\code{\link[ggplot2]{ggplot}}
}
\examples{
data("donnees_monoloc")

distrib_variable(data = donnees_monoloc ,varRatio = "VAR_AN_MOY", nbClasses = 4)
}
\keyword{documentation}
