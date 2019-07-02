\name{donnees}
\alias{com_dep_13_30_83_84}
\alias{donnees_a_facon}
\alias{donnees_monoloc}
\alias{donnees_biloc}
\alias{donnees_biloc_saphirs}
\alias{depm}
\alias{regm}
\alias{fram}
\docType{data}
\title{
	Examples of data
}
\description{
	Data tables for examples.
	
	- \code{com_dep_13_30_83_84} for \code{zonage_a_facon}
	
	- \code{donnees_a_facon} for \code{zonage_a_facon}
	
	- \code{donnees_monoloc} for proportional circles, chroropleth and typology
	
	- \code{donnees_biloc} for propotionnal arrows
	
	- \code{donnees_biloc_saphirs} for sapphire arrows
	
	- \code{depm, regm, fram} for the meshes and contours of the maps
}
\usage{
data("com_dep_13_30_83_84")

data("donnees_a_facon")

data("donnees_monoloc")

data("donnees_biloc")

data("donnees_biloc_saphirs")

data("depm")

data("regm")

data("fram")
}
\format{
  \code{com_dep_13_30_83_84}
  Un data frame de 792 observations et 3 variables.
  \describe{
    \item{\code{CODE}}{vecteur caracteres}
    \item{\code{LIBELLE}}{vecteur caracteres}
    \item{\code{geometry}}{sfc_GEOMETRY}
  }
  
   Un data frame de 281 observations et 3 variables.
  \describe{
    \item{\code{DEPCOM}}{vecteur caracteres}
    \item{\code{ZE2010}}{vecteur caracteres}
    \item{\code{LIB_ZE2010}}{vecteur caracteres}
  }
  
  \code{donnees_monoloc}
    Un data frame de 96 observations et 6 variables.
  \describe{
    \item{\code{COD_DEP}}{vecteur caracteres}
    \item{\code{LIB_DEP}}{vecteur caracteres}
    \item{\code{POP_2010}}{vecteur numerique}
    \item{\code{POP_2015}}{vecteur numerique}
    \item{\code{VAR_AN_MOY}}{vecteur numerique}
    \item{\code{REG}}{vecteur caracteres}
  }
  
  \code{donnees_biloc}
    Un data frame de 378 observations et 3 variables.
  \describe{
    \item{\code{REG_DEPART}}{vecteur caracteres}
    \item{\code{REG_ARRIVEE}}{vecteur caracteres}
    \item{\code{MIGR}}{vecteur numerique}
  }
  
  \code{donnees_biloc_saphirs}
    Un data frame de 21 observations et 3 variables.
  \describe{
    \item{\code{REG_DEPART}}{vecteur caracteres}
    \item{\code{REG_ARRIVEE}}{vecteur caracteres}
    \item{\code{MIGR}}{vecteur numerique}
  }
  
  \code{depm}
    Un data frame de 96 observations et 5 variables.
  \describe{
    \item{\code{CODE}}{vecteur caracteres}
    \item{\code{LIBELLE}}{vecteur caracteres}
	\item{\code{REG}}{vecteur caracteres}
	\item{\code{SURF}}{vecteur numerique}
    \item{\code{geometry}}{sfc_GEOMETRY}
  }
  
  \code{regm}
    Un data frame de 13 observations et 4 variables.
  \describe{
    \item{\code{CODE}}{vecteur caracteres}
    \item{\code{LIBELLE}}{vecteur caracteres}
	\item{\code{SURF}}{vecteur numerique}
    \item{\code{geometry}}{sfc_GEOMETRY}
  }
  
  \code{fram}
    Un data frame de 1 observation et 4 variables.
  \describe{
    \item{\code{CODE}}{vecteur caracteres}
    \item{\code{LIBELLE}}{vecteur caracteres}
	\item{\code{SURF}}{vecteur numerique}
    \item{\code{geometry}}{sfc_GEOMETRY}
  }
}
\examples{
data(com_dep_13_30_83_84)
data(donnees_a_facon)
data(donnees_monoloc)
data(donnees_biloc)
data(donnees_biloc_saphirs)
data(depm)
data(regm)
data(fram)
}
\keyword{datasets}
