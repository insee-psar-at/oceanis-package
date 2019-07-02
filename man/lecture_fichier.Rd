\name{lecture_fichier}
\alias{lecture_fichier}
\title{
	Import data table
}
\description{
	Import data table with extendsion .dbf, .xls, .ods, .csv, .rds, .rda or .rdata.
}
\usage{
lecture_fichier(file)
}
\arguments{
  \item{file}{
	chaine de caracteres (character). Chemin du dossier et du fichier a importer. Inclure l'extension dans le nom du fichier.
}
}
\details{
	Il est possible d'importer des fichiers .dbf, .xls, .ods, .csv, .rds, .rda ou .rdata.
	
	Pour l'import des fichiers .xls et .ods, les donnees doivent etre sur la 1ere feuille.
	
	Pour l'import d'un fichier .csv, le separateur de variable doit etre la virgule et le separateur de texte les double quotes. Attention, les valeurs numeriques doivent etre au format anglo-saxon avec des points a la place des virgules.
}
\value{
	Retourne un objet data.frame.
}
\seealso{
	\code{\link[foreign]{read.dbf}, \link[xlsx]{read.xlsx}, \link[readODS]{read_ods}, \link[utils]{read.csv}, \link[base]{readRDS}}
}
\examples{
\donttest{
## Not run:
data <- lecture_fichier(file = system.file("data/donnees_monoloc.rda", package = "oceanis"))
## End(Not run)
}
}
\keyword{documentation}
