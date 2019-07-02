\name{zonage_a_facon}
\alias{zonage_a_facon}

\title{
	Creating a custom zoning
}
\description{
	Creating a custom zoning from a group of entities.
}
\usage{
zonage_a_facon(fondMaille, groupe, idMaille, idGroupe, libGroupe, fondContour = NULL,
dom = "0")
}
\arguments{
  \item{fondMaille}{
	objet sf. Fond de carte.
}
  \item{groupe}{
	tableau de donnees (data.frame) contenant un identifiant de maille, un identifiant de groupes et un libelle de groupes.
}
  \item{idMaille}{
	chaine de caractere (character). Variable identifiant la maille.
}
  \item{idGroupe}{
	chaine de caractere (character). Variable identifiant les groupes. Le choix de l'identifiant de groupes est libre ("A", "B" et "C" par exemple).
}
  \item{libGroupe}{
	chaine de caractere (character). Variable des libelles de groupes.
}
  \item{fondContour}{
	objet sf. Fond de carte. Par defaut a NULL.
}
  \item{dom}{
	chaine de caracteres (character). "0" (par defaut pour la France metropolitaine), "971" (Guadeloupe), "972" (Martinique), "973" (Guyane), "974" (La Reunion) ou "976" (Mayotte). Ce parametre permet de definir le systeme de projection de la carte.
}
}
\details{
	La fonction \code{zonage_a_facon} permet de creer son propre zonage.
	
	A partir d'un fond de maille, la fonction va regrouper certaines entites pour en former des plus grandes. Par exemple, partir d'une maille communale pour former une maille de zones d'emploi (voir examples).
	
	Il faut s'assurer que l'identifiant de maille de la table corresponde bien a l'identifiant du fond de maille.
	
	Le zonage ainsi genere peut correspondre a un zonage connu (administratif ou d'etudes) ou inconnu (personnalise).
	
	La fonction peut prendre en parametre un contour de territoire. Il s'agit du contour qui delimite l'ensemble de la maille. Il peut etre utile de le specifier si vous voulez tronquer la maille sur ce contour. Par exemple, pour representer uniquement la partie regionale des zones d'emploi.
}
\value{
	Retourne un objet de type \code{sf} (fond de carte)
}
\seealso{
	\code{\link[sf]{sf}}
	
	\code{\link[sf]{read_sf}}
}
\examples{
data("donnees_a_facon")
data("com_dep_13_30_83_84")
data("depm")

ze13etplus <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon,
idMaille = "DEPCOM", idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
fondContour = NULL, dom = "0") # sans contour
ze13 <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon,
idMaille = "DEPCOM", idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
fondContour = depm[depm$CODE=="13",], dom = "0") # avec contour

\donttest{
# affiche les ZE2010 ayant au moins une partie dans les Bouches-du-Rhone
plot(sf::st_geometry(ze13etplus), col = "transparent", border = "grey")
# affiche uniquement la partie des ZE2010 contenue dans les Bouches-du-Rhone
plot(sf::st_geometry(ze13), col = "transparent", add = TRUE)
# affiche le contour des Bouches-du-Rhone
plot(sf::st_geometry(depm[depm$CODE=="13",]), border = "red", add = TRUE)
}
}
\keyword{documentation}
