#' @title Creating a custom zoning
#'
#' @description Creating a custom zoning from a group of entities.
#'
#' @details La fonction \code{zonage_a_facon} permet de creer son propre zonage.
#'
#' A partir d'un fond de maille, la fonction va regrouper certaines entites
#' pour en former des plus grandes. Par exemple, partir d'une maille communale
#' pour former une maille de zones d'emploi (voir examples).
#'
#' Il faut s'assurer que l'identifiant de maille de la table corresponde bien a
#' l'identifiant du fond de maille.
#'
#' Le zonage ainsi genere peut correspondre a un zonage connu (administratif ou
#' d'etudes) ou inconnu (personnalise).
#'
#' La fonction peut prendre en parametre un contour de territoire. Il s'agit du
#' contour qui delimite l'ensemble de la maille. Il peut etre utile de le
#' specifier si vous voulez tronquer la maille sur ce contour. Par exemple,
#' pour representer uniquement la partie regionale des zones d'emploi.
#'
#' @usage zonage_a_facon(fondMaille, groupe, idMaille, idGroupe, libGroupe,
#' fondContour = NULL)
#'
#' @param fondMaille objet sf. Fond de carte.
#' @param groupe tableau de donnees (data.frame) contenant un identifiant de
#' maille, un identifiant de groupes et un libelle de groupes.
#' @param idMaille chaine de caractere (character). Variable identifiant la
#' maille.
#' @param idGroupe chaine de caractere (character). Variable identifiant les
#' groupes. Le choix de l'identifiant de groupes est libre ("A", "B" et "C" par
#' exemple).
#' @param libGroupe chaine de caractere (character). Variable des libelles de
#' groupes.
#' @param fondContour objet sf. Fond de carte. Par defaut a NULL.
#'
#' @return Retourne un objet de type \code{sf} (fond de carte)
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_a_facon")
#' data("com_dep_13_30_83_84")
#' data("depm")
#'
#' ze13etplus <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon,
#' idMaille = "DEPCOM", idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
#' fondContour = NULL) # sans contour
#' ze13 <- zonage_a_facon(fondMaille = com_dep_13_30_83_84, groupe = donnees_a_facon,
#' idMaille = "DEPCOM", idGroupe = "ZE2010", libGroupe = "LIB_ZE2010",
#' fondContour = depm[depm$CODE=="13",]) # avec contour
#'
#' \donttest{
#' # affiche les ZE2010 ayant au moins une partie dans les Bouches-du-Rhone
#' plot(sf::st_geometry(ze13etplus), col = "transparent", border = "grey")
#' # affiche uniquement la partie des ZE2010 contenue dans les Bouches-du-Rhone
#' plot(sf::st_geometry(ze13), col = "transparent", add = TRUE)
#' # affiche le contour des Bouches-du-Rhone
#' plot(sf::st_geometry(depm[depm$CODE=="13",]), border = "red", add = TRUE)
#' }
#'
#' @import sf
#'
#' @export zonage_a_facon
#'
zonage_a_facon <-
function(fondMaille,groupe,idMaille,idGroupe,libGroupe,fondContour=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10 <- NULL

    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error1 <- "Le fond de maille doit etre un objet sf / "
    if(any(class(groupe)!="data.frame")) msg_error2 <- "Le groupe doit etre un data.frame / "
    if(any(class(idMaille)!="character")) msg_error3 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(idGroupe)!="character")) msg_error4 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(libGroupe)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(fondContour)) if(any(!any(class(fondContour) %in% "sf"),!any(class(fondContour) %in% "data.frame"))) msg_error6 <- "Le fond du contour doit etre un objet sf / "

    if(!any(names(groupe) %in% idMaille))  msg_error7 <- "La variable identifiant la maille n'existe pas dans la table / "
    if(!any(names(groupe) %in% idGroupe))  msg_error8 <- "La variable identifiant le groupe n'existe pas dans la table / "
    if(!any(names(groupe) %in% libGroupe))  msg_error9 <- "La variable libelle du groupe n'existe pas dans la table / "
    if(length(names(fondMaille))<3) msg_error10 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),
           !is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),!is.null(msg_error9),!is.null(msg_error10)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,msg_error9,msg_error10)))
    }

    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"

    groupe <- groupe[order(groupe[,idGroupe]),]
    liste_code <- groupe[,idMaille]
    zonage_a_facon <- fondMaille[fondMaille$CODE %in% liste_code,]

    if(nrow(zonage_a_facon)==0)
    {
      stop(simpleError("Les identifiants des mailles a regrouper ne correspondent pas aux identifiants du fond de carte."))
    }

    zonage_a_facon <- merge(zonage_a_facon,groupe,by.x="CODE",by.y=idMaille)
    if(length(names(zonage_a_facon)[grep("[.]y",names(zonage_a_facon))])>1)
    {
      zonage_a_facon <- zonage_a_facon[,-grep("[.]y",names(zonage_a_facon))]
      names(zonage_a_facon)[grep("[.]x",names(zonage_a_facon))]<-sub(".x","",names(zonage_a_facon)[grep("[.]x",names(zonage_a_facon))])
    }

    zonage_a_facon <- st_sf(zonage_a_facon[,c("CODE","LIBELLE",idGroupe,libGroupe,"geometry")],stringsAsFactors = FALSE)
    names(zonage_a_facon) <- c("CODE","LIBELLE","CODE_TERR","LIB_TERR","geometry")
    zonage_a_facon <- zonage_a_facon[order(as.data.frame(zonage_a_facon)[,"CODE_TERR"]),]

    zonage_a_facon_split <- split(zonage_a_facon,as.factor(zonage_a_facon$CODE_TERR))

    assign(paste0("zonage_a_facon_",1),st_union(zonage_a_facon_split[[1]],by_feature = FALSE))
    zonage_a_facon <- st_sf(geometry=get(paste0("zonage_a_facon_",1)))

    for(i in 2:length(zonage_a_facon_split))
    {
      assign(paste0("zonage_a_facon_",i),st_union(zonage_a_facon_split[[i]],by_feature = FALSE))
      zonage_a_facon <- unique(rbind(zonage_a_facon,get(paste0("zonage_a_facon_",i))))
    }

    zonage_a_facon <- cbind(CODE_TERR=unique(groupe[,idGroupe]),LIB_TERR=unique(groupe[,libGroupe]),zonage_a_facon)

    if(!is.null(fondContour))
    {
      st_agr(fondContour) <- "constant"
      st_agr(zonage_a_facon) <- "constant"
      zonage_a_facon <- st_transform(zonage_a_facon, crs = st_crs(fondContour))
      zonage_a_facon <- st_intersection(zonage_a_facon,fondContour)
      if(length(names(zonage_a_facon)[grep("..1",names(zonage_a_facon))])>0)
      {
        zonage_a_facon <- zonage_a_facon[,-grep("..1",names(zonage_a_facon))]
      }
    }

    return(zonage_a_facon)
  }
