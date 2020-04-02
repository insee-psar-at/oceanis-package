#' @title Histogram of the distribution of the class variable
#'
#' @description Displays a histogram of the distribution of the class variable (ratio, part,
#' evolution ...)  based on the specified method, number of classes or breaks.
#'
#' @details Si les bornes sont renseignees, aucune methode est appliquee. Elle est
#' consideree comme manuelle.
#'
#' Si les donnees a representer possedent des valeurs negatives et positives,
#' alors une borne de classe a zero est geree automatiquement pour les methodes
#' "kmeans", "fisher" et "jenks". La distribution des valeurs dans les classes,
#' effectuee par la methode specifiee, est independante entre les valeurs
#' negatives et les valeurs positives.
#'
#' La methode "quantile" ne gere pas de borne a zero.
#'
#' La fonction \code{ggplot2::ggplot} est utilisee pour la creation de
#' l'histogramme.
#'
#' @usage distrib_variable(data, varRatio, methode = "kmeans", nbClasses = 3,
#' bornes = NULL)
#'
#' @param data tableau de donnees (data.frame).
#' @param varRatio chaine de caracteres (character). Variable des classes de la
#' table.
#' @param methode chaine de caracteres (character). A choisir parmi "kmeans"
#' (par defaut), "fisher", "jenks" ou "quantile".
#' @param nbClasses valeur numerique (numeric). Nombre de classes. Par defaut 3
#' classes.
#' @param bornes vecteur de valeurs numeriques (numeric). Par defaut a NULL.
#'
#' @return Retourne un objet de type \code{ggplot}
#'
#' Affiche l'histogramme dans le plot.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @keywords documentation
#'
#' @examples
#'
#' data("donnees_monoloc")
#'
#' distrib_variable(data = donnees_monoloc ,varRatio = "VAR_AN_MOY", nbClasses = 4)
#'
#' @import classInt ggplot2
#'
#' @export distrib_variable
#'
distrib_variable <-
function(data,varRatio,methode="kmeans",nbClasses=3,bornes=NULL)
  {
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7 <- NULL

    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(class(varRatio)!="character")) msg_error2 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(methode)!="character")) msg_error3 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error4 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error5 <- "La variable doit etre un vecteur numerique / "

    if(!any(names(data) %in% varRatio))  msg_error6 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error7 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "

    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7)))
    }

    donnees <- data.frame(VAR=as.numeric(data[,varRatio]))

    if(is.null(bornes))
    {
      suppressWarnings(bornes <- classIntervals(data[,varRatio],as.numeric(nbClasses),style=methode,rtimes=10,intervalClosure="left")$brks)
    }

    ggplot(donnees, aes(x=donnees$VAR)) +
      stat_bin(breaks=unique(sort(c(min(donnees$VAR),bornes,max(donnees$VAR)))), closed = "left", fill="#5182B6", col="white") +
      scale_x_continuous(breaks=unique(sort(c(min(donnees$VAR),bornes,max(donnees$VAR)))), labels = round(unique(sort(c(min(donnees$VAR),bornes,max(donnees$VAR)))),2)) +
      ggtitle(label=paste0("Distribution de la variable  : ",varRatio)) +
      xlab(label = varRatio)
  }
