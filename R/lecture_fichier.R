lecture_fichier <-
function(file)
{
  msg_error1 <- NULL
  
  if(any(class(file)!="character")) msg_error1 <- "Le file doit etre une chaC.ne de caracteres / "
  
  if(!is.null(msg_error1))
  {
    stop(simpleError(msg_error1))
  }
  
  fichiers <- NULL
  
  if(str_sub(file,start=-4) %in% c(".dbf","DBF")) fichiers <- read.dbf(file, as.is=T)
  if(str_sub(file,start=-4) %in% c(".xls","XLS")) fichiers <- read.xlsx(file, sheetIndex = 1)
  if(str_sub(file,start=-4) %in% c(".ods",".ODS")) fichiers <- read_ods(file)
  if(str_sub(file,start=-4) %in% c(".csv",".CSV"))
  {
    fichiers <- read.table(file, sep=",", quote = "\"")
    names_col <- names(read.csv(file, sep=",", quote = "\""))
    names(fichiers) <- names_col
    if(names_col[1]=="X") fichiers <- fichiers[,-1]
  }
  if(str_sub(file,start=-4) %in% c(".rds",".RDS")) fichiers <- readRDS(file)
  if(str_sub(file,start=-4) %in% c(".rda",".RDA",".rdata",".RData","RDATA"))
  {
    nom_fichier <- load(file)
    fichiers <- get(nom_fichier)
    rm(nom_fichier)
  }
  
  if(is.null(fichiers))
  {
    stop(simpleError("Impossible de lire le fichier. Verifier svp le chemin d'acces au fichier."))
  }else
  {
    return(fichiers)
  }
}
