# data preprocessing 
library(tidyverse)
pat2025 <- read.csv("data/pats-20250710-win1252.csv", header = T, sep = ";", fileEncoding = "CP1252",
                    dec = ".") # CP1252 permet de gérer les apostrophes non détectées 
pat2025[pat2025== ""] <- NA # transformation des cases vides en NA
pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur <- gsub("’", "'", pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur, fixed = TRUE)

textdata <- as.data.frame(cbind(doc_id = pat2025$nom_administratif, text = pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur))


# gérer le pb des sauts de lignes non pris en compte
textdata$text <- gsub("(?<=[a-z])(?=[A-Z])", " ", x =  textdata$text, perl = TRUE) # décole les MAJ collées à des minuscules (précédées par des minuscules)

# Supprimer les doubles espaces
textdata$text <- gsub(pattern = "  ", replacement = " ", x = textdata$text)


# supprimer le nom des villes
# noms des villes 
villes <- gsub(pattern = ",", replacement = "", x = pat2025$communes_nom) # on retire les virgules
villes <- unique(unlist(strsplit(x = villes, split = " "))) # on coupe après chaque espace , on créé un vecteur et on supprime les doublons 
villes <- as.character(chartr(                        # on retire les accents 
  old = "ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü",
  new = "AACEEEEIIOUUUaaceeeeiiouuu",
  x= villes))
# villes <- gsub(pattern = "-",, replacement = " ", x = villes, fixed = TRUE)
villes<- villes[-c(1:5)]

villes_PAT <- strsplit(x = pat2025$communes_nom, split = ", ") # permet de couper après les virgules et garde bien les "Le... La...." qui sont dans les noms des villes 

# noms_PAT <- strsplit(x= textdata$doc_id, split = " ") # ajouter le nom des PAT pour les retirer aussi des descriptions
# noms_PAT <-str_remove_all(string = noms_PAT, pattern = c"")


retire_villes <- function(df) {
  for (i in 1:nrow(df)) {
    pattern =  paste0(villes_PAT[[i]], collapse = "|")
    # pattern2 = paste(noms_PAT[[i]], collapse = "|")
    df[i,2] <- gsub(pattern= pattern, replacement = "", x = df[i,2])
    # df[i,2] <- gsub(pattern= pattern2, replacement = "", x = df[i,2])
    }
  return(df)}

textdata <- retire_villes(df = textdata)


# passer en minuscules 
textdata$text <- (tolower(textdata))

