
# filtres nombres : pour les nombres suivies d'une lettre

test = "2ème 2007menaces covid19 acteur.rice -acteur 2010-2020 agri-alimentaire et2022-2027 egalim6-lutte 1-structuration"

gestion_nombres <- function(text) {
  text <- gsub("\\.", " ", x =  text, perl = TRUE)                   # mettre des espaces à la place des points
  text <- gsub("(?<=[0-9])(?=[[\\p{L}])|(?<=[[\\p{L}])(?=[0-9])", " ", x =  text, perl = TRUE)   # mettre des espaces s'il y a un chiffre avant une lettre minuscule (ex : 2007menaces) ou si il y a un chiffre après une lettre minuscule (ex : covid19)
  text <- gsub("(?<=[0-9])-(?=[[\\p{L}])|(?<=[[\\p{L}])-(?=[0-9])|(?<=[0-9])-(?=[0-9])", " ", text, perl = TRUE) # chiffre'-'lettre ou lettre'-'chiffre ou chiffre'-'chiffre
  text <- gsub(" -", " ", x = text)                                  # supprimer les tirets en début de mots
  return(text)

}


# filtres

test = gestion_nombres(test)
test


