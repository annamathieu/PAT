
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


#######################################################################

# fonction pour le tri de res lemmat 


tri.reslemmat <- function(base) {
  
  # créer une nouvelle colonne lem.f contenant le lemma disponible 
  # d'abord on regarde si il y a qqch dans lemma y : prio n°1
    base$lem.f <- ifelse(test = !is.na(base$lemma.y), yes = base$lemma.y,
                       
                       # sinon : on va prendre le lemma de lemma x 
                       no = ifelse(test = !is.na(base$lemma.x), yes = base$lemma.x,
                                   
                                   # sinon : on va prendre le lemma de lemma 
                                   no = ifelse(test = !is.na(base$lemma), yes = base$lemma,
                                     
                                          # sinon on prend le token lui même    
                                          base$token)))
  
    # on conserve les colonnes : doc et lem final 
  base = base[,c(1,7)] 
    
  return(base)
}


testbase <- res.lemmat 

testbase <- tri.reslemmat(base = testbase)

sum(is.na(testbase$lem.f))



#######################################################################

# fonction pour associer les noms des pat avec les n° des PAT 






