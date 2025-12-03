
library(quanteda)
library(readxl)
library(cld2)
library(data.table)
library(readtext)

# # Review_corpus <- corpus(game_review, text_field = "review")
# 
# # test_data <- tokens(Review_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE)|>
# #   tokens_remove(stopwords("en"))
# 
# #Filtrage jeu avec au moins 90 commentaires----
# set <- game_review[,.N, by="nom_jeux"]
# set <- c(set[N>=90,]$nom_jeux)
# game_review <- game_review[nom_jeux %in% set]
# 

######
library(readr)
library(tidytext)
library(cld2)
library(dplyr)
library(stringr)
library(data.table)

#Import du jeu de données et mis sous la forme d'un datatable
game_review <- read.csv2(file="Donnees/game_review.csv")
game_review <- data.table(game_review)

#Utilisation du package cld2 pour identifier la "vraie langue" du commentaire, permet de filtrer une partie des commentaires avec des caractères arabes par exemple
game_review <- game_review[,vrai_langue := detect_language(review)]
 #comparaison <- game_review[,c("review","vrai_langue")]
game_review <- game_review[vrai_langue == "en"]


#On garde les jeux avec minimum 90 commentaires
set <- game_review[,.N, by="nom_jeux"]
set_final <- c(set[N>=90,]$nom_jeux)
jeux_commentaires <- game_review[nom_jeux %in% set_final]

#On tokenize les mots et on enlève les mots outils anglais contenu dans tidytext
jeux_mots <- unnest_tokens(tbl= jeux_commentaires, input = review, output = "mot", token = "words")
jeux_mots_stopword <- anti_join(x = jeux_mots, y = tidytext::stop_words, by=c("mot"="word"))

#On veut filtrer tous les "mots" qui contiennent des chiffres, donc majoritairement les nombres
jeux_mot_sans_nombre <- jeux_mots_stopword %>% 
  filter(!stringr::str_detect(mot, "[0-9]"))

#On regarde les mots qui ne sont pas anglais manuellement dans le datatable et on réalise un anti_join avec ce vecteur de "mots", pour supprimer les mots qui sont passés à travers le premier filtre
indesirable <- data.frame(indesirable = c("ゞ","超","簿","探偵","事件","ᄽʘ̆wʘ̆ᄿ","🅰","𝑟𝑒𝑣𝑖𝑒𝑤𝑠","𝑝𝑎𝑔𝑒","𝑚𝑦","𝑚𝑜𝑟𝑒","𝑐𝘩𝑒𝑐𝑘","𝑐𝑢𝑟𝑎𝑡𝑜𝑟","𝐹𝑜𝑟","𝐹𝑒𝑒𝑛𝑎'𝑠","𝐶𝘩𝑜𝑖𝑐𝑒","レ","コード","イン","ω",
                                          "__________","_if_","_once_","૭","aa","aaa"))
jeux_mots_clean <- anti_join(x = jeux_mot_sans_nombre, y = indesirable, by=c("mot"="indesirable"))

library(textstem)

liste_lemma <- read.csv2("Donnees/lemmatization-en.csv", sep=";")

#On enleve tout ce qui concerne les nombres
liste_lemma[-(1:110),]
jeux_mots_clean$lemma <- lemmatize_words(jeux_mots_clean$mot)

freq_lemmes <- jeux_mots_clean %>%
  group_by(mot) %>% 
  summarise(freq=n()) %>% 
  arrange(desc(freq)) %>% 
  na.omit()
head(freq_lemmes)

#On filtre les mots avec moins qu'une certaine occurence dans tous le corpus

freq_lemmes <- as.data.table(freq_lemmes)
mots_filtre<- freq_lemmes[freq>10,]
jeux_mots_lemma <- right_join(x= jeux_mots_clean, y= mots_filtre , by=c("lemma"="mot"))

# freq_lemmes <- jeux_mots_lemma %>%
#   group_by(mot) %>% 
#   summarise(freq=n()) %>% 
#   arrange(desc(freq)) %>% 
#   na.omit()
# head(freq_lemmes)

tib_lex=table(jeux_mots_lemma$lemma, jeux_mots_lemma$nom_jeux)
dim(tib_lex)
tib_lex

library(SnowballC)

jeux_mots_lemma$stem <- SnowballC::wordStem(jeux_mots_lemma$lemma, language = "eng")


jeux_mots_light <- jeux_mots_lemma %>%
  group_by(lemma) %>% 
  mutate(n=n()) %>% 
  filter(n>1000) %>% 
  ungroup()
head(jeux_mots_light)

library(widyr)
mots_cooc <- jeux_mots_light %>% 
  pairwise_count(lemma,feature=nom_jeux,sort=TRUE)


