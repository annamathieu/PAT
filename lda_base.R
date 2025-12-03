
# 1- Chargement des données 
library(tidyverse)
pat2025 <- read.csv("data/pats-20250710-win1252.csv", header = T, sep = ";", fileEncoding = "CP1252",
                    dec = ".") # CP1252 permet de gérer les apostrophes non détectées 
pat2025[pat2025== ""] <- NA # transformation des cases vides en NA
pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur <- gsub("’", "'", pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur, fixed = TRUE)

# Installation de l'environnement (si besoin)
# install.packages("tm")
# install.packages("topicmodels")
# install.packages("reshape2")
# install.packages("wordcloud")
# install.packages("pals")
# install.packages("SnowballC")
# install.packages("lda")
# install.packages("ldatuning")
# install.packages("kableExtra")
# install.packages("flextable")

# 2- load packages
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)


# 3- Data Preprocessing 

# chargement des stop words en francais 
fr_stopwords <- read.table("data/stopwords-fr.txt")
fr_stopwords <- as.vector(fr_stopwords$V1)
# from : https://github.com/stopwords-iso/stopwords-fr/blob/master/stopwords-fr.txtas.character(chartr(                        # on retire les accents 
fr_stopwords <-  as.character(chartr(   
old = "ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü",
new = "AACEEEEIIOUUUaaceeeeiiouuu",
x= fr_stopwords))

textdata <- as.data.frame(cbind(doc_id = pat2025$nom_administratif, text = pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur))
# il y a 3 NA pour "nom administratif" (versus 23 NA pour "nom usuel") donc on va utiliser la colonne nom usuel  
# > sum(is.na(pat2025$nom_administratif))
# [1] 3
# > sum(is.na(pat2025$nom_usuel))
# [1] 23

# noms des villes 
villes <- gsub(pattern = ",", replacement = "", x = pat2025$communes_nom) # on retire les virgules
villes <- tolower(unique(unlist(strsplit(x = villes, split = " ")))) # on coupe après chaque espace , on créé un vecteur et on supprime les doublons 
villes <- as.character(chartr(                        # on retire les accents 
  old = "ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü",
  new = "AACEEEEIIOUUUaaceeeeiiouuu",
  x= villes))
villes <- gsub(pattern = "-",, replacement = " ", x = villes, fixed = TRUE)



# retirer les apostre
  
# create corpus object
corpus <- Corpus(DataframeSource(textdata))                     # créer le corpus 


# passe tous les mots en minuscules 
processedCorpus <- tm_map(corpus, content_transformer(tolower))  
processedCorpus[[1]]$content # verif 


# retire la ponctuation 
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE) 

# on retire les accents 
processedCorpus <- tm_map(processedCorpus, FUN = content_transformer(function(x) {  stringi::stri_trans_general(x, "Latin-ASCII")}))

# retire les stop words
processedCorpus <- tm_map(processedCorpus, removeWords, fr_stopwords) 

# retire la ponctuation 2e fois 
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE) 

# retire les nombres
processedCorpus <- tm_map(processedCorpus, removeNumbers) 

# retire les stop words 2e fois 
processedCorpus <- tm_map(processedCorpus, removeWords, fr_stopwords) 

# processedCorpus <- tm_map(corpus, removeWords, villes) # retirer les noms propres : nom des villes  

processedCorpus <- tm_map(processedCorpus, stemDocument, language = "fr") # stemmatisation : vise à garder la racine du mot
processedCorpus <- tm_map(processedCorpus, stripWhitespace) # retire les espaces doubles 



processedCorpus[[1]]$content # verif 

