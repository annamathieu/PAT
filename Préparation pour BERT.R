library(dplyr)

#Chargement des textes lemmatisés
load(file = "res.lemmat.RData")

#Nous avons décider de supprimer les mots suivantes car étant les premiers à ressortir dans tout les topics proposés dans le résultat de BERTopic
mots_a_supprimer <- c("territoire", "agricole","alimentaire","alimentation")

res.lemmat <-res.lemmat %>% 
  filter(!(lem.f %in% mots_a_supprimer)) 

#On recréé des documents avec les mots gardés précédemment lemmatisés
#C'est le format que prend BERT en entrée
res.lemmat.fus <- res.lemmat %>%
  group_by(doc) %>%
  summarise(text.lemma = paste(lem.f, collapse = " "))

#On réécrit ce document pour l'utiliser sur python
write.csv2(x=res.lemmat.fus,"python/texte lemma.csv",row.names = F)
