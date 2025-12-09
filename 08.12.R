# Test du LDA avec la fonction stm => Structural Topic Model 

# install.packages("stm")

library(stm)
library(tidytext)



###########################################################
# obtention du bon format de départ pour lancer STM 

# step 1 : Create the Vocabulary
vocab <- unique(res.lemmat$lem.f) # liste unique de mots 

# step 2 :  Create a Word-to-Index Mapping
word_to_idx <- setNames(seq_along(vocab), vocab)

# Step 3: Count Word Occurrences per Document
# Group by document and count occurrences of each word
doc_word_counts <- res.lemmat %>%
  group_by(doc, lem.f) %>%
  summarise(count = n(), .groups = 'drop')

# step4 4 : create document list 
# Get unique documents in order
unique_docs <- unique(res.lemmat$doc)

# Create the documents list
documents <- lapply(unique_docs, function(current_doc) {
  # Filter words for this document
  words_in_doc <- doc_word_counts %>%
    filter(doc == current_doc)
  
  # Convert words to indices
  indices <- as.integer(word_to_idx[words_in_doc$lem.f])
  counts <- as.integer(words_in_doc$count)
  
  # Create 2-row matrix: row 1 = indices, row 2 = counts
  matrix(c(indices, counts), nrow = 2, byrow = TRUE)
})

# Name the documents (optional but recommended)
names(documents) <- unique_docs


# Step 5 : prep with prep documents
out <- prepDocuments(documents, vocab, 
                     lower.thresh = 1,  # remove words appearing in only 1 doc
                     upper.thresh = Inf)

documents <- out$documents
vocab <- out$vocab

###########################################################
# STM 
topic_model<-stm(documents, 
                 vocab,
                 K=9, verbose=FALSE, init.type = "LDA", 
                 seed = 1245)

summary(topic_model)



# Topic 1 : Commercialisation et logistique → Économie Alimentaire
# Topic 2 : Planification territoriale alimentaire → Gouvernance
# Topic 3 : Restauration collective et anti-gaspillage → Restauration collective + Éducation alimentaire
# Topic 4 : Déclin démographique rural → Justice Sociale
# Topic 5 : Structures et pratiques agricoles → Économie Alimentaire
# Topic 6 : Identités et patrimoines territoriaux → Culture et gastronomie
# Topic 7 : Géographie des territoires → Urbanisme
# Topic 8 : Transition agroécologique → Environnement
# Topic 9 : Gouvernance territoriale → Gouvernance

lda.model <- function(k, seed) {
  topic_model<-stm(documents, 
                   vocab,
                   K=k, verbose=FALSE, init.type = "LDA", 
                   seed = seed)
  
  return(topic_model)
}



topic.extraction <- function(topic_model) {
  
  # Faire tourner l'algo plusieurs fois => associer les thèmes ensemble pour les rendre + robustes ... ? 
  # ???
  
  # récupérer les mots avec les indices FREX (Fréquence exclusivité) les plus forts 
  frex <- data.frame(t(summary(topic_model)$frex))
  
  # récupérer aussi ceux avec coefficients beta = probabilité d'appartenance d’un terme dans un thème
  beta <-  data.frame(t(summary(topic_model)$prob))
  
  # supprimer les mots qui ne portent pas de sens (alimentaire ? alimentation ?)
  # ???
  
  # créer liste_mots
  liste_mots <- rbind(frex,beta)
  colnames(liste_mots) <- paste0("topic",seq(from=1,to=9))
  
  list <- sapply(res, paste, collapse = " ")
  
  return (list)
  
}

res = topic.extraction(topic_model)
res





install.packages('clue')
clue::solve_LSAP

plot.STM(topic_model, type = "hist", 
         labeltype = "frex")

cloud(topic_model)

plot(topic_model, type = "perspectives", topics = c(4, 8))

plot.topicCorr(topic_model)

findThoughts(topic_model)


mod.corr <- topicCorr(topic_model)
plot(mod.corr)









