# Consolidation LDA pour aller vers typo 

library(clue)


topic.association <- function (model1, model2) {
  
  # on a dans lda 1 le model lda 1  
  # et idem dans model  2
  
  words1 <- topic.extraction(model1)  # on récupère les mots du 1
  words2 <- topic.extraction(model2)  # et les mots du 2 
  
  # faut il supprimer certains mots ? (alimentaire, agricole, alimentation .... ?)
  
  # on va ensuite associer les topics du model 1 aux topics du model 2
  get_overlap <- function(words1, words2) {
    sum(words1 %in% words2)
  }
  
  K <- length(words1) # nombre de topics 
  S <- matrix(0, K, K) # matrice carrée de taille K * K 
  
  for (i in 1:K) {
    for (j in 1:K) {
      S[i, j] <- get_overlap(words1[[i]], words2[[j]])
    }
  }
  
  # conversion en matrice de cout 
  cost <- max(S) - S
  
  # association topic  
  assign <- solve_LSAP(cost) # numero du 2 qui correspond au 1 
  assign <- as.numeric(assign)
  
  # critère supplémentaire ? 
  ####
  
  
  # on va aller récupérer les mots du model 2 correspondants au topic 1 pour les associer aux mots des topics 1 correspondant
  words_consolides <- words1
  for (i in 1:K){
    id <- as.numeric(assign[i])
    words_consolides[[i]] <- c(words1[[i]], words2[[id]])
  }
  
  return(words_consolides)
  
  
  # return l'association des topics, et la liste des mots de chaque topic
  # return(assign)
  
}


# topic.identification <- function (matrice_asso) {
#   
# 
# }

res = topic.association(model1 = lda.model(k = 9, seed = 1234), 
                        model2 = lda.model(k = 9, seed = 4567))

res



consolidation.lda <-  function(nb.lda = 10, k) {
  
 seed.list = sample(1:9999,nb.lda, replace = F)
 
 
 
 while (length(seed.list)>2){
   res = topic.association(model1 = lda.model(9,seed.list[1]), model2 = lda.model(9,seed.list[2]))
   seed.list <- seed.list[-c(1:2)]
   
   
 }
 
}



res = consolidation.lda(10, 9)


#######################################################################################

seed.list = sample(1:9999,10, replace = F)



topic.extraction2 <- function(topic_model) {
  
  frex <- data.frame(t(summary(topic_model)$frex))
  beta <-  data.frame(t(summary(topic_model)$prob))
  
  liste_mots <- rbind(frex,beta)

  list <- sapply(liste_mots, paste, collapse = " ")
  list <- str_split(list, pattern = " ")
  
  return (list)
  
}

topic.extraction2(modeltest)


