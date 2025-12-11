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




# A partir des deux words lists, créer data frame avec en lignes : mots,
# en colonnes : les itérations du lda
# au croisement : topic dans lequel le mot apparait 


# 9 colonnes (ou k colonnes) par lda 

k <- 9      # nombre de topics 
nb_lda <- 2 # nombre d'exécution de la lda 


words = unique(unlist(c(wordsA,wordsB))) # récupére la liste unique des mots de toutes les lda
# ajouter toutes les listes de mots 

lda_list <- list(wordsA,wordsB)
# on créé la liste avec les mots de chaque lda : n objets, contenant chacun k éléments (9 ici)

col = sort(paste0("lda",seq(1:nb_lda), rep(paste0("topic", seq(1:k)),nb_lda)))
# on crée un vecteur avec les noms de colonnes avec x lda et n topics, et on le met dans l'ordre des lda

mfa.df = data.frame(matrix(ncol = 1+ k *nb_lda, nrow = length(words), NA))
# on créé le df => 1 + k * n colonnes 
colnames(mfa.df) <- c("words",col)
# on met les noms de colonnes issus du vecteur col
mfa.df$words <- words
# on ajoute les mots dans la colonne words

# On va ensuite associer à chaque lda les mots des différents topics 

for (i in 1:nb_lda) {
  for (j in 1:k) {
    col_name <- paste0("lda", i, "topic", j)
    
    # récupération des mots du topic j de la lda i
    words_in_topic <- lda_list[[i]][[j]]
    # lda_list[[i]][[j]] retourne les mots du topic j de la lda i
    
    # pour chaque mot du dataframe
    mfa.df[[col_name]] <- ifelse(
      mfa.df$words %in% words_in_topic,
      1,  # si le mot est dans ce topic
      0  
    )
  }
}

# colSums(mfa.df[,-1]) 
# les colonnes avec - de 14 mots ont des mots qui sont répétés 


seeds = sample(1:9999, 10, replace = F)

k = 9
model1 <- lda.model(k = k, seed = seeds[1])
model2 <- lda.model(k = k, seed = seeds[2])
model3 <- lda.model(k = k, seed = seeds[3])
model4 <- lda.model(k = k, seed = seeds[4])
model5 <- lda.model(k = k, seed = seeds[5])
model6 <- lda.model(k = k, seed = seeds[6])
model7 <- lda.model(k = k, seed = seeds[7])
model8 <- lda.model(k = k, seed = seeds[8])
model9 <- lda.model(k = k, seed = seeds[9])
model10 <- lda.model(k = k, seed = seeds[10])

words1 <- topic.extraction(model1)
words2 <- topic.extraction(model2)
words3 <- topic.extraction(model3)
words4 <- topic.extraction(model4)
words5 <- topic.extraction(model5)
words6 <- topic.extraction(model6)
words7 <- topic.extraction(model7)
words8 <- topic.extraction(model8)
words9 <- topic.extraction(model9)
words10 <- topic.extraction(model10)



lda_list <- list(words1,words2,words3,words4,words5,words6,words7,words8,words9,words10)

words <- unique(unlist(lda_list))





