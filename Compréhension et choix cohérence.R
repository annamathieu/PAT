library(topicdoc)
library(tidyverse)

#Fonction visant à produir un graph montrant la cohérence moyenne des topicss proposé à la sortie d'une LDA entre 2 valeurs du nombre de topics
# nstart : nombre de topics minimum
# nend : nombre de topics maximum
# La fonction a un pas de 1 pour les valeurs de k, il est donc recommandé de ne pas mettre des valeurs trop éloignées 
coherence_graph <- function(nstart,nend){
  L <- as.data.frame(matrix(nrow=nend-nstart+1,ncol=2))
  colnames(L) <- c("k","min_coherence")
  L$k <- nstart:nend
  
  for (k_topic in nstart:nend){
    lda_model <- LDA(stm, k = k_topic, method = "Gibbs",
                     control = list(seed = as.integer(800)))
    
    L$min_coherence[k_topic-nstart+1] <- min(topic_coherence(lda_model,stm))
  }
  L %>% ggplot(aes(x=k,y=min_coherence)) +
    geom_line() +
    geom_point()
  
  return(L)
}


A <- coherence_graph(nstart = 3,nend=20)
L %>% ggplot(aes(x=k,y=min_coherence)) +
  geom_line() +
  geom_point()

lda_model <- LDA(stm, k = 9, method = "Gibbs",
                 control = list(seed = as.integer(800)))


rel_dtm <- stm[,c("acteur","action","sau")]
df_dtm <- rel_dtm > 0
cooc_mat <- tcrossprod_simple_triplet_matrix(t(df_dtm))

top_n_tokens <- 3
smoothing_beta <- 1

c_l <- 0
for (m in 2:top_n_tokens) {
  for (l in 1:(top_n_tokens - 1)) {
    df_ml <- cooc_mat[m,l]
    df_l <- cooc_mat[l,l]
    c_l <- c_l + log((df_ml + smoothing_beta) / df_l)
  }
}

#Coherence : mesure à quel point les termes d'un même topic apparaisse ensemble

#log de la fréquence de co-occurence dans des documents df[m,l]+1/df[l,l] :
# df[m,l] --> nombre de documents où les mots d'un thème apparaissent ensemble
# df[l,l] --> nombre de documents avec le terme l
# +1 pour éviter un log de 0
# fais le calcul sur la partie triangle inférieur de la matrice de co-occurence


#In general, a coherence score quantifies the degree of semantic interconnection among words within a given topic. 
#Farea et al. 2024
#C'est la UMass coherence qu'on utilise
