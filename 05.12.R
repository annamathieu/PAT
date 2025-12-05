
# petit alpha : meilleure séparation 

# test avec variation de alpha pour un même nb de clusters 

k=9

lda_model_alpha <- LDA(stm, k = k, method = "Gibbs", 
                       control = list(alpha =0.5))

tidy_model_beta_alpha <- tidy(lda_model_alpha, matrix = "beta")

tidy_model_beta_alpha %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_fill_viridis_d() + 
  coord_flip() + 
  labs(x = "Topic", 
       y = "beta score", 
       title = "Topic modeling of PAT description")

terms(tidy_model_beta_alpha, 8)


# 1 - Gouvernance 
# 2 - Ruralité 
# 3 - Economie alimentaire
# 4 - Production agricole
# 5 - Filière et circuits locaux
# 6 - Environnement (agriculture durable)
# 7 - Alimentation durable 
# 8 - Justice Sociale - Nutrition santé 
# 9 - Culturel et gastronomie 

require(ldatuning)

library(remotes)
remotes::install_github("nikita-moor/ldatuning")



# DTM <- DocumentTermMatrix(res.lemmat)
# 
# result <- ldatuning::FindTopicsNumber(
#   DTM,
#   topics = seq(from = 2, to = 25, by = 1),
#   metrics = c("CaoJuan2009",  "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(result)
# 
# K = 5
# topicModel_5 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
# 
# tmResult <- posterior(topicModel_5)
# # format of the resulting object
# attributes(tmResult)
# 
# # topics are probability distributions over the entire vocabulary
# beta <- tmResult$terms   # get beta from results
# dim(beta)                # K distributions over nTerms(DTM) terms
# 
# terms(topicModel_5, 5)







####################################################################

# récupérer les % moyennes de topics 1 à 9 dans chaque PAT moyen représentatif du cluster 

# relancer le modèle avec les paramètres choisis 
# => k = 9 topics
# => alpha = 0.5


k=9

lda_model_test <- LDA(stm, k = k, method = "Gibbs", 
                       control = list(alpha =0.5))

tidy_model_beta_t <- tidy(lda_model_test, matrix = "beta")

tidy_model_gamma_t <- tidy(lda_model_test, matrix = "gamma")

# 1 gamma par topic et par texte => cela corresond au % du topic abordé dans chaque texte 
# probabilité de chaque document d'appartenir à un topic.

# modifier la str de ce doc 
# => 

tidy_model_gamma_t <- data.frame(tidy_model_gamma_t)

tidy_model_gamma_t <- tidy_model_gamma_t %>% pivot_wider(
  names_from = topic, 
  values_from = gamma)


tidy_model_beta_t %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_fill_viridis_d() + 
  coord_flip() + 
  labs(x = "Topic", 
       y = "beta score", 
       title = "Topic modeling of PAT description")


tidy_model_gamma_t$document <- gsub("text", "", tidy_model_gamma_t$document)
tidy_model_gamma_t$document <- as.numeric(tidy_model_gamma_t$document)
tidy_model_gamma_t[,c(2:10)] <- 100*round(tidy_model_gamma_t[,c(2:10)],3)

# faire le clustering
res.pca_t <- PCA(tidy_model_gamma_t, scale.unit = T, quali.sup = 1, ncp = 3, graph = F)

res.hcpc_t <- HCPC(res = res.pca_t, nb.clust = -1, description = T)

clust_t <- data.frame(cbind(doc = tidy_model_gamma_t$document, 
                          res.pca_t$ind$coord[,1:2],
                          clust = res.hcpc_t$data.clust$clust))
clust_t$clust <- as.factor(clust_t$clust)


tidy_model_gamma_t <- cbind(tidy_model_gamma_t, clust = clust_t$clust )


pat_moyen <- tidy_model_gamma_t %>% 
  group_by(clust) %>% 
  summarize(across(2:10, mean, .names = "mean_{.col}"))

library(tidyr)
pat_moyen_long <- pivot_longer(data = pat_moyen, 
                                 cols = 2:10,names_to = c("topic", "proportion"), 
                               names_sep = "_",
                               values_to = "proportion") 


pat_moyen_long <- pivot_longer(
  data = pat_moyen,
  cols = 2:10,
  names_to = c("mean", "topic"),
  names_sep = "_",
  values_to = "proportion"
)



pat_moyen_long %>% ggplot() +
  aes(x = clust, y = proportion) +
  geom_col(aes(fill = topic), position = position_stack()) +
  # scale_color_manual(values = c("green4","blue4","lightgreen","grey","orange2", "purple2","yellow3","green2","pink"))+
  scale_fill_manual(
    values = c("yellow2","blue4","lightgreen","grey","orange2", "purple2","yellow3","green2","pink"),
    labels = c(
      "Production agricole",
      "Gouvernance",
      "Alimentation durable",
      "Ruralité",
      "Restauration Collective",
      "Justice Sociale",
      "Filières locales",
      "Environnement",
      "Nutrition et santé"
    )
  )


# Bar plot empilés de y = counts par x = cut,
# coloré par la variable
ggplot(df2, aes(x = dose, y = len)) +
  geom_col(aes(color = supp, fill = supp), position = position_stack()) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

