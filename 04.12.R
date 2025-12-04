
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
                                          NA)))
  
    # on conserve les colonnes : doc et lem final 
  base = base[,c(1,7)] 
  base = drop_na(base)
    
  return(base)
}


testbase <- res.lemmat 

testbase <- tri.reslemmat(base = testbase)

sum(is.na(testbase$lem.f))



#######################################################################

# fonction pour associer les noms des pat avec les n° des PAT 





#######################################################################

library(topicmodels)  # For LDA
library(tm)          # For DocumentTermMatrix
library(slam)

# df avec les mots regroupés par fréquence 
df_lda <- res.lemmat %>%
  group_by(doc, lem.f) %>%
  summarise(freq = n(), .groups = 'drop')

# Get unique documents and terms
docs <- unique(df_lda$doc)
terms <- unique(df_lda$lem.f)

# Create mappings
doc_to_idx <- setNames(seq_along(docs), docs)
term_to_idx <- setNames(seq_along(terms), terms)

# Map to indices
i <- as.integer(doc_to_idx[df_lda$doc])
j <- as.integer(term_to_idx[df_lda$lem.f])
v <- as.integer(df_lda$freq)  # IMPORTANT: must be integer

# Create simple_triplet_matrix
stm <- simple_triplet_matrix(
  i = i, 
  j = j, 
  v = v,
  nrow = length(docs),
  ncol = length(terms),
  dimnames = list(docs, terms)
)

# Fit LDA model
lda_model_9 <- LDA(stm, k = 9, method = "Gibbs")



tidy_model_beta <- tidy(lda_model, matrix = "beta")


# proba que chaque mot soit tiré dans un topic (1 des 9 topics)
tidy_model_beta %>% 
  filter(term == "social")

tidy_model_beta %>% 
  filter(term == "éducation")

tidy_model_beta %>% 
  filter(term == "production")

tidy_model_beta %>% 
  filter(term == "végétal")

tidy_model_beta %>% 
  filter(term == "agriculture-biologique")



# mots qui participent le + à chaque topic 

tidy_model_beta %>%
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

# identification des axes avec 9 axes : 

# 1 — Ruralité                       | => Urbanisme ? 
# 2 — Alimentation durable           | => Nutrition et santé ? 
# 3 — Agriculture & Climat           | => Environnement ? 
# 4 — Agroéconomie                   | => Economie Alimentaire 
# 5 — Marchés & Consommation         | => Culturel et gasrtronomie 
# 6 — Restauration collective        | => Restauration collective
# 7 — Exploitation agricole          | =>
# 8 — Développement territorial      | => Gouvernance
# 9 — Qualité alimentaire / Accès    | => Justice sociale

# Education alimentaire ? semble absent

# Tester avec un modèle à une cluster de moins


# vecteurs de répartition des axes dans chaque PAT 
tidy_model_gamma <- tidy(lda_model, matrix = "gamma")

# 1 gamma par topic et par texte => cela corresond au % du topic abordé dans chaque texte 
# probabilité de chaque document d'appartenir à un topic.

# modifier la str de ce doc 
# => 

tidy_model_gamma <- data.frame(tidy_model_gamma)

tidy_model_gamma_wide <- tidy_model_gamma %>% pivot_wider(
                               names_from = topic, 
                               values_from = gamma)
 

tidy_model_gamma_wide$document <- gsub("text", "", tidy_model_gamma_wide$document)
tidy_model_gamma_wide$document <- as.numeric(tidy_model_gamma_wide$document)
tidy_model_gamma_wide[,c(2:10)] <- 100*round(tidy_model_gamma_wide[,c(2:10)],3)

# pour obtenir des vecteurs de la répartition des 9 clusters (topics) pour chaque PAT

# peut on faire un classfication des PAT selon ces vecteurs ? 

library(FactoMineR)

res.pca <- PCA(tidy_model_gamma_wide, scale.unit = T, quali.sup = 1, ncp = 9)
res.pca$eig

res.hcpc <- HCPC(res = res.pca, nb.clust = -1, description = T)
res.hcpc$desc.var


#############################################################

# modèle avec 8 clusters 

# modèle
lda_model_8 <- LDA(stm, k = 8, method = "Gibbs")
tidy_model_beta_8 <- tidy(lda_model_8, matrix = "beta")

tidy_model_beta_8 %>%
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

# identificaton des clusters / axes 

# 1 — Agro-environnement           | => Environnement ? 
# 2 — Développement territorial    | => Gouvernance
# 3 — Marchés locaux               | => Economie Alimentaire 
# 4 — Production agricole
# 5 — Alimentation durable         | => Nutrition et santé + Justice sociale 
# 6 — Ressources & foncier
# 7 — Restauration collective      | => Restauration collective
# 8 — Ruralité                     | => Urbanisme 



