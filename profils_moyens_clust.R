# graphiques de barplot emplilée, profil moyens en topics par cluster
library(tidyr)
library(ggplot2)
library(tidyverse)

# code à reprendre : 

tidy_model_gamma_t <- cbind(tidy_model_gamma_wide, clust = clust$clust )

pat_moyen <- tidy_model_gamma_t %>% 
  group_by(clust) %>% 
  summarize(across(2:10, mean, .names = "mean_{.col}"))

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
  scale_fill_manual(
    values = c("gold1","tomato2","springgreen4","royalblue","chartreuse2","saddlebrown"),
    labels =  c("economie_alimentaire" = "Logistique et commercialisation",          
                "education" = "Education et restauration collective",
                "environnement" = "Environnement et santé",  
                "gouvernance" = "Gouvernance", 
                "secteur_agri"=  "Secteur agricole", 
                "territoires"=  "Territoires",
                name = "Noms des topics fortifiés" ))
  


