# Objectif 

# A partir des % de composition des PAT en topics , on essaie de voir s'il y a une évolution temporelle des thématiques des PAT en fonction de leur ancienneté 

# on va utiliser la composition en topics fortifiés 
# => theta_resume_unique ou theta_resume_2


###############################################################################################"
# On va utiliser "annee_de_signature_de_la_convention", mais c'est une colonne avec bcp de NA
# dans un premier temps

load(file="data/pat2025.RData")
load(file="data/theta_resume_unique.RData")


annee_sign <- pat2025$annee_de_signature_de_la_convention  # annee signature => les 462 
annee_sign <- annee_sign[as.numeric(rownames(theta_resume_unique))] # On ne garde que celles pour lesquelles on a les vecteurs de %

df_evol_temp <- data.frame(cbind(theta_resume_unique, annee_sign ))
df_evol_temp <- na.omit(df_evol_temp)
df_evol_temp$annee_sign <- as.factor(df_evol_temp$annee_sign)

# ? On va retirer l'année 2025 car il n'y a que 4 pat 


# Construction du graph d'évolution des topics en fonction des années, avec "premiere année de labellisation"

   df_evol_temp %>%
    pivot_longer(
      cols = -annee_sign,
      names_to = "topic",
      values_to = "valeur"
    ) %>%
    
     # on calcule le nombre de PAT par année de signature pour pouvoir l'afficher sur le graph 
     group_by(annee_sign) %>%
     mutate(
       n = n()/6,
       annee_n = paste0(annee_sign, "\n(n=", n, ")")
     ) %>% 
     ungroup() %>% 
     
     # on calcule les % de composition moyens par année et par topic 
     group_by(annee_n, topic) %>%
    summarise(
      valeur = mean(valeur, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    
     #on plot le graph 
    ggplot(aes(x = annee_n, y = valeur, color = topic, group = topic )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    scale_color_manual(values =c("gold1","tomato2","springgreen4","royalblue","chartreuse2","saddlebrown")) +
     
    ggtitle("Evolution de la proportion moyennne du topic par année de signature de la convention ") +
     
    labs(
      x = "Année",
      y = "Valeur moyenne",
      color = "Topic"
    )
   
   
   
  
  
###############################################################################################"

   # graphique de l'évolution des topics en fonction du niveau de labellisation des pat 
   
   niv_lab <- pat2025$niveaux_de_labelisation  # annee signature => les 462 
   niv_lab <- niv_lab[as.numeric(rownames(theta_resume_unique))] # On ne garde que celles pour lesquelles on a les vecteurs de %
   
   df_evol_temp_lab <- data.frame(cbind(theta_resume_unique, niv_lab ))
   df_evol_temp_lab$niv_lab <- as.factor(df_evol_temp_lab$niv_lab)
   
   
boxplot <- df_evol_temp_lab %>%
     pivot_longer(
       cols = -niv_lab,
       names_to = "topic",
       values_to = "valeur"
     ) %>%
     
     filter(niv_lab!="Labellisation en attente") %>% 
     
     #on plot le graph 
     ggplot(aes(x = niv_lab, y = valeur, fill = topic)) +
     geom_boxplot() +
     geom_point(size = 0.5) +
     scale_fill_manual(values =c("gold1","tomato2","springgreen4","royalblue","chartreuse2","saddlebrown")) +
  
     ggtitle("Evolution de la proportion moyennne du topic par niveau de labellisation du PAT ", 
             subtitle = "Niveau 1 = 155 PAT, Niveau 2 = 189 PAT (Niveau en attente = 5 PAT, mis de côté)") +
  
     
     labs(
       x = "Année",
       y = "Valeur moyenne",
       fill = "Topic"
     ) +
     
    theme(axis.text.x = element_text(vjust = 0.5, angle = 45))


boxplot + facet_grid(. ~ topic  )
   