# Objectif 

# A partir des % de composition des PAT en topics , on essaie de voir s'il y a une évolution temporelle des thématiques des PAT en fonction de leur ancienneté 

# on va utiliser la composition en topics fortifiés 
# => theta_resume_unique ou theta_resume_2

library(tidyverse)
library(ggplot2)
library(fmsb)

###############################################################################################"
# On va utiliser "annee_de_signature_de_la_convention", mais c'est une colonne avec bcp de NA
# dans un premier temps

load(file="data/pat2025.RData")
load(file="data/theta_resume_unique.RData")
load(file= "data/df_textes.RData")
rownames(theta_resume_unique) <- gsub(x = df_textes$doc, pattern = "text", replacement = "")

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
     
    ggtitle("Evolution de la proportion moyennne en thématiques abordées en 2025 par année de signature de la convention ", 
            subtitle = "Nombre de PAT comptabilisés = 230 (175 PAT n'ont pas d'année de signature renseignée) ;\nLecture : les PAT les plus anciens sont à gauche du graphique, les PAT les plus récents à droite") +
     
    labs(
      x = "Année",
      y = "Valeur moyenne",
      color = "Thématique fortifiée"
    ) +
     theme_bw() +
     
   
     # changer le nom des éléments de la légende 
     scale_color_manual(
       values =c("gold1","tomato2","springgreen4","royalblue","chartreuse2","saddlebrown"),
     labels = c("economie_alimentaire" = "Logistique et commercialisation",          
                "education" = "Education et restauration collective",
                "environnement" = "Environnement et santé",  
                "gouvernance" = "Gouvernance", 
                "secteur_agri"=  "Secteur agricole", 
                "territoires"=  "Territoires",
                name = "Noms des topics fortifiés" )) +
     
     
     theme(plot.title = element_text(face = "bold", size = 15 ), 
           plot.subtitle = element_text(face = "italic", size = 14 ),
           axis.title.x = element_text(size = 14), 
           axis.title.y = element_text(size = 14), 
           axis.text.x = element_text(size = 12 ), 
           axis.text.y = element_text(size = 12 ), 
           legend.title =  element_text(size = 14),
           legend.text =  element_text(size = 12 )
           
           ) +
     
     scale_x_discrete(expand = c(0.012, 0)) +
     ylim(2, 50)   
   
  
  
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


#Passage en spyder graph 


spider <- df_evol_temp %>%
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
  pivot_wider(names_from = topic , values_from = valeur) 


row <- spider$annee_n
spider <- spider[,-1]
rownames(spider) <- row

spider <- rbind("Max." = rep(100,9),"Min." = rep(0,9),spider)





##############
# TOUTES LES ANNEES
##############

point <- c(rgb(0.1, 0.1, 0.1, 1),
           rgb(1, 0, 0, 1),
           rgb(0, 1, 0, 1),
           rgb(0, 0, 1, 1),
           rgb(1, 0, 1, 1),
           rgb(0, 1, 1, 1),
           rgb(1, 1, 0.4, 1),
           rgb(0.4, 1, 1, 1),
           rgb(1, 0.4, 1, 1))

areas <- c(rgb(0.1, 0.1, 0.1, 0.25),
           rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25),
           rgb(1, 0, 1, 0.25),
           rgb(0, 1, 1, 0.25),
           rgb(1, 1, 0.4, 0.25),
           rgb(0.4, 1, 1, 0.25),
           rgb(1, 0.4, 1, 0.25))

colnames(spider) <- c("Logistique commercialisation",
                 "Education-Restauration collective",
                 "Environnement-Santé",
                 "Gouvernance",
                 "Secteur agricole",
                 "Territoires")

radarchart(spider,
           plwd = 1,
           cglty = 1,
           pcol=point,
           plty = 1,
           cglcol = "black",
           pfcol = areas,
           axistype = 1,
           caxislabels = c("0", "25", "50", "75", "100"))


legend(x=1.5,
       y=1.75,
       y.intersp = 0.1,
       legend = rownames(spider)[c(-1,-2)],
       bty = "n",
       pch = 19,
       col = areas,
       text.col = "black",
       cex = 1.2,
       pt.cex = 2,
       xjust = 0,
       xpd = T)

title(main = "Comparaison des profils moyen par année")

#####
# 2017 - 2021 - 2024
#####

radarchart(spider[c(1,2,3,7,10),],
           plwd = 2,
           cglty = 1,
           cglcol = "black",
           pcol=point,
           plty = 1,
           pfcol = areas,
           axistype = 1,
           caxislabels = c("0", "25", "50", "75", "100"),
           axislabcol = "darkblue")

legend(x=1.5,
       y=0.8,
       y.intersp = 0.1,
       legend = rownames(spider)[c(3,7,10)],
       bty = "n",
       pch = 19,
       col = areas,
       text.col = "black",
       cex = 1.2,
       pt.cex = 2,
       xjust = 0,
       xpd = T)

title(main = "Comparaison des profils moyen par année")
mtext("Années représentées : 2017 - 2021 - 2024",
      side = 3,
      line = 0.5,
      cex = 1,
      col = "black")
