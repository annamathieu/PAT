# Typologie des PAT selon leur composition en topic fortifiés

# packages 
library(tidyr)
library(ggplot2)
library(tidyverse)
library(FactoMineR)
library(ggrepel)

#############################
# to do 
# NB : ajouter ellipse ✅
# NB : ajouter points de luc  + nom des axes ✅
# NB : ajouter points des variables ✅
# NB : faire nom automatiques des groupes (clusters) ✅
# NB : changer le nom des thématiques fortifiées ✅




##############################
# préparation fichier
load("data/theta_resume2.RData")
load("data/df_clust_ill.RData") # PAT 2025 avec sel de variables illus
load("data/df_textes.RData") #⚠️ ON CHANGE LES ROWNAMES AVANT LA JOINTURE SINON CE SERA PAS LES BONS PAT JOINTS 
load("data/luc_AFC.RData")   # PAT de lucs prétraités 
 
rownames(theta_resume2) <- gsub(x = df_textes$doc, pattern = "text", replacement = "")
theta_resume2 = as.data.frame(theta_resume2)
df_illus_freq_afc <- left_join(data.frame(cbind(num = rownames(theta_resume2), theta_resume2)),
                               data.frame(cbind(num = rownames(df_clust_ill), df_clust_ill)) , by = 'num')

rm(df_textes)
rm(df_clust_ill)

num = as.numeric(as.character(df_illus_freq_afc$num))#on sauve le num des PAT sélectionnés 

df_illus_freq_afc <- df_illus_freq_afc %>%
  select(
    -num,
    -taux_de_pauvrete_du_territoire,
    -emission_de_ges,
    -nombre_de_captage_prioritaire_grenelle_dans_le_perimetre,
    -evolution_de_lirrigation_depuis_5_ans,
    -departement # quali en + que l'on retire
  )# colonnes à retirer du à des NA


colnames(df_illus_freq_afc)[1:6] = c("Logistique commercialisation", "Education Restauration Collective", "Environnement Santé","Gouvernance", "Secteur agricole", "Territoires")


# DOC LUC changer colnames
colnames(luc) = paste0("Dim",seq(1,5))



##############################
# AFC 

# identification des variables supp
quanti.sup <- which(sapply(df_illus_freq_afc, is.numeric))
quanti.sup <- as.numeric(quanti.sup[-c(1:6)]) # on retire les colonnes 1 à 6 qui seront les colonnes actives
quali.sup  <- as.numeric(which(sapply(df_illus_freq_afc, function(x) is.factor(x) || is.character(x))))

res.afc.f <- FactoMineR::CA(df_illus_freq_afc,
                            quanti.sup = quanti.sup, 
                            quali.sup = quali.sup, 
                            ncp = 3) 

#############################
# HCPC 

res.hcpc.f <- HCPC(res.afc.f, nb.clust = -1)
res.hcpc.f$desc.var$frequency



##############################
# GRAPHIQUE DE LA TYPOLOGIE 
##############################
# création du dataframe
# ⚠️ etre sur de bien join sur les bons individus !!! ⚠️

df_hcpc <- data.frame(clust = res.hcpc.f$data.clust$clust)
rownames(df_hcpc) = rownames(res.hcpc.f$data.clust)
df_hcpc <- df_hcpc %>% arrange(as.numeric(rownames(df_hcpc))) # on retrie par nom de lignes

df_hcpc <- data.frame(cbind(df_hcpc, res.afc.f$row$coord[,1:2])) # et on combine avec les coordonnées sur les axes 1 et 2
df_hcpc$clust = as.factor(df_hcpc$clust) # on passe clust en factor 

# ROWNAMES
rownames(df_hcpc) = num # on met les bons noms de numéro de pat REELS en rownames 
rownames(df_illus_freq_afc) = num

# Points des variables à ajouter au graph 
df_coord_var <- data.frame(res.afc.f$col$coord[,1:2])


rownames(luc) <- gsub("luc_","",rownames(luc))

df_coord_text <- df_coord_var
df_coord_text["Logistique commercialisation",]$Dim.1 <- df_coord_text["Logistique commercialisation",]$Dim.1 + 0.5
# df_coord_text["Logistique commercialisation",]$Dim.2 <- df_coord_text["Logistique commercialisation",]$Dim.2 - 0.25
df_coord_text["Education Restauration Collective",]$Dim.1 <- df_coord_text["Education Restauration Collective",]$Dim.1 - 0.2

##########################
# graphique

df_hcpc %>% ggplot() +
  aes(x = Dim.1, y = Dim.2, col = clust) +
  geom_point(size = 2,alpha = 0.3) +
  stat_ellipse() +
  
  theme_bw() +
  
  ggtitle ("Typologie des PAT construite sur leur proportion \nen topics fortifiés dans leur description", 
           subtitle = "349 PAT sont utilisés pour construire cette typologie") +
  
  theme(plot.title = element_text(size = 18, face= "bold", hjust = 0.4), 
        plot.subtitle = element_text(size = 13, , hjust = 0.5), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.title =  element_text(face="bold",size = 16),
        legend.text =  element_text(size = 15 )
        
  ) +
  
  scale_color_manual(
    # nom des groupes 
    label = c("Groupe1", "Groupe2", "Groupe3"), 
    values = c("royalblue", "limegreen", "darkorange"))+ 
  
  labs(
    x = "Dim 1",
    y = "Dim 2",
    color = "Cluster"
  ) +
  
  coord_equal(xlim = c(-1.50, 1.50), ylim = c(-1.50,1.50),clip = "off") +

  
  # POINTS DES DESC DE PAT DE LUC
  # à mettre en commentaire pour ne pas afficher

  
  # POINTS DES VARIABLES 
  
  geom_point(data  = df_coord_var, aes(x=Dim.1, y = Dim.2), col = "black",
            size = 3, shape = 15) +
  geom_text(data  = df_coord_text, aes(x=Dim.1, y = Dim.2+0.1), 
            label = rownames(df_coord_var), col = "black", fontface = "bold", alpha = 1,
            size = 6) +
  geom_point(data = luc,
             aes(x = Dim1, y = Dim2,),
             size = 2.2, color = "red", shape = 15) +
  geom_text_repel(data = luc, aes(x = Dim1, y = Dim2),
                  label = rownames(luc), colour = "brown3",
                  alpha = 0.8,
                  size = 6)
  


###########################################
# PROFILS MOYENS DES PAT par CLUSTER 
###########################################

##########################################
# PAR ANNEE d'ANCIENNETE DU PAT

# graphiques de barplot emplilée, profil moyens en topics par cluster

df_illus_freq_afc[,c(1:6,24)] %>% 
  drop_na() %>% 
  group_by(annee_de_signature_de_la_convention) %>% 
  summarize(across(1:6, mean, .names = "mean_{.col}"))  %>% 
  
  pivot_longer(
    cols = -annee_de_signature_de_la_convention,
    names_to = c("mean", "topic"),
    names_sep = "_",
    values_to = "proportion"
  ) %>% 
  
  select(-mean) %>% 

 ggplot() +
  aes(x = annee_de_signature_de_la_convention, y = proportion) +
  geom_col(aes(fill = topic), position = position_stack()) +
  scale_fill_manual(values = c("tomato2","springgreen4","royalblue","gold1","chartreuse2","saddlebrown"),
                    name = "Topics fortifiés", 
                    labels = c("Education - Restauration collective", "Environnement - Santé", "Gouvernance", 
                               "Logistique Commercialisation" , "Secteur Agricole", "Territoires")) +
  
  theme_bw() +
  
  labs(x = "Année signature convention", y = "Proportion moyenne") +
  
  scale_x_discrete(labels = c("2017","2018","2019","2020","2021","2022","2023","2024","2025")) +
  
  ggtitle("Composition moyenne des PAT en thématiques fortifiés \npar année de signature de la convention") +
  
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 18, angle = 45, face = "bold", vjust = 0.9),
        axis.title.y = element_text(size = 14))


##########################################
#  PAR CLUSTER 

data.frame(cbind(df_illus_freq_afc[,c(1:6)],clust = df_hcpc$clust)) %>% 
  group_by(clust) %>% 
  summarize(across(1:6, mean, .names = "mean_{.col}"))  %>% 
  
  pivot_longer(
    cols = -clust,
    names_to = c("mean", "topic"),
    names_sep = "_",
    values_to = "proportion"
  ) %>% 
  
  select(-mean) %>% 
  
  ggplot() +
  aes(x = clust, y = proportion) +
  geom_col(aes(fill = topic), position = position_stack()) +
  scale_fill_manual(values = c("tomato2","springgreen4","royalblue","gold1","chartreuse2","saddlebrown"),
                    name = "Topics fortifiés", 
                    labels = c("Education - Restauration collective", "Environnement - Santé", "Gouvernance", 
                               "Logistique Commercialisation" , "Secteur Agricole", "Territoires")) +
  
  theme_bw() +
  
  labs(x = "Cluster", y = "Proportion moyenne") +
  
  
  ggtitle("Composition moyenne des PAT en thématiques fortifiés par cluster") +
  
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.2), 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14))



##########################################
#  PAR NIVEAU DE LABELLISATION 

df_illus_freq_afc[,c(1:6,23)] %>% 
  drop_na() %>% 
  filter(niveaux_de_labelisation!="Labellisation en attente") %>% 
  group_by(niveaux_de_labelisation) %>% 
  summarize(across(1:6, mean, .names = "mean_{.col}"))  %>% 
  
  pivot_longer(
    cols = -niveaux_de_labelisation,
    names_to = c("mean", "topic"),
    names_sep = "_",
    values_to = "proportion"
  ) %>% 
  
  select(-mean) %>% 
  
  ggplot() +
  aes(x = niveaux_de_labelisation, y = proportion) +
  geom_col(aes(fill = topic), position = position_stack()) +
  scale_fill_manual(values = c("tomato2","springgreen4","royalblue","gold1","chartreuse2","saddlebrown"),
                    name = "Topics fortifiés", 
                    
                    labels = c("Education - Restauration collective", "Environnement - Santé", "Gouvernance", 
                               "Logistique Commercialisation" , "Secteur Agricole", "Territoires")) +
  
  theme_bw() +
  
  labs(x = "Niveau de labellisation", y = "Proportion moyenne") +
  
  scale_x_discrete(labels = c("Niveau 1", "Niveau 2")) +
  
  ggtitle("Composition moyenne des PAT en thématiques fortifiés \npar niveau de labellisation") +
  
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
        # legend.title = element_text(size = 14), 
        # legend.text = element_text(size = 12), 
        legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))




##########################################
# PROMPT POUR IDENTIFICATION NOM DES CLUSTERS 
# 
# J'ai besoin que tu donnes un nom à des groupes de projets alimentaires territoriaux (PAT). J'ai classifié ces PAT selon leur compositions en thématiques qu'ils abordent pour décrire les enjeux de leur territoires, obtenues par topic modelling et renforcé afin d'otenir des topics plus robustes. Ces thématiques sont : ("Logistique commercialisation", "Education Restauration Collective", "Environnement Santé","Gouvernance", "Secteur agricole", "Territoires") et se décrivent de la façon suivante : Comparison of All Groups
# 
# Les six groupes de mots, identifiés comme des thèmes distincts au sein des systèmes alimentaires territoriaux (SAT), révèlent des facettes complémentaires de ces dynamiques complexes:
#   
#   *   **Planification et Animation Territoriale (Groupe 1)**: Ce thème met l'accent sur les processus de gouvernance, de collaboration et de mise en œuvre stratégique des projets alimentaires territoriaux. Il souligne l'importance des partenariats, de l'engagement communautaire et des cadres d'action pour structurer le SAT.
# 
# *   **Restauration Collective et Éducation Alimentaire (Groupe 2)**: Ce groupe se concentre sur les enjeux de la demande et de la consommation, en particulier au sein de la restauration collective (notamment scolaire). Il aborde les initiatives visant à promouvoir une alimentation saine, durable et locale, la lutte contre le gaspillage, et l'éducation des consommateurs, souvent en lien avec des politiques publiques comme EGalim.
# 
# *   **Typologies Territoriales et Productions Spécifiques (Groupe 3)**: Ce thème décrit les caractéristiques géographiques, démographiques et productives des territoires. Il distingue différents types d'espaces (urbains, ruraux, montagneux, périurbains) et les productions agricoles ou transformées spécifiques qui y sont associées, offrant un aperçu du contexte intrinsèque de chaque SAT.
# 
# *   **Enjeux Climatiques, Environnementaux et Santé (Groupe 4)**: Ce groupe met en lumière les défis majeurs auxquels les SAT sont confrontés. Il couvre les impacts du changement climatique (sécheresse, gestion de l'eau), la préservation de la biodiversité, ainsi que les questions de santé publique liées à l'alimentation (obésité, maladies), soulignant la nécessité d'adaptation et de résilience.
# 
# *   **Analyse Socio-Économique et Agricole (Groupe 5)**: Ce thème est axé sur les indicateurs quantitatifs et les dynamiques économiques et sociales. Il analyse les données statistiques concernant la production agricole (cheptel, céréales, SAU), les conditions socio-économiques des ménages (pauvreté) et les tendances générales à différentes échelles territoriales.
# 
# *   **Logistique et Commercialisation des Filières (Groupe 6)**: Ce groupe aborde les aspects pratiques de la chaîne d'approvisionnement et de la mise sur le marché des produits. Il traite des défis liés à la distribution, à la commercialisation, à la logistique et aux débouchés pour les producteurs, notamment pour les fruits et légumes, identifiant les freins et les leviers d'action.
# 
# En somme, ces thèmes dessinent un panorama holistique des systèmes alimentaires territoriaux, couvrant leur gouvernance, leurs interactions avec les consommateurs, leurs spécificités géographiques, leurs vulnérabilités environnementales, leurs fondements socio-économiques et leurs mécanismes de marché.
# 
# Liste des Noms de Groupe Attribués
# 
# *   **Groupe 1**: Planification et Animation Territoriale
# 
# *   **Groupe 2**: Restauration Collective et Éducation Alimentaire
# 
# *   **Groupe 3**: Typologies Territoriales et Productions Spécifiques
# 
# *   **Groupe 4**: Enjeux Climatiques, Environnementaux et Santé
# 
# *   **Groupe 5**: Analyse Socio-Économique et Agricole
# 
# *   **Groupe 6**: Logistique et Commercialisation des Filières. Voici la description des clusters de PAT obtenus : res.hcpc.f$desc.var$frequency
# 1
#                                    Intern %    glob % Intern freq Glob freq        p.value     v.test
# Gouvernance                       47.219163 14.418409    3069.264   5032.018  0.000000e+00        Inf
# Territoires                       15.876582 18.197108    1031.984   6350.782  4.625218e-08  -5.465148
# Education.Restauration.Collective 27.345113 30.298218    1777.443  10574.064  7.339768e-09  -5.782960
# Logistique.commercialisation       2.176787  9.221044     141.492   3218.140 4.868086e-138 -25.009095
# Secteur.agricole                   2.973828 11.331574     193.300   3954.714 6.159747e-158 -26.775949
# Environnement.Santé                4.408527 16.533647     286.556   5770.235 8.060855e-237 -32.860513
# 
# 2
#                                    Intern %    glob % Intern freq Glob freq        p.value    v.test
# Education.Restauration.Collective 52.889811 30.298218    7034.339  10574.064  0.000000e+00       Inf
# Environnement.Santé               23.024245 16.533647    3062.222   5770.235 8.133766e-141  25.26309
# Logistique.commercialisation       6.974299  9.221044     927.581   3218.140  4.893863e-31 -11.58528
# Gouvernance                        8.777879 14.418409    1167.457   5032.018 5.389526e-130 -24.25842
# Territoires                        5.715854 18.197108     760.208   6350.782  0.000000e+00      -Inf
# Secteur.agricole                   2.617912 11.331574     348.182   3954.714  0.000000e+00      -Inf
# 
# 3                                    Intern %    glob % Intern freq Glob freq        p.value    v.test Territoires                       30.189488 18.197108    4558.590   6350.782  0.000000e+00       Inf Secteur.agricole                  22.604298 11.331574    3413.232   3954.714  0.000000e+00       Inf Logistique.commercialisation      14.232302  9.221044    2149.067   3218.140 1.049825e-174 28.179267 Environnement.Santé               16.036219 16.533647    2421.457   5770.235  2.908400e-02 -2.182346 Gouvernance                        5.266894 14.418409     795.297   5032.018  0.000000e+00      -Inf Education.Restauration.Collective 11.670800 30.298218    1762.282  10574.064  0.000000e+00      -Inf . Je veux que tu donnes un nom à chacun de ces groupes de PAT 
# 


################
# CLAUDE (modele Sonnet 4.5)

# Cluster 1 : "PAT de Gouvernance et Animation Territoriale" ou alternativement : "PAT pilotés par la Gouvernance"
# Cluster 2 : "PAT Restauration Collective et Santé Publique" ou alternativement : "PAT orientés Consommation Durable"
# Cluster 3 : "PAT Filières Agricoles et Valorisation Territoriale" ou alternativement : "PAT Production et Circuits Territoriaux"



################
# ChatGPT (modele GPT 5.2)

# "PAT de Gouvernance et d’Animation Territoriale" ou "Gouverner" (Cluster 1)
# "PAT de la Transition Alimentaire par la Consommation" ou "Transformer les pratiques alimentaires" (Cluster 2)
# "PAT Productifs et Territorialisés" ou "Structurer la production et les filières territoriales" (Cluster 3)

# Cluster	Nom proposé
# 1	PAT de Gouvernance Territoriale
# 2	PAT de Transition Alimentaire
# 3	PAT Productifs Territorialisés


