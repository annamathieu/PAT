library(readr)
library(tidyverse)
PAT <- read.csv2("pats-20250710-win1252.csv",header=T,sep=";",fileEncoding = "Windows-1252",dec=",")
PAT[PAT== ""] <- NA

cols <- colnames(PAT)
NA_perce <- apply(PAT,MARGIN = 2,FUN = function(x) round(100*sum(is.na(x)) / 461, 2))
# NA_perce <- sort(NA_perce,decreasing = T)
print(NA_perce)


library(data.table)
PAT_table <- as.data.table(PAT)

#Aucun dispositif d'évaluation mise en place selon les réponses (pas de réponses)
PAT_table[is.na(un_dispositif_de_suivi_evaluation_a_t_il_ete_mis_en_place),.N]

#Nombre de captage grenelle négatif = 2 --> passé en positif ?
PAT_table[nombre_de_captage_prioritaire_grenelle_dans_le_perimetre < 0,.N]

#Description absente pour niv 1 et 2 

PAT_table[niveaux_de_labelisation == "Labellisation Niveau 1" & is.na(descriptions_libre_des_enjeux_du_territoire_par_le_porteur),.N] /PAT_table[niveaux_de_labelisation == "Labellisation Niveau 1",.N]
PAT_table[niveaux_de_labelisation == "Labellisation Niveau 2" & is.na(descriptions_libre_des_enjeux_du_territoire_par_le_porteur),.N] /PAT_table[niveaux_de_labelisation == "Labellisation Niveau 2",.N]
PAT_table[niveaux_de_labelisation == "Labellisation en attente",.N]

library(VIM)
NA_graph <- aggr(x=PAT,numbers=T,sortVar=F)

library(FactoMineR)
PAT_pca <- PAT[,c(1,5:6,11,13:18,21,24:28,30:31,34,41:52,55:56)]
#On regarde les données manquantes pour ce jdd
NA_graph <- aggr(x=PAT_pca,numbers=T,sortVar=F)

substi<-function(x) {gsub(pattern = ",", replacement = ".", x) }
espace<-function(x) {gsub(pattern = " ", replacement = "", x) }
#On considère l'absence de captage grenelle comme une abscence
PAT_pca$nombre_de_captage_prioritaire_grenelle_dans_le_perimetre <- ifelse(is.na(PAT_pca$nombre_de_captage_prioritaire_grenelle_dans_le_perimetre),0,PAT_pca$nombre_de_captage_prioritaire_grenelle_dans_le_perimetre)

PAT_pca <- PAT_pca %>% 
  mutate(id=as.character(id),
         nombre_dhabitants_permanents_du_territoire = as.integer( gsub(x=PAT_pca$nombre_dhabitants_permanents_du_territoire,pattern =" ",replacement = "")),
         nombre_de_communes_du_territoire = as.integer(nombre_de_communes_du_territoire),
         densite_de_population_hab_km2 = as.numeric( gsub(x=densite_de_population_hab_km2,pattern = ",",replacement=".")),
         surface_des_communes_du_territoire_en_km2 = as.numeric( gsub(x=surface_des_communes_du_territoire_en_km2,pattern = ",",replacement=".")),
         surface_du_territoire_artificialisee = as.numeric(surface_du_territoire_artificialisee),
         revenu_median = as.integer(gsub(x=PAT_pca$revenu_median,pattern =" ",replacement = "")))

sapply(PAT_pca,class)

PCA(PAT_pca,quali.sup=c(1:3))



# # test <- var.pca$emission_de_ges
# # test.res <- substi(test)
# var.pca$emission_de_ges <- substi(var.pca$emission_de_ges)
# var.pca$evolution_du_nombre_demplois_dans_la_transformation_entre_2018_et_2023 <- substi(var.pca$evolution_du_nombre_demplois_dans_la_transformation_entre_2018_et_2023)
# var.pca[,c(6:8,11:14,17:21)] <- substi(var.pca[,c(6:8,11:14,17:21)])
# 
# str(var.pca)
# 
# var.pca[,36:40] <- lapply(var.pca[,36:40], 2, FUN = as.numeric)