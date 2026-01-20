library(readr)
library(tidyverse)

loc <- locale(encoding = "latin1")

load("data/df_clust_ill.RData")
pat2025 <- read.csv("data/pats-20250710-win1252.csv", header = T, sep = ";", fileEncoding = "CP1252", dec = ".")

#On importe les données illustratives de chaque PAT (la SAU, le nombre d'habitants etc.)
df_clust_ill <- df_clust_ill[,-c(1:2,40:49)]
df_clust_ill <- cbind(df_clust_ill,id=pat2025$id)

#On importe les données pour chaque axe 

culture <- read_delim("data/Axes/pats-axe-culturel-et-gastronomie-20250710-win1252.csv", delim=";", locale=loc)
economie <- read_delim("data/Axes/pats-axe-economie-alimentaire-20250710-win1252.csv", delim=";", locale=loc)
education <- read_delim("data/Axes/pats-axe-education-alimentaire-20250710-win1252.csv", delim=";", locale=loc)
environnement <- read_delim("data/Axes/pats-axe-environnement-20250710-win1252.csv", delim=";", locale=loc)
gouvernance <- read_delim("data/Axes/pats-axe-gouvernance-20250710-win1252.csv", delim=";", locale=loc)
justice_soc <- read_delim("data/Axes/pats-axe-justice-sociale-20250710-win1252.csv", delim=";", locale=loc)
nutrition <- read_delim("data/Axes/pats-axe-nutrition-et-sante-20250710-win1252.csv", delim=";", locale=loc)
restauration <- read_delim("data/Axes/pats-axe-restauration-collective-20250710-win1252.csv", delim=";", locale=loc)
urbanisme <- read_delim("data/Axes/pats-axe-urbanisme-20250710-win1252.csv", delim=";", locale=loc)

#On annote chaque colonne pour connaître à quel axe thématique cela correspond
colnames(culture) <- paste0("culture_", colnames(culture))
colnames(economie) <- paste0("economie_", colnames(economie))
colnames(education) <- paste0("education_", colnames(education))
colnames(environnement) <- paste0("environnement_", colnames(environnement))
colnames(gouvernance) <- paste0("gouvernance_", colnames(gouvernance))
colnames(justice_soc) <- paste0("justice_soc_", colnames(justice_soc))
colnames(nutrition) <- paste0("nutrition_", colnames(nutrition))
colnames(restauration) <- paste0("restauration_", colnames(restauration))
colnames(urbanisme) <- paste0("urbanisme_", colnames(urbanisme))

#On extrait la colonne etat d'avancement
culture_q <- culture[ , grepl( "etat_davancement" , names( culture ) ) ]
economie_q <- economie[ , grepl( "etat_davancement" , names( economie ) ) ]
education_q <- education[ , grepl( "etat_davancement" , names( education ) ) ]
environnement_q <- environnement[ , grepl( "etat_davancement" , names( environnement ) ) ]
gouvernance_q <- gouvernance[ , grepl( "etat_davancement" , names( gouvernance ) ) ]
justice_soc_q <- justice_soc[ , grepl( "etat_davancement" , names( justice_soc ) ) ]
nutrition_q <- nutrition[ , grepl( "etat_davancement" , names( nutrition ) ) ]
restauration_q <- restauration[ , grepl( "etat_davancement" , names( restauration ) ) ]
urbanisme_q <- urbanisme[ , grepl( "etat_davancement" , names( urbanisme ) ) ]

#On fusionne les tableau
synthese <- cbind(culture_q,economie_q,education_q,environnement_q,gouvernance_q,justice_soc_q,nutrition_q,restauration_q,urbanisme_q)
synthese[synthese==""] <- NA
synthese$total <- rowSums(!is.na(synthese[,1:225]))
dim(synthese)

#On compte le nombre d'action programmée/en cours/réalisée pour chaque PAT
synthese$action_realisee <- rowSums(synthese == "Action réalisée", na.rm = TRUE)
synthese$action_en_cours <- rowSums(synthese == "Action en cours", na.rm = TRUE)
synthese$action_programmee <- rowSums(synthese == "Action programmée", na.rm = TRUE)

#On calcule une fréquence pour chaque type d'action 
synthese$freq_realisee <- ifelse(synthese$total == 0,0,round(synthese$action_realisee/synthese$total,digits = 3)*100)
synthese$freq_en_cours <- ifelse(synthese$total == 0,0,round(synthese$action_en_cours/synthese$total,digits = 3)*100)
synthese$freq_programmee <- ifelse(synthese$total == 0,0,round(synthese$action_programmee/synthese$total,digits =3)*100)

#Fonction pour refaire toute la démarche faites sur le jeu total mais pour chaque sous jeu de données/axe thématique
calcul_stats_actions <- function(df,axes) {
  
  df2 <- as.data.frame(matrix(nrow=460))
  
  df2$action_realisee   <- rowSums(df == "Action réalisée",   na.rm = TRUE)
  df2$action_en_cours   <- rowSums(df == "Action en cours",   na.rm = TRUE)
  df2$action_programmee <- rowSums(df == "Action programmée", na.rm = TRUE)
  
  df2$total <- df2$action_realisee +
    df2$action_en_cours +
    df2$action_programmee
  
  df2$freq_realisee <- ifelse(df2$total == 0, NA,
                             round(df2$action_realisee / df2$total, 3) * 100)
  
  df2$freq_en_cours <- ifelse(df2$total == 0, NA,
                             round(df2$action_en_cours / df2$total, 3) * 100)
  
  df2$freq_programmee <- ifelse(df2$total == 0, NA,
                               round(df2$action_programmee / df2$total, 3) * 100)
  df2 <- df2[,-1]
}

#On isole chacun des sous jeu de données
axes <- list(
  culture = synthese[,grepl("culture_", names(synthese))],
  economie = synthese[,grepl("economie_", names(synthese))],
  education = synthese[,grepl("education_", names(synthese))],
  environnement = synthese[,grepl("environnement_", names(synthese))],
  gouvernance = synthese[,grepl("gouvernance_", names(synthese))],
  justice_soc = synthese[,grepl("justice_soc_", names(synthese))],
  nutrition = synthese[,grepl("nutrition_", names(synthese))],
  restauration = synthese[,grepl("restauration_", names(synthese))],
  urbanisme = synthese[,grepl("urbanisme_", names(synthese))]
)

NA_to_0 <- function(df) {
  df[is.na(df)] <- 0
  df
}

id <- culture$culture_id
#On calcul pour chaque axe thématique
axes_final <- lapply(axes, calcul_stats_actions)

culture_axes <- NA_to_0(axes_final$culture)
colnames(culture_axes) <- paste0("culture_", colnames(culture_axes))

economie_axes <- NA_to_0(axes_final$economie)
colnames(economie_axes) <- paste0("economie_", colnames(economie_axes))

education_axes <- NA_to_0(axes_final$education)
colnames(education_axes) <- paste0("education_", colnames(education_axes))

environnement_axes <- NA_to_0(axes_final$environnement)
colnames(environnement_axes) <- paste0("environnement_", colnames(environnement_axes))

gouvernance_axes <- NA_to_0(axes_final$gouvernance)
colnames(gouvernance_axes) <- paste0("gouvernance_", colnames(gouvernance_axes))

justice_soc_axes <- NA_to_0(axes_final$justice_soc)
colnames(justice_soc_axes) <- paste0("justice_soc_", colnames(justice_soc_axes))

nutrition_axes <- NA_to_0(axes_final$nutrition)
colnames(nutrition_axes) <- paste0("nutrition_", colnames(nutrition_axes))

restauration_axes <- NA_to_0(axes_final$restauration)
colnames(restauration_axes) <- paste0("restauration_", colnames(restauration_axes))

urbanisme_axes <- NA_to_0(axes_final$urbanisme)
colnames(urbanisme_axes) <- paste0("urbanisme_", colnames(urbanisme_axes))

#On prépare notre jeu de données avec les comptages du type d'action par axe thématique
MFA_actions <- cbind(
  id=as.character(id),
  culture_axes[, 1:3],
  economie_axes[, 1:3],
  education_axes[, 1:3],
  environnement_axes[, 1:3],
  gouvernance_axes[, 1:3],
  justice_soc_axes[, 1:3],
  nutrition_axes[, 1:3],
  restauration_axes[, 1:3],
  urbanisme_axes[, 1:3]
)

# MFA_actions2 <- cbind(
#   id,
#   culture_axes[, 5:7],
#   economie_axes[, 5:7],
#   education_axes[, 5:7],
#   environnement_axes[, 5:7],
#   gouvernance_axes[, 5:7],
#   justice_soc_axes[, 5:7],
#   nutrition_axes[, 5:7],
#   restauration_axes[, 5:7],
#   urbanisme_axes[, 5:7]
# )

##### Analyse facto #####
library(Factoshiny)
library(FactoMineR)

#Nombre d'action total par PAT
MFA_actions$sum <- rowSums(MFA_actions[,-1])

MFA_actions <- cbind(MFA_actions,synthese[,c(226,230:232)])

# MFA_actions <- MFA_actions %>%
#   filter(sum != 0) %>%
#   select(-sum)

#Suppression de PAT mal alignés dans le jdd
df_clust_ill$id[289:290] <- c(NA,NA)

df_clust_ill <- df_clust_ill %>% 
  filter(!is.na(id)) %>% 
  select(where(~ !any(is.na(.)))) #Suppression des colonnes avec des NA

nvarsup <- dim(df_clust_ill)[2]

#On index les variables supplémentaire pour la CA
quanti.sup <- which(sapply(df_clust_ill[,-nvarsup], is.numeric)) + dim(MFA_actions)[2]
quali.sup  <- as.numeric(which(sapply(df_clust_ill[,-nvarsup], function(x) is.factor(x) || is.character(x)))) + dim(MFA_actions)[2]
quali.sup <- c(1,quali.sup)

#On aligne bien les PAT avec les var supplmentaires qui n'ont pas de NA
MFA_actions <- MFA_actions %>% 
  left_join(df_clust_ill,join_by(id==id))

#On garde les lignes avec au moins une action enregistrée pour que la CA fonctionne correctement
MFA_actions <- MFA_actions[rowSums(MFA_actions[, 2:28], na.rm = TRUE) != 0, ]

#On réalise la CA
res.CA <-CA(MFA_actions, quali.sup=quali.sup , quanti.sup = c(quanti.sup,29:33))
plot.CA(res.CA, choix = c("CA"),invisible = c("row","quali.sup")) #Regroupement de variable selon qu'elles soient programmée/en cours/réalisée ?
#Quelle interprétation et quelle raison ?


#On cherche le pourcentage d'action entreprise pour chaque axe thématique sur le total d'action listée pour chaque PAT
mots <- c("culture_", "economie_", "education_", "environnement_", "gouvernance_", "justice_soc_","nutrition_","restauration_","urbanisme_")

A <- synthese %>%
  select(-c(226:232))

total_colonnes <- ncol(A)

frequence <- sapply(mots, function(mot) {
  cols <- startsWith(colnames(A),mot)
  round(100 * ifelse(rowSums(!is.na(A)) == 0, 0 , rowSums(!is.na(A[,cols]))/rowSums(!is.na(A))),2) #si on a 0 action dans le projet référencé on force à 0
})

frequence <- as.data.frame(frequence)
frequence$id <- as.character(id)

MFA_actions <- MFA_actions %>% 
  left_join(frequence, join_by(id==id))

res.CA <-CA(MFA_actions, quali.sup=quali.sup , quanti.sup = c(quanti.sup,29:33,66:74))
plot.CA(res.CA, choix = c("CA"),invisible = c("row","quali.sup"))



  