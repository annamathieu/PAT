library(tibble)
library(dplyr)
library(stringr)
library(tidyverse)
library(emmeans)

#Importation jeu de données
pat2025 <- read.csv("data/pats-20250710-win1252.csv", header = T, sep = ";", fileEncoding = "CP1252", dec = ".") # CP1252 permet de gérer les apostrophes non détectées 

#On cherche à reconnaître le nombre d'axes qui sont abordées par les PAT pour connaître l'aspect systémique
pat2025$axes_thematiques <- str_replace(pat2025$axes_thematiques,"Tourisme,","Tourisme")
theme <- pat2025$axes_thematiques

motifs <- c("Éducation alimentaire","Économie alimentaire dont structuration des filières","Nutrition et santé",
            "Justice sociale dont lutte contre la précarité alimentaire","Tourisme mise en valeur du patrimoine alimentaire et gastronomie",
            "Environnement dont lutte contre le gaspillage alimentaire","Accompagnement de la restauration collective",
            "Urbanisme et Aménagement du territoire","Gouvernance alimentaire du territoire","Autres")

#On compte le nomnbre de thème et on créé un tableau pour pouvoir faire des analyses/visu
compte_theme <- str_count(theme, str_c(motifs, collapse = "|"))
compte_theme <- as.data.frame(compte_theme)

#On rajoute le niveau de labellisation
A2 <- df %>%
  rownames_to_column("Symbol") %>%
  left_join(
    pat2025 %>%
      rownames_to_column("Symbol") %>%
      select(Symbol,niveaux_de_labelisation),
    by = "Symbol"
  ) %>%
  column_to_rownames("Symbol")

#On regarde le nombre de sujets abordés par le PAT en regardant le nombre de sujet où le pourcentage de composition du texte est supérieur à 1% (on considère ça comme un artefact)
A2$Sujets <- apply(A2[,1:6],MARGIN = 1,function(x) sum(x>=1))

A2 %>% 
  ggplot(aes(x=niveaux_de_labelisation,y=Sujets)) +
  geom_boxplot()


#On rajoute l'année de signature de la convention, c'est l'information la plus proche d'une date pour déterminer l'ancienneté du PAT
#A verifier et préciser 

A2 <- A2 %>%
  rownames_to_column("Symbol") %>%
  left_join(
    pat2025 %>%
      rownames_to_column("Symbol") %>%
      select(Symbol,annee_de_signature_de_la_convention),
    by = "Symbol"
  ) %>%
  column_to_rownames("Symbol")

#On rajoute le nombre de thèmes présents dans la colonne axes_thématiques qu'on a calculé plus tôt :
A2 <- A2 %>%
  rownames_to_column("Symbol") %>%
  left_join(
    compte_theme %>%
      rownames_to_column("Symbol") %>%
      select(Symbol,compte_theme),
    by = "Symbol"
  ) %>%
  column_to_rownames("Symbol")

#On enlève labellisation en attente qui sont des PAT n'étant pas encore en action
# A2 <- A2 %>% filter(compte_theme!=0) %>% 
#   filter(niveaux_de_labelisation != "Labellisation en attente")

#Boxplot pour l'année de signature 
A2 %>% 
  filter(compte_theme!=0) %>% 
  ggplot(aes(x=factor(annee_de_signature_de_la_convention),y=compte_theme)) +
  geom_boxplot() +
  geom_jitter(height=0) +
  theme_minimal()+
  ylim(0,9)

#Boxplot pour le niveau de labellisation
A2 %>% 
  filter(compte_theme!=0) %>% 
  ggplot(aes(x=factor(niveaux_de_labelisation),y=compte_theme)) +
  geom_boxplot() +
  geom_jitter(height=0) +
  theme_minimal()+
  ylim(0,9)

#Test chi-2 et modèle linéaire avec niveaux_de_labelisation
tab <- table(A2$compte_theme,A2$niveaux_de_labelisation)

A2 %>% 
  ggplot(aes(x=compte_theme,y=..density..,fill=niveaux_de_labelisation,colour = niveaux_de_labelisation))+
  geom_histogram(position = "identity",bins = 10,alpha = 0.5)+
  theme_classic()

chisq.test(tab,simulate.p.value = T)

mod1 <- lm(data=A2,compte_theme ~ niveaux_de_labelisation)
anova(mod1)
em_mod1 <- emmeans(mod1, ~ niveaux_de_labelisation)
pairs(em_mod1) #On a une différence entre label 1 et label 2, on a globalement un moins grand nombre de topic

#Test chi-2 et modèle linéaire avec année de signature de la convention
A2 <- A2 %>%
  mutate(
    Groupe = case_when(
      annee_de_signature_de_la_convention %in% 2017:2019 ~ "2017–2019",
      annee_de_signature_de_la_convention %in% 2020:2022 ~ "2020–2022",
      annee_de_signature_de_la_convention %in% 2023:2025 ~ "2023–2025",
      TRUE ~ NA_character_
    )
  )

A2 <- A2 %>%
  mutate(
    annee = as.factor(annee_de_signature_de_la_convention)
  ) %>%
  group_by(annee) %>%
  mutate(
    n = n(),
    annee_n = paste0(annee, "\n(n=", n, ")")
  )


A2 %>% 
  group_by(Groupe) %>% 
  summarise(Nb_moyen_theme = mean(compte_theme))

A2 %>% 
  group_by(niveaux_de_labelisation) %>% 
  summarise(Nb_moyen_theme = mean(compte_theme))

A2 %>% 
  group_by(annee_de_signature_de_la_convention) %>% 
  summarise(Nb_moyen_theme = mean(compte_theme))

A2 %>% 
  ggplot(aes(x=compte_theme,y=..density..,fill=annee , colour = annee))+
  geom_histogram(position = "dodge",bins = 10,alpha = 0.9)+
  geom_density(aes(alpha=0))+
  theme_classic()


#On tente de regrouper entre la période avant; après et encore après France Relance en 2021 pour voir si des 

tab <- table(A2$compte_theme,A2$Groupe)
chisq.test(tab,simulate.p.value = T)

mod3 <- lm(data=A2,compte_theme ~ factor(Groupe))
anova(mod3)
em_mod3 <- emmeans(mod3, ~ Groupe)
pairs(em_mod3) #On a une différence entre label 1 et label 2, on a globalement un moins grand nombre de topic

#####POISSON#####

A3 <- A2 %>%
  rownames_to_column("Symbol") %>%
  left_join(
    pat2025 %>%
      rownames_to_column("Symbol") %>%
      select(Symbol, niveaux_de_labelisation),
    by = "Symbol"
  ) %>%
  column_to_rownames("Symbol")

library(broom)
library(car)

mod_poisson <- glm(data=A2, compte_theme ~ as.factor(annee_de_signature_de_la_convention), family = poisson)
tidy(mod_poisson)
summary(mod_poisson)
glance(mod_poisson)

# mod_poisson <- glm(data=A2, Sujets ~ as.factor(annee_de_signature_de_la_convention), family = poisson)
# tidy(mod_poisson)
# summary(mod_poisson)
# glance(mod_poisson)

library(tidyverse)

A2 %>% 
  ggplot(aes(x=annee_n,y=compte_theme,fill=annee_n)) +
  geom_boxplot()+
  geom_jitter(height=0.05)+
  ggtitle("Nombre de thématiques abordées par année de mise en place des PAT")+
  theme_bw() +
  xlab("Année de mise en place") +
  ylab("Nombre de thématiques") +
  theme(plot.title = element_text(face="bold",hjust=0.5,size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.position = "none",
        axis.text.x = element_text(size=16))


library(emmeans)
emmeans(mod_poisson,pairwise ~ as.factor(annee_de_signature_de_la_convention), type= "response")

mod_poisson <- glm(data=A2, compte_theme ~ Groupe, family = poisson)
tidy(mod_poisson)
summary(mod_poisson)
glance(mod_poisson)


mod_poisson <- glm(data=A2, compte_theme ~ niveaux_de_labelisation, family = poisson)
tidy(mod_poisson)
summary(mod_poisson)
glance(mod_poisson)

library(AER)
library(tidyverse)
dispersiontest(mod_poisson,alternative = "less")

ggplot(A2, aes(compte_theme, fill = niveaux_de_labelisation)) + geom_histogram(binwidth = 1) + facet_grid(niveaux_de_labelisation ~., margins = TRUE, scales = "free")
ggplot(A2, aes(compte_theme, fill = Groupe)) + geom_histogram(binwidth = 1) + facet_grid(Groupe ~., margins = TRUE, scales = "free")


ggplot(A2, aes(Sujets, fill = niveaux_de_labelisation)) + geom_histogram(binwidth = 1) + facet_grid(niveaux_de_labelisation ~., margins = TRUE, scales = "free")
ggplot(A2, aes(Sujets, fill = Groupe)) + geom_histogram(binwidth = 1) + facet_grid(Groupe ~., margins = TRUE, scales = "free")

#####Avec le nombre de forme forte####

mod_poisson <- glm(data=A2, Sujets ~ as.factor(annee_de_signature_de_la_convention), family = poisson)
tidy(mod_poisson)
summary(mod_poisson)
glance(mod_poisson)


mod_poisson <- glm(data=A2, Sujets ~ Groupe, family = poisson)
tidy(mod_poisson)
summary(mod_poisson)
glance(mod_poisson)


mod_poisson <- glm(data=A2, Sujets ~ niveaux_de_labelisation, family = poisson)
tidy(mod_poisson)
summary(mod_poisson)
glance(mod_poisson)
