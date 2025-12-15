
# construction distribution 
names(pat2025)

pat2025$annee_dobtention_de_la_derniere_labellisation

pat2025$annee_de_signature_de_la_convention
sum(!is.na(pat2025$annee_de_signature_de_la_convention))/462


pat2025$annee_de_signature_de_la_convention<- as.factor(pat2025$annee_de_signature_de_la_convention)
summary(pat2025$annee_de_signature_de_la_convention)




# Comment définit t-on l'ancienneté d’un PAT ? Utilise t-on les niveaux 1 et 2 ?   (ou les niveaux 1 à 6, plus précis) 

library(ggplot2)
library(stringr) # faire des comptages 
library(utils)
library(tidyverse)
library(dplyr)


# peut on faire un graph de distribution des PAT sur les étapes de 1 à 6 selon s'ils sont niveau 1 ou niveau 2

n_et_e<- as.data.frame(cbind(niveau = pat2025$niveaux_de_labelisation, 
                             steps = pat2025$les_etapes_cles_conduites_et_realisees ))

summary(n_et_e)

trans_etapes <- function(etapes){
  
  for (j in 1:length(etapes)){
    if (is.na(etapes[j])) {
      etapes[j] <- ""
      next
    }
    
    virg <- str_count(string = etapes[j], pattern = ",") # compte le nombre de virgules => virg + 1 = nombre d'étapes 
    etapes[j] <- str_split_i(string = etapes[j], pattern= ", ", i = virg+1) # couper un espace après la virgule et garder le dernier élément (de num virg + 1)
    # conserve la dernière étape
  
    if (etapes[j]=="plans d'actions établis par délibération"){
     etapes[j]<- "Trois ou plus, plans d'actions établis par délibération"
    } 
  }
  # Transformation en nombre
  etapes <- case_when(
    etapes == "Concertation" ~ 1,
         etapes == "Stratégie du projet construite et validée" ~ 2,
         etapes == "Premier plan d'action établi par une délibération" ~ 3,
         etapes == "Mise en oeuvre du premier plan d'action" ~ 4,
         etapes == "Second plan d'action établi par une délibération" ~ 5,
         etapes == "Trois ou plus, plans d'actions établis par délibération" ~ 6,
         etapes == "" ~ NA
  )
}

n_et_e$steps <- as.factor(trans_etapes(n_et_e$steps))
n_et_e$niveau <- as.factor(n_et_e$niveau)




n_et_e %>% ggplot() +
  aes(x = steps, group = niveau, fill = niveau) +
  geom_bar(col="black") +
  scale_fill_manual(values = c("#F2C799","#F0921E","#C75318")) +
  labs(fill = "Niveau du PAT")+
  ylab("Nombre de PAT") +
  xlab("Avancement du PAT") +
  scale_x_discrete(labels = c(
    "Concertation",
    "Stratégie construite",
    "Premier plan d'action établi",
    "Mise en oeuvre 1er plan",
    "Second plan d'action établi","≥3 plans établis"
  ))+
  ggtitle(label = "Etape d'avancement la plus avancée réalisée par PAT selon leur niveau - Novembre 2025")+
  theme(panel.background = element_rect(fill="white"),
        plot.title = element_text(face="bold",hjust=0.2,size=15), 
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        panel.grid.major = element_line(colour = "grey85"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(vjust=0.65,angle=50,size=12,colour="black"))


