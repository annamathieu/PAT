# Réalier les 10 LDA 


seeds = sample(1:9999, 10, replace = F)
k = 9
nb_lda = length(seeds)

model1 <- lda.model(k = k, seed = seeds[1])
model2 <- lda.model(k = k, seed = seeds[2])
model3 <- lda.model(k = k, seed = seeds[3])
model4 <- lda.model(k = k, seed = seeds[4])
model5 <- lda.model(k = k, seed = seeds[5])
model6 <- lda.model(k = k, seed = seeds[6])
model7 <- lda.model(k = k, seed = seeds[7])
model8 <- lda.model(k = k, seed = seeds[8])
model9 <- lda.model(k = k, seed = seeds[9])
model10 <- lda.model(k = k, seed = seeds[10])

words1 <- topic.extraction(model1)
words2 <- topic.extraction(model2)
words3 <- topic.extraction(model3)
words4 <- topic.extraction(model4)
words5 <- topic.extraction(model5)
words6 <- topic.extraction(model6)
words7 <- topic.extraction(model7)
words8 <- topic.extraction(model8)
words9 <- topic.extraction(model9)
words10 <- topic.extraction(model10)



lda_list <- list(words1,words2,words3,words4,words5,words6,words7,words8,words9,words10)
# on créé la liste avec les mots de chaque lda : n objets, contenant chacun k éléments (9 ici)

words <- unique(unlist(lda_list))# récupére la liste unique des mots de toutes les lda
# ajouter toutes les listes de mots 



# Créer le data frame 

col = sort(paste0("lda",seq(1:nb_lda), rep(paste0("topic", seq(1:k)),nb_lda)))
# on crée un vecteur avec les noms de colonnes avec x lda et n topics, et on le met dans l'ordre des lda

mfa.df = data.frame(matrix(ncol = k *nb_lda, nrow = length(words), NA))
# on créé le df => 1 + k * n colonnes 
colnames(mfa.df) <- col
# on met les noms de colonnes issus du vecteur col
rownames(mfa.df) <- words 
# on ajoute les mots dans la colonne words



# filtre : si un mot apparait moins de 5 fois en tout, on le retire
sumrows <- apply(mfa.df, MARGIN = 1, FUN = sum)
sumrows <- ifelse(sumrows <  5, NA, sumrows )

mfa.df.filtre <- data.frame(c(mfa.df, sumrows = sum(mfa.df[,1:nb_lda*k])))
row.names(mfa.df.filtre) = row.names(mfa.df)

# On va ensuite associer à chaque lda les mots des différents topics 

for (i in 1:nb_lda) {
  for (j in 1:k) {
    col_name <- paste0("lda", i, "topic", j)
    
    # récupération des mots du topic j de la lda i
    words_in_topic <- lda_list[[i]][[j]]
    # lda_list[[i]][[j]] retourne les mots du topic j de la lda i
    
    # pour chaque mot du dataframe
    mfa.df[[col_name]] <- ifelse(
      rownames(mfa.df) %in% words_in_topic,
      1,  # si le mot est dans ce topic
      0  
    )
  }
}

library(FactoMineR)
library(Factoshiny)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(plotly)

res.mfa <- MFA(base = mfa.df, 
              group = rep(k,nb_lda),
              type = rep("f",nb_lda), 
              name.group = paste0("lda",seq(1:nb_lda)), 
              ncp = 5
              # graph = F
              )

barplot(res.mfa$eig[1:40,2])

Factoshiny(res.mfa)

res.hcpc <- HCPC(res = res.mfa, nb.clust = 8)
summary(res.hcpc)

clust <- data.frame(cbind(word = rownames(mfa.df),
                          clust = res.hcpc$data.clust$clust, 
                          dim1 = res.mfa$ind$coord[,1], 
                          dim2 = res.mfa$ind$coord[,2]))

str(clust)
clust$clust <- as.factor(clust$clust)
clust$word <- as.factor((clust$word))


clust %>% 
  ggplot(aes(x = dim1, y = dim2, color = clust)) +
  geom_point(size = 2) +
  geom_text_repel(
    aes(label = word),
    size = 3,
    max.overlaps = Inf,
    force = 1,
    box.padding = 0.2
  ) +
  labs(color = "Cluster",
       x = "Dimension 1",
       y = "Dimension 2",
       title = "Projection des mots dans l'espace factoriel (Dim 1 & Dim 2)") +
  theme_minimal(base_size = 14)


library(plotly)

plot_ly(
  data = clust,
  x = ~dim1,
  y = ~dim2,
  type = "scatter",
  mode = "markers+text",
  color = ~factor(clust),
  text = ~word,       # nom de ta colonne contenant les mots
  textposition = "top center",
  marker = list(size = 9),
  hoverinfo = "text"
) %>%
  layout(
    title = "Projection des mots dans l'espace factoriel (Dim 1 & Dim 2)",
    xaxis = list(
      title = "Dimension 1",       # supprime le titre
      showticklabels = FALSE,  # supprime les graduations
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Dimension 2",
      showticklabels = FALSE,
      zeroline = FALSE
    ),
    legend = list(title = list(text = "Cluster"))
  )


# passer les 0 en non et les 1 en oui => tester en catégoriel 

####### test significativite > hasard composantes 
simul <- function(nbsimul,nind,nvar){
  res <- rep(0,times = nbsimul)
  for (i in 1:nbsimul){
    dta <- as.data.frame(matrix(rnorm(nind*nvar),nrow=nind))
    ACP <- PCA(dta,graph = FALSE)
    res[i] <- ACP$eig[2,3]
  }
  quant <- quantile(res,probs=c(0.95))
  return(quant)
}

res <- simul(100, nrow(mfa.df), ncol(mfa.df))
res
######################### 

