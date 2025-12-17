plot_clust <- function(res.hcpc, res.mfa, mfa.df.final){
  
  clust <- data.frame(cbind(word = rownames(mfa.df.final),
                            clust = res.hcpc$data.clust$clust, 
                            dim1 = res.mfa$ind$coord[,1], 
                            dim2 = res.mfa$ind$coord[,2]))
  
  str(clust)
  clust$clust <- as.factor(clust$clust)
  clust$word <- as.factor((clust$word))
  clust$dim1 <- as.numeric(clust$dim1)
  clust$dim2 <- as.numeric(clust$dim2)
  
  plot <- plot_ly(
    data = clust,
    x = ~dim1,
    y = ~dim2,
    type = "scatter",
    mode = "markers+text",
    color = ~factor(clust),
    text = ~word,       # nom de ta colonne contenant les mots
    textposition = "top center",
    marker = list(size = 7),
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
  
  plot
}
