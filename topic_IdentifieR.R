# Travailler sur une fonction qui a partir d'un corpus donne les topics renforcés et identifie leur nom 

# Input : corpus : | prétraité | ou non ? | 
# La fonction a besoin d'un tableau de données de la forme suivante : 
# X : soit 
    # 2 colonnes, une avec le texte d'origine du terme, et la 2e avec le mot lui même, autant de lignes que de couples mots x textes
    # 2 colonnes, une avec le texte d'origine du terme, et l'autre avec le texte prétraité (ensemble des mots), autant de ligne que de textes
# => format_x 

# Hyperparamètres 
  # nombre de lda réalisées = nb.iter 
  # format_x 
  # taille_min_text : nombre de termes lemmatisés min pour conserver le doc
  # api_key = clé api pour requetes nailR
  # limit_inf : seuil supérieur du nombre de docs dans lequel un doc apparait pour qu'un terme doit dépasser pour être conservé dans le vocabulaire 
  # nb.frex = 20 : nombre de termes frex caractéristiques de chaque topic de chaque modele retenus
  # frex.threshold = 0.5, seuil frex , entre 0 et 1 : 0 = fréquence 100 % et 1 = exclusivité 100 % 


# Output 
  # résumé des settings 
      # seed, nb.lda, ... 
  # vecteurs de composition fortifiés
  # nom des topics (automatique)




#####################
# questions / problèmes à régler : 

# la fonction fait elle 'une' proposition ou bien identifie t-elle directement les valeurs des hyperparamètres à choisir ? 
# (pour obtenir les meilleurs topics ?)

# la fonction peut soit réaliser le prétraitement ou non avec argument : preprocessing = "T" ou "F" 

# choix de la langue de communication (texte ou requetes), argument : language = "eng" ou "fr" (par défaut)

# gérer le pb des seeds qui vont bouger à chaque fois ; le gérer en externe (demander avant de le faire ?)

# Gérer le format initial du tableau de données 

# stop words = "base" ou "personalized" 

# liste stop words supp : liste perso de stop words 

# hyperparamètre filtre mfa df 

# 

######################





######################
# pour garder tjs les mêmes seeds
nb.iter = 10
seeds = sample(1:9999, nb.iter, replace = F)

topicIdentifieR <- function ( X, 
                              nb.topics, 
                              nb.iter = 10, 
                              taille_min_text = 20,
                              api_key, 
                              preprocessing = TRUE, 

                              liste_stopwords_supp = NULL, 
                              language = c("fr", "english"),
                              
                              
                              limit.inf = 1, 
                              nb.frex = 20, 
                              frex.threshold = 0.5, 
                              ncp = 5,
                              freq.min.term = 3,
                              
                              introduction_llm = NULL
                              )
{
  
  library(stm)        # lda 
  library(FactoMineR) # Analyses factorielles et classification 
  library(mixr)       # get lexicon pour lemmatisation 
  library(NaileR)     # pour requetes LLM
  

  # ==== Progra défensive ====
  
  
  
  

  # ==== Gestion du type de tableau de données ==== 
  
  
    # format de base : deux colonnes avec 1) n°/nom du texte 2) texte
    colnames(X) = c("id", "text")
  
  if (preprocessing ==TRUE)  {
    # nettoyage 
    X$text <- gsub("(?<=[a-z])(?=[A-Z])", " ", x =  X$text, perl = TRUE) # décole MAJ collées à  minuscules (précédées par des minuscules)
    X$text <- tolower(str_squish(X$text)) # supp db espaces  + on passe en minuscules
    X$text <- str_replace_all(X$text, c("l'"="","d'"="","l’" = "", "d’" = "")) #supp apostrophes 
    
    X$text <- gsub("\\.", " ", x = X$text, perl = TRUE) # mettre des espaces à la place des points
    X$text <- gsub("(?<=[0-9])(?=[\\p{L}])|(?<=[\\p{L}])(?=[0-9])", " ", X$text, perl = TRUE)  # mettre des espaces s'il y a un chiffre avant une lettre
    X$text <- gsub("(?<=[0-9])-(?=[\\p{L}])|(?<=[\\p{L}])-(?=[0-9])|(?<=[0-9])-(?=[0-9])", " ", X$text, perl = TRUE) # chiffre'-'lettre ou lettre'-'chiffre ou chiffre'-'chiffre
    X$text <- gsub(" -", " ", X$text) # supprimer tirets en début de mot
    
    # stop words 
    stopwords <- stopwords::stopwords(language)
    
    if (language=="fr") {
      stopwords_fr <- unlist(read_table("https://raw.githubusercontent.com/stopwords-iso/stopwords-fr/master/stopwords-fr.txt", show_col_types = FALSE))
      stopwords <- unique(c(stopwords, stopwords_fr))
      }
    
    if (language=="english") {
      stopwords_eng <- tidytext::stop_words$word
      stopwords <- unique(c(stopwords,stopwords_eng)) 
    }
       
      # liste supp a ajouter 
    if (!is.null(liste_stopwords_supp)) {
      stopwords <- unique(c(stopwords, stopwords_supp))
    }
    
      # on retire les stop words
    filter <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")
    X$text <- str_squish(str_replace_all(string=X$text, filter,"")) 
    
    print("--------------------------------------------")
    print("Data cleaning : Done")
}
    
    # tokenisation 
    res.tokens <- quanteda::tokens(X$text,
                   what = "word",
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_numbers = TRUE)
    
    
    df_X_long <- data.frame(id = rep(names(res.tokens), lengths(res.tokens)),
                            text = unlist(res.tokens)
    )
    
    # lemmatisation 
    if (language == "english"){
      language = "en"  
    } 
      
    lexique <- mixr::get_lexicon(language = language)[,1:2]
    colnames(lexique) = c("text", "lemma")
    lexique <- lexique[!duplicated(lexique$text), ]
    
    df_X_long <- left_join(df_X_long, lexique, by = "text") 
    df_X_long$lemma <- ifelse(is.na(df_X_long$lemma), df_X_long$text, df_X_long$lemma)
    df_X_long <- df_X_long[,c(1,3)]  
    colnames(df_X_long) = c("id", "word")
    
    # filtre de taille de chaque doc : avec taille_min_text
    df_X_long <- df_X_long %>% 
      group_by(id) %>%
      mutate(n_lignes = n()) %>% 
      filter(n_lignes >= taille_min_text)
    
      # on calcule : freq des mots 
    df_X_long <- df_X_long %>%
      group_by(id, word) %>%
      summarise(freq = n(), .groups = 'drop')
    
    df_X_long$id <- as.numeric(gsub("text","", df_X_long$id))
    df_X_long <- df_X_long %>% arrange(id)

    print("--------------------------------------------")
    print("Tokenization and lemmatization : Done")
    
    

  # ==== Construction du vocabulaire ====
  vocab <- unique(df_X_long$word)
  unique_docs <- unique(df_X_long$id)
  
  word_to_idx <- stats::setNames(seq_along(vocab), vocab)
    
  documents <- lapply(unique_docs, function(current_doc) {
    # Filter words for this document
    words_in_doc <- df_X_long %>% filter(id == current_doc)
    
    # Convert words to indices
    indices <- as.integer(word_to_idx[words_in_doc$word])
    counts <- as.integer(words_in_doc$freq)
    
    # Create 2-row matrix: row 1 = indices, row 2 = counts
    matrix(c(indices, counts), nrow = 2, byrow = TRUE)
  })
  
  names(documents) <- unique_docs
  
  df_stm <- stm::prepDocuments(documents, vocab, 
                       lower.thresh = limit.inf,  # remove words appearing in only 1 doc
                       upper.thresh = Inf)
  
  documents <- df_stm$documents
  vocab <- df_stm$vocab
  
  

  # ==== Construction des modèles ====
  
  lda.model <- function(k, seed) {
    topic_model<-stm(documents, 
                     vocab,
                     K=k, verbose=FALSE, init.type = "LDA", 
                     seed = seed)
    return(topic_model)
  }
  
    # construction des modèles
  models <- list()
  for (i in 1:nb.iter){
    models[[i]] <-  lda.model(k = nb.topics, seeds[i])
  }
  
    # récupération des FREX 
  words_frex <- list()
  
  for (model in seq_along(models)){
    frex_mat <- t(stm::labelTopics(models[[model]], n = nb.frex, frexweight = frex.threshold)$frex)
    # n: choix du nombre de frex , frexweight : optimisation frequence / exclusivité
    
    words_frex[[model]] <- vector("list", ncol(frex_mat))
    
    for ( topic in seq_len(ncol(frex_mat))  ) {
      words_frex[[model]][[topic]] <- frex_mat[,topic]
    }
  }
   
  
    # récupération des frex uniques
  words_frex_unique <- unique(unlist(words_frex))
  
  print(" \n--------------------------------------------")
  print("Topic modelling models : Done")
  

  # ==== Fortification des topics ====
  
    # on construit le jeu de données permettant la solidification des topics 
  df_mfa = data.frame(matrix(ncol = nb.topics * nb.iter, nrow = length(words_frex_unique), NA))
  colnames(df_mfa) <- paste0("lda", rep(1:nb.iter, each = nb.topics), "_topic", rep(1:nb.topics, times = nb.iter))
  rownames(df_mfa) <- words_frex_unique 
  
    # on remplit df_mfa
  for (i in 1:nb.iter) {
    for (j in 1:nb.topics) {
      col_name <- paste0("lda", i, "_topic", j)
      
      # récupération des mots du topic j de la lda i
      words_in_topic <- words_frex[[i]][[j]]
      # lda_list[[i]][[j]] retourne les mots du topic j de la lda i
      
      # pour chaque mot du dataframe
      df_mfa[[col_name]] <- ifelse(
        rownames(df_mfa) %in% words_in_topic,
        1,  # si le mot est dans ce topic
        0  
      )
    }
  }
  
    # filtrage : supp les mots de faible fréquence 
    df_mfa$freq <- rowSums(df_mfa)
    df_mfa <- df_mfa %>% filter(df_mfa$freq>=freq.min.term) %>% select(-"freq")
    

    # MFA
    res.mfa <- MFA(base = df_mfa, 
                   group = rep(nb.topics,nb.iter),
                   type = rep("f",nb.iter), 
                   name.group = paste0("lda",seq(1:nb.iter)),
                   graph = F 
    
    ) 
    
  
    barplot(res.mfa$eig[,2])
    
    res.mfa <- MFA(base = df_mfa, 
                   group = rep(nb.topics,nb.iter),
                   type = rep("f",nb.iter), 
                   name.group = paste0("lda",seq(1:nb.iter)),
                   graph = F, 
                   ncp = ncp 
                   
    )
    
    res.hcpc <- FactoMineR::HCPC(res = res.mfa, nb.clust = -1, graph = F)
    
    df_topics_clust <- data.frame(cbind(
                              word = rownames(df_mfa),
                              clust = res.hcpc$data.clust$clust, 
                              dim1 = res.mfa$ind$coord[,1], 
                              dim2 = res.mfa$ind$coord[,2]))

      # liste des mots des topics fortifiés 
    words_topics_fortifies <- unname(split(df_topics_clust$word, df_topics_clust$clust))
    
    
    print("--------------------------------------------")
    print("Fortification of strong forms topics : Done")

    
  # ==== Identfication des topics automatiquement avec modele de langage ====
    Sys.setenv(GEMINI_API_KEY = api_key)
    
    cat(gemini_generate(nail_textual(dataset = df_topics_clust[,1:2],num.text = 1, num.var = 2, 

                      introduction = c(introduction_llm,"A study on a topic is achieved and we want to find the topics discussed in the given documents. We use structural topic modelling with variational Expectation Maximisation algorithms. We consolidate our topics by using multiple executions of the algorithm and perform MFA to find strong forms in our topics."),
                      
                      request = "We want to automatically put the best name on each of those topics based on the words caracterising them. Please give a name to each topic suming up most of the words caracterising it. Do all of the explanations and name of topics in the language of the words. Only give the list of asigned topic and a short description of each.",
                      isolate.groups = F, drop.negative = T, generate = F)
                      )
        )


  # ==== Sortie de la fct ====
    
    # voir tous les jeux de données que je peux sortir en output 
  
  output <- list(X = X, 
                 X_long_preprocessed = df_X_long, 
                 X_tokenised = res.tokens,
                 frex_terms = words_frex,
                 df_mfa = df_mfa, 
                 df_topics_clust = df_topics_clust,
                 words_topics_fortifies= words_topics_fortifies,
                 res.mfa = res.mfa,
                 res.hcpc = res.hcpc
                 )
  
  return(output)
}
  


topicIdentifieR(X = textdata, 
                nb.topics = 9, 
                nb.iter = 10, 
                taille_min_text = 20, 
                api_key = "AIzaSyA1UCATqGqUpQuVRJBPEKTcilR9H-OF-g4", 
                language = "fr"  )
  


# hyperparamètres pour le test 
language = "fr"
nb.iter = 10
nb.topics = 9
limit.inf = 1
taille_min_text = 20
X = textdata
nb.frex = 20  
frex.threshold = 0.5
ncp = 5

###################################################################
# test de topicIdentifieR sur une autre colonne du jeu de données 
library(tidyr)
library(tidyverse)
load(file = "data/pat2025.RData")

pat2025[,146:150][is.na(pat2025[,146:150])] <- ""

text.objectifs <- as.data.frame(cbind(
                              doc_id = pat2025$nom_administratif, 
                              
                              unite(data = pat2025[,c(146:150)], 
                              col = "text", 
                              sep = " ")))

text.objectifs$text <- str_squish(text.objectifs$text)

text.objectifs[text.objectifs==""] <- NA

nb.iter = 10
seeds = sample(1:9999, nb.iter, replace = F)

res2 = topicIdentifieR(X = text.objectifs, 
                       nb.topics = 10, 
                       nb.iter = 10, 
                       taille_min_text = 20,
                       nb.frex = 20,
                       api_key = "AIzaSyCT_RzdrUtkY5TdvPetzLvaVP5j-f6XbAA", 
                       language = "fr"  )

res3 = topicIdentifieR(X = text.objectifs, 
                      nb.topics = 8, 
                      nb.iter = 10, 
                      taille_min_text = 20,
                      nb.frex = 10,
                      api_key = "AIzaSyCT_RzdrUtkY5TdvPetzLvaVP5j-f6XbAA", 
                      language = "fr"  )

res4 = topicIdentifieR(X = text.objectifs, 
                       nb.topics = 8, 
                       nb.iter = 10, 
                       taille_min_text = 20,
                       nb.frex = 10,
                       freq.min.term = 2,
                       api_key = "AIzaSyCT_RzdrUtkY5TdvPetzLvaVP5j-f6XbAA", 
                       language = "fr"  )

res5 <- topicIdentifieR(X = text.objectifs, 
                               nb.topics = 6, 
                               nb.iter = 10, 
                               taille_min_text = 20,
                               nb.frex = 10,
                               freq.min.term = 2,
                               api_key = "AIzaSyCT_RzdrUtkY5TdvPetzLvaVP5j-f6XbAA", 
                               language = "fr"  )
res5$words_topics_fortifies

