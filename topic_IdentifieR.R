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
  #
  # 
  # 

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



######################


library(stm)        # lda 
library(FactoMineR) # Analyses factorielles et classification 



######################
# pour garder tjs les mêmes seeds
nb.iter = 10
seeds = sample(1:9999, nb.iter, replace = F)

topicIdentifieR <- function ( X, 
                              format_X, 
                              nb.topics = k, 
                              nb.iter = 10, 
                              taille_min_text = 20,
                              
                              stopwords = "base",
                              liste_stopwords_supp = c(NULL, stopwords_supp), 
                              preprocessing = TRUE, 
                              language = c("fr", "english"), 
                              
                              )
{
  
  #################
  # Etape 0 : progra défensive et types 
  
  
  
  
  #################
  # Gestion du type de tableau de données 
  
    # format de base : deux colonnes avec 1) n°/nom du texte 2) texte
    colnames(X) = c("id", "text")
  
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
    if (language =="fr"){
      
    lexique <- mixr::get_lexicon(language = "fr")[,1:2]
    colnames(lexique) = c("text", "lemma")
    lexique <- lexique[!duplicated(lexique$text), ]
    
    df_X_long <- left_join(df_X_long, lexique, by = "text") 
    df_X_long$lemma <- ifelse(is.na(df_X_long$lemma), df_X_long$text, df_X_long$lemma)
    df_X_long <- df_X_long[,c(1,3)]  
    colnames(df_X_long) = c("id", "word")
    }
    
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

  #################
  # Construction du vocabulaire 
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
                       lower.thresh = 1,  # remove words appearing in only 1 doc
                       upper.thresh = Inf)
  
  documents <- df_stm$documents
  vocab <- df_stm$vocab
  
  
  #################
  # Construction des modèles 
  
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
  ######################################
  #############⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️
  #####################################
  words_frex
  
  
  words_frex <- lapply(X= models, function(x) as.list(t(stm::labelTopics(models[[1]], n = 20, frexweight = 0.5)$frex)))   # n: choix du nombre de frex , frexweight : optimisation frequence / exclusivité
  words_frex_unique <- unique(unlist(words_frex))
  
  
  
    #################
  # Fortification des topics 
  
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
  
  
  
  #################
  # Identfication des topics automatiquement avec modele de langage 
  
  
  
  
  # Sortie de la fct
  
  
  
  
}
  
  
  
  
  
  
  
topicIdentifieR(X = textdata)  
  
  
  
  
  