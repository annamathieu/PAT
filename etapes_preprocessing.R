
#############################################################
# ETAPE 1 : bonne importation 
 # apostrophes 

library(tidyverse)
library(dplyr)

pat2025 <- read.csv("data/pats-20250710-win1252.csv", header = T, sep = ";", fileEncoding = "CP1252",
                    dec = ".") # CP1252 permet de gérer les apostrophes non détectées 
pat2025[pat2025== ""] <- NA # transformation des cases vides en NA
pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur <- gsub("’", "'", pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur, fixed = TRUE)

# df pour desc 
textdata <- as.data.frame(cbind(doc_id = pat2025$nom_administratif, text = pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur))

# gérer le pb des sauts de lignes non pris en compte
textdata$text <- gsub("(?<=[a-z])(?=[A-Z])", " ", x =  textdata$text, perl = TRUE) # décole les MAJ collées à des minuscules (précédées par des minuscules)

# supp db espace doubles ou +
textdata$text <- str_squish(textdata$text)
txt <- textdata$text


############################################################

# ETAPE 2 : FILTRES sur les villes + apostrophes 

villes <- unlist(strsplit(pat2025$communes_nom, split = ", ")) #On récupère les noms des villes tels que donnés 

villes_maj <- gsub(pattern = "-",, replacement = " ", x = villes, fixed = TRUE) 
villes_maj <- unlist(strsplit(villes_maj, split = " "))
villes_maj <- grep("^[A-Z]", villes_maj, value = TRUE)#On récupère les mots en majuscules dans les noms des villes 

villes_filtre <- c(villes,villes_maj)

escape_regex <- function(x) {
  str_replace_all(x, "([\\.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1")
}
villes_filtre <- escape_regex(villes_filtre) #Pour permettre d'appliquer le filtre avec les charactères spéciaux
villes_filtre <- paste0("\\b(", paste(villes_filtre, collapse = "|"), ")\\b") #Empêcher de reconnaitres des noms de ville en début de phrase "Port" --> "Portrait"

##### Application du filtre #####

txt_clean <- str_replace_all(string=txt, villes_filtre,"")
txt <- tolower(txt_clean)

#Filtre apostrophes
txt <- str_replace_all(txt, c("l'"="","d'"="","l’" = "", "d’" = ""))


###########################################################

# Etape 3 : Retirer les stop words

# FILTRE 1 

### Listes des stopwords ###
fr_stopwords <- read.table("data/stopwords-fr.txt")
fr_stopwords <- as.character(fr_stopwords$V1)

filter1 <- paste0("\\b(", paste(fr_stopwords, collapse = "|"), ")\\b")

txt_filter1 <- str_replace_all(string=txt, filter1,"")


# filtre 2 
### Fusion avec les stopwords de Xplortext extrais ###
fr_stopwords_2 <- unlist(str_extract_all("à ai aie aient aies ait as au aura aurai auraient aurais aurait auras aurez auriez aurions aurons auront aux avaient avais avait avec avez aviez avions avons ayant ayez ayons c ce ceci cela celà ces cet cette d dans de des du elle en es est et étaient étais était étant été étée étées êtes étés étiez étions eu eue eues eûmes eurent eus eusse eussent eusses eussiez eussions eut eût eûtes eux fûmes furent fus fusse fussent fusses fussiez fussions fut fût fûtes ici il ils j je l la le les leur leurs lui m ma mais me même mes moi mon n ne nos notre nous on ont ou par pas pour qu que quel quelle quelles quels qui s sa sans se sera serai seraient serais serait seras serez seriez serions serons seront ses soi soient sois soit sommes son sont soyez soyons suis sur t ta te tes toi ton tu un une vos votre vous y plusieurs d’accord hélas peut-être donc pourtant autour derrière dessous dessus
devant parmi vers durant pendant depuis afin malgré sauf dès lorsque parce pendant pourquoi dedans loin partout aujourhui aussitôt autrefois avant-hier bientôt d'abord déjà demain en ce moment hier enfin longtemps maintenant quelquefois soudain souvent assez aussi autant davantage presque debout mieux sinon brusquement exactement doucement facilement heureusement lentement sagement seulement tranquillement st", boundary("word")))

filter2 <- paste0("\\b(", paste(fr_stopwords_2, collapse = "|"), ")\\b")

txt_filter2 <- str_replace_all(string=txt_filter1, filter2,"")


txt_final <- str_squish(txt_filter2)

#######################################################

# Etape 4 : TOKENISATION 

toks <- tokens(txt_final,
               what = "word",
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE)


#######################################################

# Etape 5 : créer df à partir des tokens 

tokens_df <- data.frame(
  doc = rep(names(toks), lengths(toks)),
  token = unlist(toks)
)



#######################################################

# Etape 5 : Lemmatisation 

# 1 : lemmatisation avec lemmar 
install.packages("remotes")
remotes::install_github("trinker/lemmar")
library(lemmar)

data("hash_lemma_fr")
hash_lemma_fr$token <- tolower(hash_lemma_fr$token)
hash_lemma_fr$lemma <- tolower(hash_lemma_fr$lemma)

res.lemmat <- left_join(x=tokens_df, y = hash_lemma_fr, by = join_by(x$token==y$token))

# perf 1ere lemmat
length(unique(res.lemmat$token[which(is.na(res.lemmat$lemma))]))



# 2 : lemmatisation avec mixr 


library(dplyr)
library(mixr)
library(remotes)
remotes::install_github("lvaudor/mixr")


unique(res.lemmat$token[which(is.na(res.lemmat$lemma))])

lexique382=mixr::get_lexicon(language = "fr")
res.lemmat <- left_join(x=tokens_df, y = hash_lemma_fr, by = join_by(x$token==y$token))
res.lemmat <- left_join(x=res.lemmat, y = lexique382, by = join_by(x$token==y$word))


lemma3 <- read.delim("data/lemmatization-fr.txt", header = TRUE, stringsAsFactors = FALSE)
colnames(lemma3) <- c("token","lemma")
#Indicateur du nombre du mots qui nous manque
unique(res.lemmat$token[which(is.na(res.lemmat$lemma.y))])




