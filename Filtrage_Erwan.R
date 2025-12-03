library(topicmodels)
library(tidytext)
library(stringr)
library(Xplortext)
library(quanteda)
library(udpipe)

pat2025 <- read.csv("data/pats-20250710-win1252.csv", header = T, sep = ";", fileEncoding = "CP1252", dec = ".")

#### Exemple pour le format pris par LDA  ####                  
data("AssociatedPress", package = "topicmodels")
eb_t <- tidy(AssociatedPress)

eb_t$document <- as.factor(eb_t$document)
print(AssociatedPress)
#Format du jdd

# i : numéro du document
length(unique(AssociatedPress$i)) # 2246 documents comme afficher dans le print
# j : indice du mot
length(unique(AssociatedPress$dimnames$Terms)) # 2246 documents comme afficher dans le print
AssociatedPress$dimnames$Terms[1] # mot avec l'indice 1
AssociatedPress$v[which(AssociatedPress$j==1)] # fréquence des mots avec le mot aaron
# v : nombre de fois que le mot apparait dans un document

#### Mise en place des données au format pour la LDA ####
pat2025[, 78] <- str_squish(pat2025[, 78]) #Suppression des doubles/triples espaces et des espaces en bout de phrase

### Listes des stopwords ###
fr_stopwords <- read.table("data/stopwords-fr.txt")
fr_stopwords <- as.character(fr_stopwords$V1)

### Fusion avec les stopwords de Xplortext extrais ###
fr_stopwords_2 <- unlist(str_extract_all("à ai aie aient aies ait as au aura aurai auraient aurais aurait auras aurez auriez aurions aurons auront aux avaient avais avait avec avez aviez avions avons ayant ayez ayons c ce ceci cela celà ces cet cette d dans de des du elle en es est et étaient étais était étant été étée étées êtes étés étiez étions eu eue eues eûmes eurent eus eusse eussent eusses eussiez eussions eut eût eûtes eux fûmes furent fus fusse fussent fusses fussiez fussions fut fût fûtes ici il ils j je l la le les leur leurs lui m ma mais me même mes moi mon n ne nos notre nous on ont ou par pas pour qu que quel quelle quelles quels qui s sa sans se sera serai seraient serais serait seras serez seriez serions serons seront ses soi soient sois soit sommes son sont soyez soyons suis sur t ta te tes toi ton tu un une vos votre vous y plusieurs d’accord hélas peut-être donc pourtant autour derrière dessous dessus
devant parmi vers durant pendant depuis afin malgré sauf dès lorsque parce pendant pourquoi dedans loin partout aujourd'hui aussitôt autrefois avant-hier bientôt d'abord déjà demain en ce moment hier enfin longtemps maintenant quelquefois soudain souvent assez aussi autant davantage presque debout mieux sinon brusquement exactement doucement facilement heureusement lentement sagement seulement tranquillement", boundary("word")))

fr_stopwords <- unique(c(fr_stopwords,fr_stopwords_2))
fr_stopwords <- paste0("\\b(", paste(fr_stopwords, collapse = "|"), ")\\b")

####Méthode chatgpt####

txt <- pat2025[, 78]
txt <- gsub("([a-z])([A-Z])", "\\1 \\2", txt)


#### Création du filtre des villes ####
#On va mettre plusieurs niveaux de filtres :
# - le nom des villes écrit en toutes les lettres avec leurs majuscules et leurs tirés 
# (ainsi on évite de supprimer des mots potentiels comme "port" comme dans Confolent-Port-Dieu)
# - on split les noms de villes aux niveaux des tirets et on garde seulement les mots en majuscules

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
txt <- str_squish(txt)

#Filtre apostrophes
txt <- str_replace_all(txt, c("l'"="","d'"="","l’" = "", "d’" = ""))
txt <- "un une un une une un"
txt_final <- str_replace_all(txt,fr_stopwords,"")
toks <- tokens(txt_final,
               what = "word",
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE)







#### Nul ####
descri_PAT <- TextData(pat2025, var.text=c(78),
                        context.quali = c(1),
                        idiom="fr",
                        stop.word.tm = TRUE,
                        lower = TRUE,
                        Dmin = 5, Fmin=43,
                        lminword = 2,
                        remov.number = TRUE,
                       stop.word.user = fr_stopwords,
                        segment = TRUE)
A <- as.matrix(descri_PAT$DocTerm)














# ```{r}
# library(FactoMineR)
# library(Factoshiny)
# pat2025[, 256:300][is.na(pat2025[, 256:300])] <- 0
# pat_ACP <- pat2025[,c(256:300)]
# 
# ACP_pat <- PCA(pat_ACP,graph = FALSE)
# plot.PCA(ACP_pat)
# 
# AFC_pat <- CA(pat_ACP,graph = FALSE)
# eig_hist <- data.frame(AFC_pat$eig[,2])
# colnames(eig_hist) <- "eig"
# 
# eig_hist %>% 
#   ggplot(aes(x=factor(1:nrow(eig_hist)),y=eig))+
#   geom_col(fill="darkblue")
# 
# plot.CA(pat_AFC)
# plot.CA(pat_AFC,invisible = c("row"))
# 
# # pat2025[which(n_et_e$niveau == "Labellisation Niveau 2" & n_et_e$steps %in% c(1:3)),3]
# # pat2025[which(n_et_e$niveau == "Labellisation Niveau 1" & n_et_e$steps %in% c(4:6)),c(3,6)]
# # pat2025[which(pat2025$niveaux_de_labelisation == "Labellisation Niveau 1" & pat2025$annee_dobtention_de_la_derniere_labellisation < "2021"),c(3,6)]