U_stoplist<-c("bien","bonne","tout")
res.TD<-TextData(base_Fr,var.text=c(9,10), idiom="fr", var.agg="Sexe_Age", Fmin=100, Dmin=80,
                 stop.word.user=U_stoplist, stop.word.tm=TRUE,,graph=FALSE)
summary(res.TD,ndoc=0)



# stoplist : fr_stopwords 

library(FactoMineR)
library(Xplortext)

preprocessed.pat <- TextData(base = textdata, var.text= 2, idiom = "fr", context.quali = 1,
                             lower = T, remov.number = T, lminword = 3, stop.word.tm = T, 
                             stop.word.user = fr_stopwords
                             )

summary(preprocessed.pat)

descr.new <- preprocessed.pat$SourceTerm.qual
tab <- preprocessed.pat$info$base[[1]]
preprocessed.pat$info$var.text[[1]]
