pat2025[,78] <- gsub("https?://\\S+", "", pat2025[,78])
res.lemmat.NA <- res.lemmat %>% filter_at(vars(lemma.x,lemma.y,lemma),all_vars(is.na(.)))
res.lemmat.NAB <- data.frame(x=unique(res.lemmat.NA$token))
write.csv2(x=res.lemmat.NAB,"lemma_complet.csv",fileEncoding = "latin1")
