
load(file = "data/pat2025.RData")
load(file = "data/df_textes.RData")

desc.pat <- data.frame(cbind(id = pat2025$id, descr = pat2025$descriptions_libre_des_enjeux_du_territoire_par_le_porteur))
desc.pat$num <- as.numeric(rownames(desc.pat))

rownames(df_textes) = gsub(x= df_textes$doc, pattern = "text", replacement = "")
df_textes$num = as.numeric(rownames(df_textes))
          
pat.df = left_join(desc.pat, df_textes, by="num")

rownames(pat.df) <- pat.df$id
pat.df <- pat.df[,c(2,5)]

save(pat.df, file = "data/pat.df.RData")
