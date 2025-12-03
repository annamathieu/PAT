
scores <- cbind(score_culturel, score_economique[,-1], score_education[,-1,], 
                score_environnement[,-1], score_gouvernance[,-1], score_justice_sociale[,-1], 
                score_nutri_sante[,-1], score_resto[,-1], score_urbanisme[,-1])


library(FactoMineR)


res.mfa <- MFA(base = scores, 
               group = c(1,4,5,5,8,3,5,4,7,4), 
               type = c("n",rep("s",9)), 
               name.group = c("Id","Culture", "EcoAlim", "Education", "Env", "Gouv","JusticeSociale",
                              "Nutrition","RestoCo","Urbanisme"), 
               num.group.sup = 1)


