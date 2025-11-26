
# construction distribution 
names(pat2025)

pat2025$annee_dobtention_de_la_derniere_labellisation

pat2025$annee_de_signature_de_la_convention
sum(!is.na(pat2025$annee_de_signature_de_la_convention))/462


pat2025$annee_de_signature_de_la_convention<- as.factor(pat2025$annee_de_signature_de_la_convention)
summary(pat2025$annee_de_signature_de_la_convention)
