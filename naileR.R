library(NaileR)
library(dplyr)
data(car_alone)

  sampled_car_alone <- car_alone %>%
    group_by(car_alone_capable_restrictive) %>%
    dplyr::sample_frac(0.5)
  sampled_car_alone <- as.data.frame(sampled_car_alone)
  
    intro_car <- "Knowing the impact on the climate,
 I have made these choices based on
 the following benefits and constraints..."
    intro_car <- gsub('\n', ' ', intro_car) |>
           stringr::str_squish()
    
      res_nail_textual <- nail_textual(sampled_car_alone, num.var = 1,
                                                                           num.text = 2,
                                                                           introduction = intro_car,
                                                                           request = NULL,
                                                                           model = 'llama3', isolate.groups = F,
                                                                           generate = FALSE)
      cat(res_nail_textual )
      
      gemini_generate(res_nail_textual)
   
         
mots.clust <- data.frame(mots.clust)
mots.clust$clust <- seq(1,6)
      


res = nail_textual(dataset = clust[,1:2], num.var = 2, num.text = 1,
                  
                  introduction = "A study on Territorial food systems is done and we want to find the topics discussed in the description of each Territorial food system, we use topic modelling and latent dirichlet allocation. We consolidate our topics by using multiple executions of the algorithm and perform MFA to find strong forms in our topics.",
                  request = "We want to automatically put a name on those topics based on the words caracterising them. Please give a name to each topic suming up most of the words caracterising it. Do it in French. Only give la Liste des Noms de Groupe Attribués",
, 
                  isolate.groups = F, drop.negative = T, generate = F)
res

descr <- gemini_generate(res)



Sys.setenv(GEMINI_API_KEY = "AIzaSyDtqqOHs_t2EiVfh7SnmXMtuyIkBilS_0Q")

?gemini_generate

Sys.setenv(GEMINI_API_KEY = "AIzaSyCl1Sm2rtMwXfBfAGw9n2TZMYP")
gemini_generate(
  prompt = "Say hello in one short sentence.",
  model  = "gemini-2.5-flash",
  verbose = TRUE
)

Sys.setenv(GEMINI_API_KEY = "AIzaSyCl1Sm2rtMwXfBfAGw9n2TZMYP-aolgoow")

Sys.setenv(GEMINI_API_KEY = "AIzaSyA1UCATqGqUpQuVRJBPEKTcilR9H-OF-g4")

Sys.setenv(GEMINI_API_KEY = "AIzaSyCT_RzdrUtkY5TdvPetzLvaVP5j-f6XbAA")

Sys.setenv(GEMINI_API_KEY = "AIzaSyA4RWX271sierisgnKp0swxft9qh5uB3fY")
