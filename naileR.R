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