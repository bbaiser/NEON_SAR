
if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)



df2 <- data_beetle %>% 
       mutate(present = 1) %>% 
       pivot_wider(names_from = taxon_name, values_from = present) %>% 
       mutate_at(vars(unique(data_beetle$taxon_name)), ~ifelse(is.na(.), 0, 1))