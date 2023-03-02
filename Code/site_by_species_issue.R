
#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
library(dplyr)

install.packages("sars")
library(sars)


####Beetles####
#get pres/abs for each species per site
beetle_df <- data_beetle %>% 
  mutate(present = 1) %>% 
  pivot_wider(names_from = taxon_name, values_from = present) %>% 
  mutate_at(vars(unique(data_beetle$taxon_name)), ~ifelse(is.na(.), 0, 1))%>%
  select(2,3,22:789)%>% 
  group_by(plotID,siteID)%>% 
  summarise(across(everything(), list(sum)))%>% 
  mutate_if(is.numeric, ~1 * (. > 0))%>% 
  remove_rownames() %>%
  column_to_rownames(var = 'plotID')%>% 
  ungroup()



####Plants####

#get pres/abs for each species per site

df_plant <- data_plant  %>% 
  mutate(present = 1) %>%
  pivot_wider(names_from = taxon_name, values_from = present) %>% 
  mutate_at(vars(unique(data_plant$taxon_name)), ~ifelse(is.na(.), 0, 1))%>%
  select(2,3,35:610)%>% 
  group_by(plotID,siteID)%>% 
  summarise(across(everything(), list(sum)))%>% 
  mutate_if(is.numeric, ~1 * (. > 0))%>% 
  remove_rownames() %>%
  column_to_rownames(var = 'plotID')%>% 
  ungroup()