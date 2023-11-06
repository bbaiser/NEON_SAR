
#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
library(dplyr)
install.packages("iNEXT")
library(iNEXT)
install.packages("sars")
library(sars)
install.packages("ggforce")
library(ggforce)

####Beetles####

#obtain a site by species data frame for all sites
beetle_df <- data_beetle %>% 
  unite('sample',plotID, boutID, remove=F )%>%
  select(sample ,plotID, taxon_name) %>% 
  mutate(present = 1) %>% 
  group_by(sample,plotID, taxon_name) %>% 
  summarise(present = sum(present)) %>% 
  pivot_wider(names_from = taxon_name, values_from = present)%>%  
  remove_rownames() %>%
  column_to_rownames(var = 'sample')    
 


#make a list of matrices (site-by-species) to run in iNEXT
lm<-split(beetle_df,beetle_df$plotID)%>%
  lapply(., function(x)x[,-1 ])%>%
  lapply(.,t)
lm$ABBY_002
#Run iNEXT for species richness q=0)
out.raw <- iNEXT(lm, q = 0, datatype="incidence_freq")

#SUBSET FOR SPECIES RICHNESS ONLY
ex<-out.raw$AsyEst%>%
  filter(Diversity=="Species richness")


data(ant)
ant$h50m
ant$h500m
####plot####

p1<-ggiNEXT(out.raw,facet.var="Assemblage", type=1)
p1+facet_wrap_paginate(~Assemblage, ncol = 3, nrow = 3, page = 3)


