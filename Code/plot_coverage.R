#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)


#need to load a previous version of iNext because the current one has a bug
require(devtools)
install_version("iNEXT", version = "2.0.20", repos = "http://cran.us.r-project.org")
library(iNEXT)

####Beetles####

#make a sample bout by species incidence (1/0) matrix
beetle_df <- data_beetle %>% 
            unite('sample',plotID, boutID, remove=F )%>%
            select(sample, plotID, taxon_name) %>% 
            mutate(present = 1) %>% 
            group_by(sample,plotID, taxon_name) %>% 
            summarise(present = sum(present)/sum(present)) %>% 
            pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
            remove_rownames() %>%
            column_to_rownames(var = 'sample') 


#format as list of lists to run in iNEXT
lm<-split(beetle_df,beetle_df$plotID)%>%
  lapply(., function(x)x[,-1 ])%>%
  lapply(.,t)%>%
  lapply(., as.incfreq)


#Run iNEXT for species richness q=0)
out.raw_test <- iNEXT(lm, q = c(0), datatype="incidence_freq")

#look at head of three outputs  
head(out.raw_test$AsyEst)
head(out.raw_test$DataInfo)
head(out.raw_test$iNextEst)


x<-unname(lm$ABBY_006)

estimateD(x, datatype="incidence_freq", base="coverage", level=NULL)



#SUBSET FOR SPECIES RICHNESS ONLY
ex<-out.raw_test$AsyEst%>%
  filter(Diversity=="Species richness")


#look at histogram of species sample coverage
hist(out.raw_test$DataInfo$SC, breaks = 50)

#plot
ggiNEXT(out.raw_test$, type=3)

#SUBSET FOR SPECIES RICHNESS ONLY
ex<-out.raw_test$AsyEst%>%
  filter(Diversity=="Species richness")


#On the other hand, with
#a fixed size of the sampling unit (plot), incidence-based sample 
#coverage evaluates the proportion of overall occurrences
#attributable to identified species. This implies that
#incidence-based sample coverage is contingent on the size
#of the sampling unit (plot).


####plot####

p1<-ggiNEXT(out.raw,facet.var="Assemblage", type=1)
p1+facet_wrap_paginate(~Assemblage, ncol = 3, nrow = 3, page = 3)

