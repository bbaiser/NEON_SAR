
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

#obtain species frequencies per plot across sampling bouts
beetle_df <-  data_beetle %>% 
              select(plotID, taxon_name) %>% 
              mutate(present = 1) %>% 
              group_by(plotID, taxon_name) %>%
              summarise(present = sum(present)) %>%
              summarize(present = paste((present),collapse=", "))


  #obtain number of samples per plot
 n_samp <-  data_beetle %>% 
            unite('sample',plotID, boutID, remove=F )%>%
            select(sample, plotID) %>%
            unique()%>%
            group_by(plotID) %>% 
            summarise(n = n()) 
 

 
 rar_in<-beetle_df %>%
        left_join(n_samp, by="plotID")%>%
        unite('sample',n, present, sep= ",")%>%
        column_to_rownames(var = 'plotID')  %>% 
        mutate_if(is.character,as.numeric)

 z<-(rar_in[1,])
x<-as.vector(z)
as.numeric(as.list(x) )
 as.numeric(rar_in)
data1 <- as.data.frame(rar_in)%>%
         as.numeric(unlist(.))
data_list <- split(data1,data1$plotID)

data_list[["1"]]
#make a list of matrices (site-by-species) to run in iNEXT
lm<-split(rar_in,rar_in$plotID)%>%
  lapply(., function(x)x[,-1 ])%>%
  lapply(.,t)

#Run iNEXT for species richness q=0)
out.raw <- iNEXT(rar_in, q = 0, datatype="incidence_freq")

#SUBSET FOR SPECIES RICHNESS ONLY
ex<-out.raw$AsyEst%>%
  filter(Diversity=="Species richness")


data(ant)
ant$h50m
ant$h500m
####plot####

p1<-ggiNEXT(out.raw,facet.var="Assemblage", type=1)
p1+facet_wrap_paginate(~Assemblage, ncol = 3, nrow = 3, page = 3)


