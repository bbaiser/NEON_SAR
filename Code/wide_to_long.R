
if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)


#get pres/abs for each speces per site
df2 <- data_beetle %>% 
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

#run species acc for one site        
zz<-vegan::specaccum(df2[,-1], method = "exact", subset = df2$siteID=="ABBY")   

#make a list of site names
varlist <- unique(df2$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in varlist) {                                              
    acc<-vegan::specaccum(df2[,-1], method = "exact", subset = df2$siteID==i)
    rich[[i]]<-acc$richness#extract the richness for each site number
 }

#the sp. accum. curves for each site
rich






       