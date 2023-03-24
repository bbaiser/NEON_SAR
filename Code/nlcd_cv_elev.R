
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
library(vegan)

####Beetles####

#calculate Shannon diversity for habitat types at each site

nlcd_div_beetle<- data_beetle%>%
                select(siteID, plotID, elevation,nlcdClass)%>%
                unique(.)%>%
                group_by(siteID,nlcdClass)%>%
                count()%>%
                pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
                column_to_rownames("siteID")%>%
                mutate(nlcd_div=(diversity(.)))%>%
                select(nlcd_div)%>%
                rownames_to_column("siteID")

#Calculate cv of plot elevations per cite and number of habitat types for each sitte
beetle_vars <- data_beetle%>%
               select(siteID, plotID, elevation,nlcdClass)%>%
               unique(.)%>%
               group_by(siteID)%>%
               summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))%>%
               left_join(nlcd_div_beetle,by="siteID")
           

write.csv(beetle_vars,"./data/beetle_vars.csv")


####Birds####

#extract plot level variables
#calculate Shannon diversity for habitat types at each site
nlcd_div_bird<-data_bird%>%
              select(siteID, plotID, elevation,nlcdClass)%>%
              unique(.)%>%
              group_by(siteID,nlcdClass)%>%
              count()%>%
              pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
              column_to_rownames("siteID")%>%
              mutate(nlcd_div=(diversity(.)))%>%
              select(nlcd_div)%>%
              rownames_to_column("siteID")


bird_vars <- data_bird%>%
             select(siteID, plotID, elevation,nlcdClass)%>%
             unique(.)%>%
             group_by(siteID)%>%
             summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))%>%
             left_join(nlcd_div_bird,by="siteID")

write.csv(bird_vars,"./data/bird_vars.csv")

####plants####

#extract plot level variables

nlcd_div_plant<-data_plant%>%
               select(siteID, plotID, elevation,nlcdClass)%>%
               unique(.)%>%
               group_by(siteID,nlcdClass)%>%
               count()%>%
               pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
               column_to_rownames("siteID")%>%
               mutate(nlcd_div=(diversity(.)))%>%
               select(nlcd_div)%>%
               rownames_to_column("siteID")



plant_vars <- data_plant%>%
              select(siteID, plotID, elevation,nlcdClass)%>%
              unique(.)%>%
              group_by(siteID)%>%
              summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))%>%
              left_join(nlcd_div_plant,by="siteID")

              left_join(nlcd_div_bird,by="siteID")
bird_vars <- data_bird%>%
  select(siteID, plotID, elevation,nlcdClass)%>%
  unique(.)%>%
  group_by(siteID)%>%
  summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))%>%
  left_join(nlcd_div_bird,by="siteID")

write.csv(plant_vars,"./data/plant_vars.csv")


####small mammals####

#extract plot level variables
mammal_vars <- data_small_mammal%>%
               select(siteID, plotID, elevation,nlcdClass)%>%
               unique(.)%>%
               group_by(siteID)%>%
               summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))


write.csv(mammal_vars,"./data/mammal_vars.csv")

  