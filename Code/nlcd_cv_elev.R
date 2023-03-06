
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

#extract plot level variables
beetle_vars <- data_beetle%>%
               select(siteID, plotID, elevation,nlcdClass)%>%
               unique(.)%>%
               group_by(siteID)%>%
               summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))
           

write.csv(beetle_vars,"./data/beetle_vars.csv")


####Birds####

#extract plot level variables
bird_vars <- data_bird%>%
             select(siteID, plotID, elevation,nlcdClass)%>%
             unique(.)%>%
             group_by(siteID)%>%
             summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))


write.csv(bird_vars,"./data/bird_vars.csv")

####plants####

#extract plot level variables
plant_vars <- data_plant%>%
              select(siteID, plotID, elevation,nlcdClass)%>%
              unique(.)%>%
              group_by(siteID)%>%
              summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))


write.csv(plant_vars,"./data/plant_vars.csv")


####small mammals####

#extract plot level variables
mammal_vars <- data_small_mammal%>%
               select(siteID, plotID, elevation,nlcdClass)%>%
               unique(.)%>%
               group_by(siteID)%>%
               summarise(elv_cv= sd(elevation)/mean(elevation), nlcd= n_distinct(nlcdClass))


write.csv(mammal_vars,"./data/mammal_vars.csv")

  