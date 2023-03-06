#load packages
library(tidyverse)
library(dplyr)
library(sars)

####Latitude_prelim_analyses####
#get latitude data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

####Beetle Lat analysis####
beetle_params<-read.csv("Data/beetle_params.csv")

comb_beetle<-beetle_params%>%
             rename(siteID=X)%>%
             left_join(site_data,by="siteID")

lat_mod<-lm(z~mean_temp, data=comb_beetle)            
summary(lat_mod) 

plot(comb_beetle$mean_temp,comb_beetle$z, xlab="mean temperature", ylab="beetle z")
abline(lat_mod<-lm(z~mean_temp, data=comb_beetle))  

pairs(comb_beetle[,c(2:10)]) 


####Bird Lat analysis####

bird_params<-read.csv("Data/bird_params.csv")

comb_bird<-bird_params%>%
           rename(siteID=X)%>%
           left_join(site_data,by="siteID")

lat_mod<-lm(z~long, data=comb_bird)            
summary(lat_mod) 

plot(comb_bird$long,comb_bird$z,xlab="longitude", ylab="bird z")
abline(lat_mod )

pairs(comb_bird[,c(2:10)]) 


####mammal Lat analysis####

mammal_params<-read.csv("Data/mammal_params.csv")

comb_mammal<-mammal_params%>%
             rename(siteID=X)%>%
             left_join(site_data,by="siteID")

lat_mod<-lm(z~mean_temp, data=comb_mammal)            
summary(lat_mod) 

plot(comb_mammal$mean_temp,comb_mammal$z,xlab="mean temperature", ylab="mammal z")
abline(lat_mod)

pairs(comb_mammal[,c(2:10)]) 


####plant Lat analysis####

plant_params<-read.csv("Data/plant_params.csv")

comb_plant<-plant_params%>%
             rename(siteID=X)%>%
             left_join(site_data,by="siteID")

lat_mod<-lm(z~c+long+mean_temp+mean_precip, data=comb_plant)            
summary(lat_mod) 

plot(comb_mammal$mean_temp,comb_mammal$z,xlab="mean temperature", ylab="mammal z")
abline(lat_mod)

pairs(comb_plant[,c(2:10)]) 

