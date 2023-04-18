#Beetle SEM using piecewiseSEM

#load packages
library(tidyverse)
library(dplyr)
library(sars)
library(piecewiseSEM)
library(lme4)
library(lmerTest)
library(DHARMa)


#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get beetle SAR parameters
beetle_params<-read.csv("Data/beetle_params.csv")

# get beetle sampling covariates
beetle_vars<-read.csv("Data/beetle_vars.csv",row=1)

#combine into one dataframe
comb_beetle<-beetle_params%>%
             rename(siteID=X)%>%
             left_join(site_data,by="siteID")%>%
             left_join(beetle_vars,by="siteID")


#species richness model
colnames(comb_beetle)

beetle_rich<-glm(n_sp~lat+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, family=gaussian, data=comb_beetle)

summary(beetle_rich)
plot(beetle_rich)
