#SEM using piecewiseSEM

#load packages
library(tidyverse)
library(dplyr)
library(sars)
library(piecewiseSEM)
library(lme4)
library(lmerTest)
library(DHARMa)
library(car)
install.packages("semPaths")

####beetle####
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
             left_join(beetle_vars,by="siteID")%>%
             subset(.,siteID!="GUAN"&siteID!="PUUM"&siteID!="STER"&siteID!="LAJA")%>%#remove puerto rico and Hawaii sites and STER
             filter(n_observation>=20)#to filter out sites with less than 40 obs

#species richness model
colnames(comb_beetle)
hist(comb_beetle$n_observation, breaks = 15)

#model without lat because lat and temp are colinear (could run atemp model) 
beetle_rich<-lm(n_sp~n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_beetle)

vif(beetle_rich)
summary(beetle_rich)
plot(beetle_rich)


#c model
beetle_c<-lm(c~n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_beetle)

vif(beetle_c)
summary(beetle_c)
plot(beetle_c)

#z model
beetle_z<-lm(z~c+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_beetle)

vif(beetle_z)
summary(beetle_z)
plot(beetle_z) #point 35 is has high leverage


#nlcd div  model
beetle_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_beetle)

vif(beetle_nlcd)
summary(beetle_nlcd)
plot(beetle_nlcd) #point 35 is has high leverage

#elev_cv  model
beetle_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_beetle)

vif(beetle_elev)
summary(beetle_elev)
plot(beetle_elev) #point 35 is has high leverage

beetle_sem_mod<-psem(beetle_c,beetle_z,beetle_elev,beetle_nlcd,beetle_rich)


summary(beetle_sem_mod)
plot(beetle_sem_mod)



####Small_mammals####
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
  select(2,12,13,21:25)%>%
  rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
         max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get mammal SAR parameters
mammal_params<-read.csv("Data/mammal_params.csv")

# get mammal sampling covariates
mammal_vars<-read.csv("Data/mammal_vars.csv",row=1)

#combine into one dataframe
comb_mammal<-mammal_params%>%
              rename(siteID=X)%>%
              left_join(site_data,by="siteID")%>%
              left_join(mammal_vars,by="siteID")%>%
              subset(.,siteID!="GUAN"&siteID!="PUUM"&siteID!="LAJA")%>%#remove puerto rico and Hawaii sites
              filter(n_observation>=20)#to filter out sites with less than 40 obs

#species richness model
colnames(comb_mammal)
hist(comb_mammal$n_sp, breaks = 15)

#model without lat because lat and temp are colinear (could run atemp model) 
mammal_rich<-lm(n_sp~n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_mammal)

vif(mammal_rich)
summary(mammal_rich)
plot(mammal_rich)
plot(comb_mammal$n_observation,comb_mammal$n_sp)


#c model
mammal_c<-lm(c~n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_mammal)

vif(mammal_c)
summary(mammal_c)
plot(mammal_c)

#z model
mammal_z<-lm(z~c+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_mammal)

vif(mammal_z)
summary(mammal_z)
plot(mammal_z) #point 35 is has high leverage


#nlcd div  model
mammal_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_mammal)

vif(mammal_nlcd)
summary(mammal_nlcd)
plot(mammal_nlcd) #point 35 is has high leverage

#elev_cv  model
mammal_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_mammal)

vif(mammal_elev)
summary(mammal_elev)
plot(mammal_elev) #point 35 is has high leverage


#piecwise sem model
mammal_sem_mod<-psem(mammal_c,mammal_z,mammal_elev,mammal_nlcd,mammal_rich)


summary(mammal_sem_mod)
plot(mammal_sem_mod)

####birds####



####plants####

#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get plant SAR parameters
plant_params<-read.csv("Data/plant_params.csv")

# get plant sampling covariates
plant_vars<-read.csv("Data/plant_vars.csv",row=1)

#combine into one dataframe
comb_plant<-plant_params%>%
            rename(siteID=X)%>%
            left_join(site_data,by="siteID")%>%
            left_join(plant_vars,by="siteID")%>%
            subset(.,siteID!="GUAN"&siteID!="PUUM"&siteID!="LAJA")%>%#remove puerto rico and Hawaii sites
            filter(n_observation>=20)#to filter out sites with less than 40 obs

#species richness model
colnames(comb_plant)
hist(comb_plant$n_sp, breaks = 15)

#model without lat because lat and temp are colinear (could run atemp model) 
plant_rich<-lm(n_sp~n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_plant)

vif(plant_rich)
summary(plant_rich)
plot(plant_rich)
plot(comb_plant$n_observation,comb_plant$n_sp)


#c model
plant_c<-lm(c~n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_plant)

vif(plant_c)
summary(plant_c)
plot(plant_c)

#z model
plant_z<-lm(z~c+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_plant)

vif(plant_z)
summary(plant_z)
plot(plant_z) #point 35 is has high leverage


#nlcd div  model
plant_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_plant)

vif(plant_nlcd)
summary(plant_nlcd)
plot(plant_nlcd) #point 35 is has high leverage

#elev_cv  model
plant_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_plant)

vif(plant_elev)
summary(plant_elev)
plot(plant_elev) #point 35 is has high leverage


#piecwise sem model
plant_sem_mod<-psem(plant_c,plant_z,plant_elev,plant_nlcd,plant_rich, z %~~% n_sp)


summary(plant_sem_mod)
plot(plant_sem_mod)
