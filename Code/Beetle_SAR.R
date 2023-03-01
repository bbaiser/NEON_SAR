
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

#run species acc for one site        
zz<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID=="ABBY")   

#make a list of site names
varlist <- unique(beetle_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in varlist) {                                              
    acc<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID==i)
    rich[[i]]<-acc$richness#extract the richness for each site number
 }

#this is the number of plots at each location
plot_num<-beetle_df%>%
          count(siteID)

#just makea vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[2,-1]))
sp_rich<-unlist(rich[2])

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
beetle_params <- data.frame(matrix(NA,
                            nrow = 47,
                            ncol = 2),
                            row.names =varlist)
colnames(beetle_params)<-c("c","z")
                      
#run forloop extracting c and z parameters
for(i in 1:47){
  
              area<-na.omit(cumsum(matrixe[i,-1]))
              sp_rich<-unlist(rich[i])
              t_dat<-cbind(area,sp_rich)
              fit <- sar_power(data=t_dat)
              beetle_params[i,1]<-fit$par[1]
              beetle_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
beetle_params

#fit multiple models#
fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

fitC[1]

fit_multi <- sar_average(data = t_dat, grid_start = "none")
summary(fit_multi)
plot(fit_multi)




####Birds####

#get pres/abs for each species per site
df_bird <- data_bird  %>% 
  mutate(present = 1) %>% 
  pivot_wider(names_from = taxon_name, values_from = present) %>% 
  mutate_at(vars(unique(data_bird$taxon_name)), ~ifelse(is.na(.), 0, 1))%>%
  select(2,3,35:610)%>% 
  group_by(plotID,siteID)%>% 
  summarise(across(everything(), list(sum)))%>% 
  mutate_if(is.numeric, ~1 * (. > 0))%>% 
  remove_rownames() %>%
  column_to_rownames(var = 'plotID')%>% 
  ungroup()

#run species acc for one site        
zz<-vegan::specaccum(df_bird[,-1], method = "exact", subset = df_bird$siteID=="ABBY")   
plot(zz)
#make a list of site names
varlist <- unique(df_bird$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in varlist) {                                              
  acc<-vegan::specaccum(df_bird[,-1], method = "exact", subset = df_bird$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}
rich$ABBY
#this is the number of plots at each location
plot_num<-df_bird%>%
         count(siteID)

#just makea vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[47,-1]))
sp_rich<-unlist(rich[47])


summary(lm(log(sp_rich)~log(area)))
#for loop the sar_power function from the "sars" package over the site richness area data
exp( 2.97012 )
#make data frame for for loop
bird_params <- data.frame(matrix(NA,
                            nrow = 47,
                            ncol = 2),
                     row.names =varlist)

colnames(bird_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:47){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  bird_params[i,1]<-fit$par[1]
  bird_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
bird_params

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

#run species acc for one site        
zz<-vegan::specaccum(df_bird[,-1], method = "exact", subset = df_bird$siteID=="ABBY")   
plot(zz)
#make a list of site names
varlist <- unique(df_bird$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in varlist) {                                              
  acc<-vegan::specaccum(df_bird[,-1], method = "exact", subset = df_bird$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}
rich$ABBY
#this is the number of plots at each location
plot_num<-df_bird%>%
  count(siteID)

#just makea vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[47,-1]))
sp_rich<-unlist(rich[47])


summary(lm(log(sp_rich)~log(area)))
#for loop the sar_power function from the "sars" package over the site richness area data
exp( 2.97012 )
#make data frame for for loop
bird_params <- data.frame(matrix(NA,
                                 nrow = 47,
                                 ncol = 2),
                          row.names =varlist)

colnames(bird_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:47){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  bird_params[i,1]<-fit$par[1]
  bird_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
bird_params


####Mammals####

#get pres/abs for each species per site
df_mammal <- data_small_mammal %>% 
  mutate(present = 1) %>% 
  pivot_wider(names_from = taxon_name, values_from = present) %>% 
  mutate_at(vars(unique(data_small_mammal$taxon_name)), ~ifelse(is.na(.), 0, 1))%>%
  select(2,3,22:170)%>% 
  group_by(plotID,siteID)%>% 
  summarise(across(everything(), list(sum)))%>% 
  mutate_if(is.numeric, ~1 * (. > 0))%>% 
  remove_rownames() %>%
  column_to_rownames(var = 'plotID')%>% 
  ungroup()

#run species acc for one site        
zz<-vegan::specaccum(df_mammal[,-1], method = "exact", subset = df_mammal$siteID=="ABBY")   
plot(zz)
#make a list of site names
varlist <- unique(df_mammal$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in varlist) {                                              
  acc<-vegan::specaccum(df_mammal[,-1], method = "exact", subset = df_mammal$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}
rich$ABBY
#this is the number of plots at each location
plot_num<-df_mammal%>%
  count(siteID)

#just makea vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[46,-1]))
sp_rich<-unlist(rich[46])
summary(lm(log(sp_rich)~log(area)))

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
mammal_params <- data.frame(matrix(NA,
                                 nrow = 46,
                                 ncol = 2),
                          row.names =varlist)

colnames(mammal_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:46){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  mammal_params[i,1]<-fit$par[1]
  mammal_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
mammal_params

####Ticks#### 
looks lime all plots have all ticks?
colnames(data_bird)
#get pres/abs for each species per site
df_tick <- data_tick %>% 
  mutate(present = 1) %>% 
  pivot_wider(names_from = taxon_name, values_from = present) %>% 
  mutate_at(vars(unique(data_tick$taxon_name)), ~ifelse(is.na(.), 0, 1))%>%
  select(2,3,22:40)%>% 
  group_by(plotID,siteID)%>% 
  summarise(across(everything(), list(sum)))%>% 
  mutate_if(is.numeric, ~1 * (. > 0))%>% 
  remove_rownames() %>%
  column_to_rownames(var = 'plotID')%>% 
  ungroup()
df_tick$plotID

#run species acc for one site        
zz<-vegan::specaccum(df_tick[,-1], method = "random", subset = df_tick$siteID=="ABBY")   
plot(zz)
#make a list of site names
varlist <- unique(df_mammal$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in varlist) {                                              
  acc<-vegan::specaccum(df_tick[,-1], method = "exact", subset = df_tick$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}
rich$ABBY
#this is the number of plots at each location
plot_num<-df_mammal%>%
  count(siteID)

#just makea vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[46,-1]))
sp_rich<-unlist(rich[46])
summary(lm(log(sp_rich)~log(area)))

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
tick_params <- data.frame(matrix(NA,
                                   nrow = 46,
                                   ncol = 2),
                            row.names =varlist)

colnames(tick_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:46){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  tick_params[i,1]<-fit$par[1]
  tick_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
tick_params



#get latitude data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
                  max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)
          



####Beetle Lat analysis####


comb_beetle<-beetle_params%>%
             rownames_to_column( "siteID")%>%
             left_join(site_data,by="siteID")

lat_mod<-lm(z~mean_temp, data=comb_beetle)            
summary(lat_mod) 

plot(comb_beetle$mean_temp,comb_beetle$z, xlab="mean temperature", ylab="beetle z")
abline(lat_mod<-lm(z~mean_temp, data=comb_beetle))  
  
pairs(comb_beetle[,c(2:10)]) 


####Bird Lat analysis####
comb_bird<-bird_params%>%
           rownames_to_column( "siteID")%>%
           left_join(site_data,by="siteID")

lat_mod<-lm(z~long, data=comb_bird)            
summary(lat_mod) 

plot(comb_bird$long,comb_bird$z,xlab="longitude", ylab="bird z")
abline(lat_mod )

pairs(comb_bird[,c(2:10)]) 


####mammal Lat analysis####


comb_mammal<-mammal_params%>%
             rownames_to_column( "siteID")%>%
             left_join(site_data,by="siteID")

lat_mod<-lm(z~mean_temp, data=comb_mammal)            
summary(lat_mod) 

plot(comb_mammal$mean_temp,comb_mammal$z,xlab="mean temperature", ylab="mammal z")
abline(lat_mod)

pairs(comb_mammal[,c(2:10)]) 
