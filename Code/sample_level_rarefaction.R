####Beetles####

#obtain a site by species data frame th
beetle_df <- data_beetle %>% 
             select(siteID, plotID, taxon_name,boutID) %>% 
             mutate(present = 1) %>% 
             mutate(sample = paste(plotID, boutID, sep = "_")) %>% 
             group_by(sample, taxon_name, siteID)%>% 
             summarise(present = sum(present)/sum(present)) %>% 
             pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0) %>%
             ungroup()%>% 
             select(-c(sample))

#make a list of matrices (site-by-species) to run in iNEXT
lm<-split(beetle_df,beetle_df$siteID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)
ad<-lm$ABBY

#Run iNEXT for species richness q=0)
out.raw <- iNEXT(lm, q = 0, datatype="incidence_raw")

#SUBSET INEXT OUTPUT FOR SPECIES RICHNESS ONLY
ex_rich<-out.raw$AsyEst%>%
         filter(Diversity=="Species richness")%>%
         rename(siteID=Assemblage)

#Try and select sites where observed is in extrapolated ci's

beetle_rar<-out.raw$iNextEst$size_based%>%
             rename(siteID=Assemblage)%>%
             group_by(siteID)%>%
             slice(20,40)%>%#take the observed richness line and the final exatrapolated line
             mutate(fit= qD>=qD.LCL[row_number()+1] & qD<=qD.UCL[row_number()+1])%>%#compare the onserved to the lcl and ucl of the exatrapolation
             mutate(percent= qD/qD[row_number()+1])%>%
             filter(Method=="Observed")%>%
             select(siteID,qD,fit, percent)

write.csv(beetle_rar,"./data/beetle_rar.csv")



  
  

#rarefy down to the lowest species richness with estimate D
d<-estimateD(lm, datatype="incidence_raw",q = 0, base="size", level=7)%>%
   rename(siteID=Assemblage)


#do sites with more samples have greater extrapolated richness estimates?
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get beetle SAR parameters
beetle_params<-read.csv("Data/beetle_params.csv")

# get beetle sampling covariates
beetle_vars<-read.csv("Data/beetle_vars.csv",row=1)

#get inter plot distances
beetle_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
             filter(taxon=="beetle")%>%
             rename(siteID = site)%>%
             select(siteID,aveDist)



#combine into one dataframe with other beetle data
comb_beetle<-beetle_params%>%
            rename(siteID=X)%>%
            left_join(site_data,by="siteID")%>%
            left_join(beetle_vars,by="siteID")%>%
            left_join(beetle_dist,by="siteID")%>%
            left_join(beetle_rar,by="siteID")%>%
            subset(.,siteID!="GUAN"& siteID!="PUUM"& siteID!="LAJA")%>%#remove puerto rico and Hawaii sites and STER
            filter(fit=="TRUE")         
          # filter(percent>=.90)#to filter out sites with less than 40 obs

##LOOK AT RELATIONSHIPS
mod<-lm(comb_beetle$qD~comb_beetle$n_observation)
summary(mod)
plot(comb_beetle$n_observation,comb_beetle$qD)

####Small_mammals####

#obtain a site by species data frame th
mammal_df <- data_small_mammal %>% 
              select(siteID, plotID, taxon_name,unique_sample_id) %>% 
              mutate(present = 1) %>% 
              #mutate(sample = paste(plotID, boutID, sep = "_")) %>% 
              rename(sample=unique_sample_id)%>% 
              group_by(sample, taxon_name, siteID)%>% 
              summarise(present = sum(present)/sum(present)) %>% 
              pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0) %>%
              subset(.,siteID!="GUAN"& siteID!="BARR"& siteID!="LAJA")%>%
              ungroup()%>% 
              select(-c(sample))

#make a list of matrices (site-by-species) to run in iNEXT
lm<-split(mammal_df,mammal_df$siteID)%>%
   lapply(., function(x)x[,-1 ])%>%
   lapply(.,t)


#Run iNEXT for species richness q=0)
out.raw_mam <- iNEXT(lm, q = 0, datatype="incidence_raw")

#SUBSET INEXT OUTPUT FOR SPECIES RICHNESS ONLY
ex_rich<-out.raw_mam$AsyEst%>%
         filter(Diversity=="Species richness")%>%
         rename(siteID=Assemblage)

#Try and select sites where observed is in extrapolated ci's

mammal_rar<-out.raw_mam$iNextEst$size_based%>%
            rename(siteID=Assemblage)%>%
            group_by(siteID)%>%
            slice(20,40)%>%#take the observed richness line and the final exatrapolated line
            mutate(fit= qD>=qD.LCL[row_number()+1] & qD<=qD.UCL[row_number()+1])%>%#compare the onserved to the lcl and ucl of the exatrapolation
            mutate(percent= qD/qD[row_number()+1])%>%
            filter(Method=="Observed")%>%
            select(siteID,qD,fit, percent)

write.csv(mammal_rar,"./data/mammal_rar.csv")






#rarefy down to the lowest species richness with estimate D
d<-estimateD(lm, datatype="incidence_raw",q = 0, base="size", level=7)%>%
  rename(siteID=Assemblage)


#do sites with more samples have greater extrapolated richness estimates?
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
                   max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get mammal SAR parameters
mammal_params<-read.csv("Data/mammal_params.csv")

# get mammal sampling covariates
mammal_vars<-read.csv("Data/mammal_vars.csv",row=1)

#get inter plot distances
mammal_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
            filter(taxon=="mammal")%>%
            rename(siteID = site)%>%
            select(siteID,aveDist)



#combine into one dataframe with other mammal data
comb_mammal<-mammal_params%>%
              rename(siteID=X)%>%
              left_join(site_data,by="siteID")%>%
              left_join(mammal_vars,by="siteID")%>%
              left_join(mammal_dist,by="siteID")%>%
              left_join(mammal_rar,by="siteID")%>%
              subset(.,siteID!="GUAN"& siteID!="PUUM"& siteID!="LAJA")%>%#remove puerto rico and Hawaii sites and STER
              #filter(fit=="TRUE")         
              filter(percent>=.9)#to filter out sites with less than 40 obs

##LOOK AT RELATIONSHIPS
mod<-lm(comb_mammal$qD~comb_mammal$n_observation)
summary(mod)
plot(comb_mammal$n_observation,comb_mammal$qD)


####birds####
#obtain a site by species data frame th
bird_df <- data_bird %>% 
          select(siteID, plotID, taxon_name,unique_sample_id) %>% 
          mutate(present = 1) %>% 
          #mutate(sample = paste(plotID, boutID, sep = "_")) %>% 
          rename(sample=unique_sample_id)%>% 
          group_by(sample, taxon_name, siteID)%>% 
          summarise(present = sum(present)/sum(present)) %>% 
          pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0) %>%
          #subset(.,siteID!="GUAN"& siteID!="BARR"& siteID!="LAJA")%>%
          ungroup()%>% 
          select(-c(sample))

#make a list of matrices (site-by-species) to run in iNEXT
lm<-split(bird_df,bird_df$siteID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)


#Run iNEXT for species richness q=0)
out.raw_mam <- iNEXT(lm, q = 0, datatype="incidence_raw")

#SUBSET INEXT OUTPUT FOR SPECIES RICHNESS ONLY
ex_rich<-out.raw_mam$AsyEst%>%
        filter(Diversity=="Species richness")%>%
        rename(siteID=Assemblage)

#Try and select sites where observed is in extrapolated ci's

bird_rar<-out.raw_mam$iNextEst$size_based%>%
          rename(siteID=Assemblage)%>%
          group_by(siteID)%>%
          slice(20,40)%>%#take the observed richness line and the final exatrapolated line
          mutate(fit= qD>=qD.LCL[row_number()+1] & qD<=qD.UCL[row_number()+1])%>%#compare the onserved to the lcl and ucl of the exatrapolation
          mutate(percent= qD/qD[row_number()+1])%>%
          filter(Method=="Observed")%>%
          select(siteID,qD,fit, percent)

write.csv(bird_rar,"./data/bird_rar.csv")






#rarefy down to the lowest species richness with estimate D
d<-estimateD(lm, datatype="incidence_raw",q = 0, base="size", level=7)%>%
  rename(siteID=Assemblage)


#do sites with more samples have greater extrapolated richness estimates?
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
                  max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get bird SAR parameters
bird_params<-read.csv("Data/bird_params.csv")

# get bird sampling covariates
bird_vars<-read.csv("Data/bird_vars.csv",row=1)

#get inter plot distances
bird_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
  filter(taxon=="bird")%>%
  rename(siteID = site)%>%
  select(siteID,aveDist)



#combine into one dataframe with other bird data
comb_bird<-bird_params%>%
          rename(siteID=X)%>%
          left_join(site_data,by="siteID")%>%
          left_join(bird_vars,by="siteID")%>%
          left_join(bird_dist,by="siteID")%>%
          left_join(bird_rar,by="siteID")%>%
          subset(.,siteID!="GUAN"& siteID!="PUUM"& siteID!="LAJA")%>%#remove puerto rico and Hawaii sites and STER
          #filter(fit=="TRUE")         
          filter(percent>=.9)#to filter out sites with less than 40 obs

##LOOK AT RELATIONSHIPS
mod<-lm(comb_bird$qD~comb_bird$n_observation)
summary(mod)
plot(comb_bird$n_observation,comb_bird$qD)

####plants####
#obtain a site by species data frame th
plant_df <- data_plant %>% 
          select(siteID, plotID, taxon_name,unique_sample_id) %>% 
          mutate(present = 1) %>% 
          #mutate(sample = paste(plotID, boutID, sep = "_")) %>% 
          rename(sample=unique_sample_id)%>% 
          group_by(sample, taxon_name, siteID)%>% 
          summarise(present = sum(present)/sum(present)) %>% 
          pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0) %>%
          #subset(.,siteID!="GUAN"& siteID!="BARR"& siteID!="LAJA")%>%
          ungroup()%>% 
          select(-c(sample))

#make a list of matrices (site-by-species) to run in iNEXT
lm<-split(plant_df,plant_df$siteID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)


#Run iNEXT for species richness q=0)
out.raw_plant <- iNEXT(lm, q = 0, datatype="incidence_raw")

#SUBSET INEXT OUTPUT FOR SPECIES RICHNESS ONLY
ex_rich<-out.raw_plant$AsyEst%>%
         filter(Diversity=="Species richness")%>%
         rename(siteID=Assemblage)

#Try and select sites where observed is in extrapolated ci's

plant_rar<-out.raw_plant$iNextEst$size_based%>%
          rename(siteID=Assemblage)%>%
          group_by(siteID)%>%
          slice(20,40)%>%#take the observed richness line and the final exatrapolated line
          mutate(fit= qD>=qD.LCL[row_number()+1] & qD<=qD.UCL[row_number()+1])%>%#compare the onserved to the lcl and ucl of the exatrapolation
          mutate(percent= qD/qD[row_number()+1])%>%
          filter(Method=="Observed")%>%
          select(siteID,qD,fit, percent)

write.csv(plant_rar,"./data/plant_rar.csv")






#rarefy down to the lowest species richness with estimate D
d<-estimateD(lm, datatype="incidence_raw",q = 0, base="size", level=7)%>%
  rename(siteID=Assemblage)


#do sites with more samples have greater extrapolated richness estimates?
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
                  max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get plant SAR parameters
plant_params<-read.csv("Data/plant_params.csv")

# get plant sampling covariates
plant_vars<-read.csv("Data/plant_vars.csv",row=1)

#get inter plot distances
plant_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
            filter(taxon=="plant")%>%
            rename(siteID = site)%>%
            select(siteID,aveDist)



#combine into one dataframe with other plant data
comb_plant<-plant_params%>%
            rename(siteID=X)%>%
            left_join(site_data,by="siteID")%>%
            left_join(plant_vars,by="siteID")%>%
            left_join(plant_dist,by="siteID")%>%
            left_join(plant_rar,by="siteID")%>%
            subset(.,siteID!="GUAN"& siteID!="PUUM"& siteID!="LAJA")%>%#remove puerto rico and Hawaii sites and STER
            #filter(fit=="TRUE")         
            filter(percent>=.85)#to filter out sites with less than 40 obs

##LOOK AT RELATIONSHIPS
mod<-lm(comb_plant$qD~comb_plant$n_observation)
summary(mod)
plot(comb_plant$n_observation,comb_plant$qD)

