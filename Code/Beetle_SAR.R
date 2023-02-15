


if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
library(dplyr)

install.packages("sars")
library(sars)


#get pres/abs for each species per site
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

#this is the number of plots at each location
plot_num<-df2%>%
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
params <- data.frame(matrix(NA,
                            nrow = 47,
                            ncol = 2),
                            row.names =varlist)
colnames(params)<-c("c","z")
                      
#run forloop extracting c and z parameters
for(i in 1:47){
  
              area<-na.omit(cumsum(matrixe[i,-1]))
              sp_rich<-unlist(rich[i])
              t_dat<-cbind(area,sp_rich)
              fit <- sar_power(data=t_dat)
              params[i,1]<-fit$par[1]
              params[i,2]<-fit$par[2]
}

#data frame with parameters
params

####fit multiple models####
fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

fitC[1]

fit_multi <- sar_average(data = t_dat, grid_start = "none")
summary(fit_multi)
plot(fit_multi)
