
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



####Beetles####
# assuming that you just want the species richness of each plotID


#obtain a site by species data frame th
beetle_df <- data_beetle %>% 
            select(siteID, plotID, taxon_name) %>% 
            mutate(present = 1) %>% 
            group_by(plotID, taxon_name, siteID) %>% 
            summarise(present = sum(present)/sum(present)) %>% 
            pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
            remove_rownames() %>%
            column_to_rownames(var = 'plotID')    


#run species iNEXT for one site        
#zz<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID=="ABBY")   
#plot(zz)


#dat<-beetle_df%>%
#     filter(siteID=="ABBY")%>%
#     #select( -siteID)%>%
#     t()%>%
     

lm<-split(beetle_df,beetle_df$siteID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)


out.raw <- iNEXT(lm, q = 0, datatype="incidence_raw")



set.seed(23)
palette <- sample(c("color1", "color2", ...), 47, replace = TRUE)

scale_fill_manual(values=palette)

####plot####

list.of.packages <- c("rstudioapi","fBasics","grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

pal <- topo.colors(n = 47)
pal

par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)

ggiNEXT(out.raw,color.var="Assemblage")
data(spider)
z <- iNEXT(spider, q=c(0), datatype="abundance")
p1 <- ggiNEXT(z)


qw<-lm$ABBY
str(ciliates$EtoshaPan)

str(ddat)
#make a list of site names
site_list <- unique(beetle_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in site_list) {                                              
  acc<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}


#this is the number of plots at each location
plot_num<-beetle_df%>%
  count(siteID)

#just make a vector of plot numbers for loop below
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
                            row.names =site_list)
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
