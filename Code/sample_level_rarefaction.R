####Beetles####
# assuming that you just want the species richness of each plotID


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

#Run iNEXT for species richness q=0)
out.raw <- iNEXT(lm, q = 0, datatype="incidence_raw")


#run species acc for one site        
#zz<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID=="ABBY")   

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

write.csv(beetle_params,"./data/beetle_params.csv")