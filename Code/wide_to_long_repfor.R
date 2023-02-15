
if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
library(dplyr)
devtools::install_github("tidyverse/tidyverse")

#get pres/abs for each speces per site
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

#this is afunction that allows you to repeat something a certian ammount of times
rep.int(40, times=11)

#here is my attempt at lapply over the different plot numbers in plot_num$n
lapply(plot_num$n,rep.int(40, times=plot_num$n))

#here is my attempt to make empty objects for a for loop
data <- data.frame(NA_col = rep(NA, 47))     # Creating dataframe containing NA
data<-list()


#just makea vector of plot numbers
plotnum<-plot_num$n

#forloop only returns last row...

matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)
for(i in 1:length(plotnum)){
  #i<-5
  obj<-as.vector(plotnum[i])
  
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  
  matrixe[i,1:length(obj)]<-obj
  
  
       
}

matrixe

data


#####extra junk####
sizes
lapply(plot_num$n,rep,x=40,times=2)

my_func<-function(plots,size){
         output<-rep.int(size, plots)
         return()
}


plotnum<-plot_num$n

for(i in 1:length(plotnum){
  pn<-my_func(40, i)
  return(pn)
}

pn
my_func(10,40)

apply(plot_num$n,1,my_func,size=40,plots=2)

input_1 <- 20
mySum <- function(input_1, input_2 = 10) {
  output <- input_1 + input_2
  return(output)
}

rep.int()
}
dim(rep.int(40, times=2))


length (rich[2])
   library(stringi)

   cbind(plot_num,rep=stri_dup("40 ",plot_num$n))

   cumsum(c(40,40,40)
       
       rep(1:4, c(2,1,2,1))   
       