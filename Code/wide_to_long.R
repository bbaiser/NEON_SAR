
if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)


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

#the sp. accum. curves for each site
plot_num<-df2%>%
          count(siteID)
 

lapply(plot_num$n,rep.int(40, times=plot_num$n))

data <- data.frame(NA_col = rep(NA, 47))     # Creating data containing NA
data<-list()

plotnum<-plot_num$n
sizes <- list()
sizes
plotnum<-c(plot_num$n,2)
for(i in length(plotnum)){
       da<-rep.int(40, times=plotnum[i])
       print(da)
}
da
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
       