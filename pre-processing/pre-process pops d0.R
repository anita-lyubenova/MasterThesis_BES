library(dplyr)
library(abind)

source("pre-processing/pre-processing functions.R")

res1par<-readRDS(file= "simulation/output/BFresults_d0_tau0.45.rds")


##Step 1) Name each element of the list ------------------------

pop_names<-sapply(res1par, function(x){return(attributes(x)$pop_name)}) 
names(res1par)<-pop_names

res1par<-lapply(res1par, function(x){
  x[,"Hu"]<-1
  Hc <- 2-x[,"H1"]
  x<-cbind(x,Hc)
  x[,c("H1", "Hc", "Hu")]
})

#Output: the same list with 135 elements, but the elements are named; and BFcu was added

## Step 2) nest all sample sizes of the same population in an inner list -----------------------
#get the names of the populations and sample sizes
pop<-sub(pattern="_n.*", "", pop_names)  %>% unique()
n<-sub(pattern=".*_n", "", pop_names)  %>% unique()

res1par2<-lapply(pop, function(s){
  x<-res1par[sub(pattern="_n.*", "", names(res1par))==s]
  names(x)<-n
  
  return(x)
})

names(res1par2)<-pop

############################add BFcu
res1par2[[2]]

####################
#Output:
# a nested list with:
#     16 elements at level 1 (conditions) [delta x tau];
#     8  elements at level 2 [sample sizes n], where each element is a dataframe [60000, 2] 

## Step 3) apply pre-processing procedure "Version 2" to res1par2------------

#save simulation conditions to a list to later use as attributes

studies<-30
iterations=1000
hypothesis<-dimnames(res1par[[1]])[[2]]
#all populations
pop_names<-sub(pattern="_n.*", "", pop_names) %>% unique()
delta<- gsub(".*delta(.+)_tau.*", "\\1", pop_names) %>% unique()
tau<-gsub(pattern = ".*_tau", "", pop_names) %>% unique()

att<-list(n=n,
          studies=studies,
          iterations=iterations,
          hypothesis=hypothesis,
          pop_names=pop_names,
          delta=delta,
          tau=tau
)
#reshape the inner lists (level 2) to 4d arrays where the dimensions are [studies, BF_hyp, iter, n],  i.e.
#   -studies*iterations are split to different dimensions
#   -levels of n are put into a dimension

array4d<-lapply(res1par2, function(x){
  reshapeBFs(BF_list=x, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
             n=n,
             studies=studies,
             iterations=iterations)
})
# => array4d is a list of length 15 [number of conditions], where each element is a 4d array



#bind the 4d arrays within the list (i.e., populations) along the 5th dim
BF_bind<-do.call(what = "abind", args=list(array4d, along=5))%>%
  #and reorder the dims such that the structure is [t,BF_hyp, population, iter,n]
  aperm(perm=c(1,2,5,3,4))
dim(BF_bind)

#name the populations contained in teh 3th dim
dimnames(BF_bind)[[3]]<-pop_names



attributes(BF_bind)<-c(attributes(BF_bind),
                       att)

saveRDS(BF_bind, file="pre-processing/output/processed_data_BF_d0_t0.45.rds")
