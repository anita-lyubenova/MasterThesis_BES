# This scripts processes the data created with the simulate.R script

#INPUT: the data created with the simulate.R script: 
# a list with 135 elements [conditions],
#       - each element is dataframe (60000 rows[iterations x studies] and 2 columns[hypotheses])
#       - each value in the data frame is a Bayes factor for a hypothesis against the uncostrained hypothesis

#OUTPUT: a 5-dimensional array with dimensions [studies,BF hypothesis, population, iterations,sample size]

# 1 predictor -----------------------------
library(dplyr)
library(abind)

source("pre-processing/pre-processing functions.R")

res1par<-readRDS(file= "simulation/output/BFresults.rds")
length(res1par)
dimnames(res1par[[1]])[[2]]

##Step 1) Name each element of the list ------------------------

pop_names<-sapply(res1par, function(x){return(attributes(x)$pop_name)}) 
names(res1par)<-pop_names

#the computation of Hc might be the reason for the excessive number of BFcu=0 => assymetric support for H1
# res1par<-lapply(res1par, function(x){
#   x[,"Hu"]<-1
#   Hc <- 2-x[,"H1"]
#   x<-cbind(x,Hc)
#   x[,c("H1", "Hc", "Hu")]
# })

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
# => array4d is a list of length [number of conditions], where each element is a 4d array



#bind the 4d arrays within the list (i.e., populations) along the 5th dim
BF_bind<-do.call(what = "abind", args=list(array4d, along=5))%>%
  #and reorder the dims such that the structure is [t,BF_hyp, population, iter,n]
  aperm(perm=c(1,2,5,3,4))
dim(BF_bind)

#name the populations contained in teh 3th dim
dimnames(BF_bind)[[3]]<-pop_names

#add BFuu = 1
#First, create an array slice for BFuu
Hu_array<-array(1, dim = c(studies,
                           1,
                           length(pop_names),
                           iterations,
                           length(n)
))
#combine the array slicefor BFuu with the main array
BF_bind3<-abind(BF_bind, Hu_array, along = 2)
#give the BFuu slice a name "Hu"
dimnames(BF_bind3)[[2]][length(dimnames(BF_bind3)[[2]])]<-"Hu"

attributes(BF_bind3)<-c(attributes(BF_bind3),
                        att)
saveRDS(BF_bind3, file="pre-processing/output/processed_data.rds")


# Opposite hypothesis ------------------------------------------------------

library(dplyr)
library(abind)

source("pre-processing/pre-processing functions.R")

res1par<-readRDS(file= "simulation/output/BFresults_opposite hypothesis.rds")
length(res1par)
dimnames(res1par[[1]])[[2]]

##Step 1) Name each element of the list ------------------------

pop_names<-sapply(res1par, function(x){return(attributes(x)$pop_name)}) 
names(res1par)<-pop_names

#the computation of Hc might be the reason for the excessive number of BFcu=0 => assymetric support for H1
# res1par<-lapply(res1par, function(x){
#   x[,"Hu"]<-1
#   Hc <- 2-x[,"H1"]
#   x<-cbind(x,Hc)
#   x[,c("H1", "Hc", "Hu")]
# })

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

#Output:
# a nested list with:
#     16 elements at level 1 (conditions) [delta x tau];
#     8  elements at level 2 [sample sizes n], where each element is a dataframe [60000, 2] 

## Step 3) apply pre-processing procedure "Version 2" to res1par2------------

#save simulation conditions to a list to later use as attributes

studies<-40
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
# => array4d is a list of length [number of conditions], where each element is a 4d array



#bind the 4d arrays within the list (i.e., populations) along the 5th dim
BF_bind<-do.call(what = "abind", args=list(array4d, along=5))%>%
  #and reorder the dims such that the structure is [t,BF_hyp, population, iter,n]
  aperm(perm=c(1,2,5,3,4))
dim(BF_bind)

#name the populations contained in teh 3th dim
dimnames(BF_bind)[[3]]<-pop_names

#add BFuu = 1
#First, create an array slice for BFuu
Hu_array<-array(1, dim = c(studies,
                           1,
                           length(pop_names),
                           iterations,
                           length(n)
))
#combine the array slicefor BFuu with the main array
BF_bind3<-abind(BF_bind, Hu_array, along = 2)
#give the BFuu slice a name "Hu"
dimnames(BF_bind3)[[2]][length(dimnames(BF_bind3)[[2]])]<-"Hu"

attributes(BF_bind3)<-c(attributes(BF_bind3),
                        att)
saveRDS(BF_bind3, file="pre-processing/output/processed_data_opposite hypothesis.rds")



