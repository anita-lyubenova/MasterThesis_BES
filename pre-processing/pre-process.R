# This scripts processes the data created with the simulate.R script

#INPUT: the data created with the simulate.R script: 
# a list with 1188 elements [conditions],
#       - each element is dataframe (30000 rows[iterations x studies] and 6 columns[hypotheses])
#       - each value in the data frame is a Bayes factor for a hypothesis against the uncostrained hypothesis

#OUTPUT: a 5-dimensional array with dimensions [studies,BF hypothesis, population, iterations,sample size]
################################################################################################## 2
#                                    ---- Version 3 HPC final ----
################################################################################################## 2


# 3 predictors -----------------------------
library(dplyr)
library(abind)

source("pre-processing/pre-processing functions.R")

res3par<-readRDS(file= "simulation/output/res3par_shiny.rds")
length(res3par)
dimnames(res3par[[1]])[[2]]
res3par[[2]]

##Step 1) Name each element of the list ------------------------

pop_names<-sapply(res3par, function(x){return(attributes(x)$pop_name)}) 
names(res3par)<-pop_names

#Output: the same list with 1188 elements, but the elements are named

## Step 2) nest all sample sizes of the same population in an inner list -----------------------
#get the names of the populations
pop<-sub(pattern="_n.*", "", pop_names)  %>% unique()
n<-sub(pattern=".*_n", "", pop_names)  %>% unique()

res3par2<-lapply(pop, function(s){
  x<-res3par[sub(pattern="_n.*", "", names(res3par))==s]
  names(x)<-n
  return(x)
})

names(res3par2)<-pop

#Output:
# a nested list with:
#     108 elements at level 1 [r2 x pcor x ratio beta x p];
#     11  elements at level 2 [sample sizes n], where each element is a dataframe [30000, 6] 

## Step 3) apply pre-processing procedure "Version 2" to res3par2------------

#save simulation conditions to a list to later use as attributes
n = n
studies<-30
iterations=1000
hypothesis<-dimnames(res3par[[1]])[[2]]
#all populations
pop_names<-sub(pattern="_n.*", "", pop_names) %>% unique()
r2<- gsub(".*r(.+)_pcor.*", "\\1", pop_names) %>% unique()
pcor<-gsub(".*_pcor(.+)_b.*", "\\1", pop_names) %>% unique()
p<-gsub(pattern = ".*p", "", pop_names) %>% unique()
ratio_beta<- gsub(".*_b(.+)_p.*", "\\1", pop_names) %>% unique()

att<-list(n=n,
          studies=studies,
          iterations=iterations,
          hypothesis=hypothesis,
          pop_names=pop_names,
          r2=r2,
          pcor=pcor,
          p=p,
          ratio_beta=ratio_beta
)
#reshape the inner lists (level 2) to 4d arrays where the dimensions are [studies, BF_hyp, iter, n],  i.e.
#   -studies*iterations are split to different dimensions
#   -levels of n are put into a dimension

array4d<-lapply(res3par2, function(x){
  reshapeBFs(BF_list=x, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
             n=n,
             studies=studies,
             iterations=iterations)
})
# => array4d is a list of length 108 [number of conditions], where each element is a 4d array



#bind the 4d arrays within the list (i.e., populations) along the 5th dim
BF_bind<-do.call(what = "abind", args=list(array4d, along=5))%>%
  #and reorder the dims such that the structure is [t,BF_hyp, population, iter,n]
  aperm(perm=c(1,2,5,3,4))


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


###add mixed population H1+Hc -----------------------

mixed_pop<-array(NA, dim = c(studies,
                            length(dimnames(BF_bind3)[[2]]), #number of hypotheses
                            3*3*6,
                            iterations,
                            length(n)
))
dimnames(mixed_pop)[[3]]<-1:(3*3*6)
dim(mixed_pop)
# i<-1
# s<-1
# t<-1
# y<-0.09
# z<-0
# j<-0
# x<-1
x=0
for(y in r2){
  for(z in pcor){

       x=x+1
       print(paste0(" | y: ",y, " | z: ",z, "||", x , "starts: ",Sys.time()))
        for(i in 1:1000){ # iterations
          for(t in 1:30){ # studies
            for(s in 1:length(n)){ #sample size

             # print(paste0(" | s: ", s))
             # print(paste0("i: ",i," | t: ",t," | s: ", s," | y: ",y, " | z: ",z, " | j: ",j ))
              pop_form<- grep(paste0("r",y), pop_names, value = TRUE) 
              pop_form<-pop_form[gsub(".*pcor(.+)_b.*", "\\1", pop_form)==z]
              pop_form<-pop_form[sub(".*_p", "", pop_form)==0]  
              
              if((t %% 2)==0){
                #population<-"Hc" b123
                population<-pop_form[2]
              }else{
                #population<-"Hi" b321
                population<-pop_form[1]
              }
              
              mixed_pop[t,,x,i,s]<-BF_bind3[t,,population,i,s]

              dimnames(mixed_pop)[[3]][x]<-sub("_b.*_p", "_bmixed_p", pop_form[1])
          
                
           
          }
        }
      

    }
  }
}
mixed_pop[,,1:9,,] %>% dim
#combine the array slice with the main arrays
BF_bind4<-abind(BF_bind3, mixed_pop[,,1:9,,], along = 3)


attributes(BF_bind4)<-c(attributes(BF_bind4),
                        att)

saveRDS(BF_bind4, file="pre-processing/output/BF_data_3par_hpc_final_mixed.rds")



# 2 predictors -----------------------------
library(dplyr)
library(abind)

source("pre-processing/pre-processing functions.R")

res2par<-readRDS(file= "simulation/output/res2par_shiny.rds")
length(res2par)
dimnames(res2par[[1]])[[2]]
res2par[[2]]
attributes(res2par[[1]])
##Step 1) XXX ------------------------

pop_names<-sapply(res2par, function(x){return(attributes(x)$pop_name)}) 

rats<-gsub(".*_b(.+)_p.*", "\\1", pop_names) %>% 
 gsub('3', '', .)

pop_names<-paste0(str_extract(pop_names, ".+?(?<=_b)"), rats, str_extract_all(pop_names, "_p\\d.*"))
names(res2par)<-pop_names

## Step 2) nest all sample sizes of a population in an inner list -----------------------
#get the names of the populations
pop<-sub(pattern="_n.*", "", pop_names)  %>% unique()
n<-sub(pattern=".*_n", "", pop_names)  %>% unique()

res2par2<-lapply(pop, function(s){
  x<-res2par[sub(pattern="_n.*", "", names(res2par))==s]
  names(x)<-n
  return(x)
})

names(res2par2)<-pop


## Step 3) apply pre-processing procedure "Version 2" to res3par2------------

#Simulation conditions
# I use only the attributes from one of the populations, because they are the same across populations
n = n
studies<-30
iterations=1000
hypothesis<-dimnames(res2par[[1]])[[2]]
#all populations
pop_names<-sub(pattern="_n.*", "", pop_names) %>% unique()
r2<- gsub(".*r(.+)_pcor.*", "\\1", pop_names) %>% unique()
pcor<-gsub(".*_pcor(.+)_b.*", "\\1", pop_names) %>% unique()
p<-gsub(pattern = ".*p", "", pop_names) %>% unique()
ratio_beta<- gsub(".*_b(.+)_p.*", "\\1", pop_names) %>% unique()

att<-list(n=n,
          studies=studies,
          iterations=iterations,
          hypothesis=hypothesis,
          pop_names=pop_names,
          r2=r2,
          pcor=pcor,
          p=p,
          ratio_beta=ratio_beta
)
#reshape the inner lists to 4d arrays where [studies, BF_hyp, iter, n],  i.e.
#   -studies*iterations are split to different dimensions
#   -levels of n are put into a dimension
# => array4d is a list condtaining 4d arrays of lenght the number of populations
array4d<-lapply(res2par2, function(x){
  reshapeBFs(BF_list=x, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
             n=n,
             studies=studies,
             iterations=iterations)
})



#bind the 4d arrays (i.e., populations) along the 5th dim
BF_bind<-do.call(what = "abind", args=list(array4d, along=5))%>%
  #and reorder the dims such that the structure is [t,BF_hyp, pop, iter,n]
  aperm(perm=c(1,2,5,3,4))


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


###add mixed population H1+Hc -----------------------

mixed_pop<-array(NA, dim = c(studies,
                             length(dimnames(BF_bind3)[[2]]), #number of hypotheses
                             3*3*6,
                             iterations,
                             length(n)
))
dimnames(mixed_pop)[[3]]<-1:(3*3*6)
dim(mixed_pop)
# i<-1
# s<-1
# t<-1
# y<-0.09
# z<-0
# j<-0
# x<-1


mixed_pop[,,1:9,,] %>% dim
#combine the array slice with the main arrays
BF_bind4<-abind(BF_bind3, mixed_pop[,,1:9,,], along = 3)


attributes(BF_bind4)<-c(attributes(BF_bind4),
                        att)

saveRDS(BF_bind4, file="pre-processing/output/BF_data_2par_hpc_final_mixed.rds")
