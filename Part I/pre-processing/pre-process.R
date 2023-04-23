# This scripts combines the simulation files created with the simulate.R script
# Version 1: every population was saved in  a different rds file
# Version 2: populations were saved in one list
################################################################################################## 2
#                                    ---- Version 1 ----
################################################################################################## 2

library(dplyr)
library(abind)

source("Part I/pre-processing/pre-processing functions.R")

#Load files 
filenames<-list.files("Part I/data simulation/output", pattern = ".rds")
path<-"Part I/data simulation/output"

varnames<-paste0("BF_pop_", 1:length(filenames))

#load each simulation file (i.e., population), to an object with name specified in the varnames vector
# the resulting BF_pop_1,BF_pop_2, etc. are lists, where elements correspond to different n,
# and contain data.frames where [iterations*studies, BF_hyp]
for(i in 1:length(filenames)){
  assign(varnames[i],readRDS(paste0(path,"/", filenames[i])))
}

#Simulation conditions
# I use only the attributes from one of the populations, because they are the same across populations
n = attributes(BF_pop_1)$n
studies<-attributes(BF_pop_1)$studies
iterations=attributes(BF_pop_1)$iterations
hypothesis<-attributes(BF_pop_1)$hypothesis

#a vector with population names obtained from the name of the files
pop_names<-sub("\\.rds.*", "", filenames)  

#reshape the lists to 4d arrays where [studies, BF_hyp, iter, n],  i.e.
#   -studies*iterations are split to different dimensions
#   -levels of n are put into a dimension
for(i in 1:length(pop_names)){
  assign(pop_names[i],  reshapeBFs(BF_list=eval(parse(text=varnames[i])), # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
                                   n=n,
                                   studies=studies,
                                   iterations=iterations)
         )
}

#save attributes of the resulting arrays 
#again, they are the same across populations
att<-attributes(eval(parse(text=pop_names[1])))
#remove the attributes about dimensionality, and keep only the attributes relevant to the simulation conditions
att<-att[names(att)[-grep("dim", names(att))]]

#bind the 4d arrays (i.e., populations) along the 5th dimension of the array
BF_bind <-do.call(abind, args=list(mget(pop_names), along=5)) %>% 
  #and reorder the dims such that the structure is [t,BF_hyp, pop, iter,n]
  aperm(perm=c(1,2,5,3,4)) 


#add BFuu = 1
#First, create an array slice for BFuu
Hu_array<-array(1, dim = c(studies,
                           1,
                           length(filenames),
                           iterations,
                           length(n)
                           ))
#combine the array slicefor BFuu with the main array
BF_bind2<-abind(BF_bind, Hu_array, along = 2)
#give the BFuu slice a name "Hu"
dimnames(BF_bind2)[[2]]<-c("H1", "Hc", "Hu")


attributes(BF_bind2)<-c(attributes(BF_bind2),
                       att)

#save the complete 5d array
saveRDS(BF_bind2, "Part I/pre-processing/output/BF_data.rds")



################################################################################################## 2
#                                    ---- Version 2 ----
################################################################################################## 2

library(dplyr)
library(abind)

source("Part I/pre-processing/pre-processing functions.R")

res3par<-readRDS(file= "Part I/data simulation/output_lognorm_ShinyApp/res3par.rds")


#n_par 3 ----------------------------
#Simulation conditions
# I use only the attributes from one of the populations, because they are the same across populations
n = attributes(res3par[[1]])$n
studies<-attributes(res3par[[1]])$studies
iterations=attributes(res3par[[1]])$iterations
hypothesis<-attributes(res3par[[1]])$hypothesis
#all populations
pop_names<-sapply(res3par, function(x){
  attributes(x)$pop_name
} )
r2<-sapply(res3par, function(x){
  attributes(x)$r2
} ) %>% unique()
pcor<-sapply(res3par, function(x){
  attributes(x)$pcor
} ) %>% unique()
p<-sapply(res3par, function(x){
  attributes(x)$p
} ) %>% unique()
ratio_beta<-lapply(res3par, function(x){
  attributes(x)$ratio_beta
} ) %>% unique()

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
array4d<-lapply(res3par, function(x){
  reshapeBFs(BF_list=x, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
             n=n,
             studies=studies,
             iterations=iterations)
})

# #save attributes of the resulting arrays that are to be used for the final data
# #again, they are the same across populations
# att<-attributes(array4d[[1]])
# #remove the attributes about dimensionality, and keep only the attributes relevant to the simulation conditions
# att<-att[names(att)[-grep("dim", names(att))]]
# #remove the "names" attribute
# att<-att[names(att)[-grep("names", names(att))]]


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

attributes(BF_bind3)<-c(attributes(BF_bind3),
                        att)
saveRDS(BF_bind3, file="Part I/pre-processing/output_ShinyApp/BF_data_3par.rds")
# n_par 2 ---------------------------------------------
res2par<-readRDS(file= "Part I/data simulation/output_lognorm_ShinyApp/res2par.rds")



#Simulation conditions
# I use only the attributes from one of the populations, because they are the same across populations
n = attributes(res2par[[1]])$n
studies<-attributes(res2par[[1]])$studies
iterations=attributes(res2par[[1]])$iterations
hypothesis<-attributes(res2par[[1]])$hypothesis
#all populations
pop_names<-sapply(res2par, function(x){
  attributes(x)$pop_name
} )
r2<-sapply(res2par, function(x){
  attributes(x)$r2
} ) %>% unique()
pcor<-sapply(res2par, function(x){
  attributes(x)$pcor
} ) %>% unique()
p<-sapply(res2par, function(x){
  attributes(x)$p
} ) %>% unique()
ratio_beta<-lapply(res2par, function(x){
  attributes(x)$ratio_beta
} ) %>% unique()

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
array4d<-lapply(res2par, function(x){
  reshapeBFs(BF_list=x, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
             n=n,
             studies=studies,
             iterations=iterations)
})

# #save attributes of the resulting arrays 
# #again, they are the same across populations
# att<-attributes(array4d[[1]])
# #remove the attributes about dimensionality, and keep only the attributes relevant to the simulation conditions
# att<-att[names(att)[-grep("dim", names(att))]]
# #remove the "names" attribute
# att<-att[names(att)[-grep("names", names(att))]]


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
BF_bind2<-abind(BF_bind, Hu_array, along = 2)
#give the BFuu slice a name "Hu"
dimnames(BF_bind2)[[2]][length(dimnames(BF_bind2)[[2]])]<-"Hu"

attributes(BF_bind2)<-c(attributes(BF_bind2),
                        att)

saveRDS(BF_bind2, file="Part I/pre-processing/output_ShinyApp/BF_data_2par.rds")


#n_par 1 -----------------------------------------

res1par<-readRDS(file= "Part I/data simulation/output_lognorm_ShinyApp/res1par.rds")



#Simulation conditions
# I use only the attributes from one of the populations, because they are the same across populations
n = attributes(res1par[[1]])$n
studies<-attributes(res1par[[1]])$studies
iterations=attributes(res1par[[1]])$iterations
hypothesis<-attributes(res1par[[1]])$hypothesis
#all populations
pop_names<-sapply(res1par, function(x){
  attributes(x)$pop_name
} )
r2<-sapply(res1par, function(x){
  attributes(x)$r2
} ) %>% unique()
pcor<-sapply(res1par, function(x){
  attributes(x)$pcor
} ) %>% unique()
p<-sapply(res1par, function(x){
  attributes(x)$p
} ) %>% unique()
ratio_beta<-lapply(res1par, function(x){
  attributes(x)$ratio_beta
} ) %>% unique()

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
array4d<-lapply(res1par, function(x){
  reshapeBFs(BF_list=x, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
             n=n,
             studies=studies,
             iterations=iterations)
})

# #save attributes of the resulting arrays 
# #again, they are the same across populations
# att<-attributes(array4d[[1]])
# #remove the attributes about dimensionality, and keep only the attributes relevant to the simulation conditions
# att<-att[names(att)[-grep("dim", names(att))]]
# #remove the "names" attribute
# att<-att[names(att)[-grep("names", names(att))]]
# 

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
BF_bind1<-abind(BF_bind, Hu_array, along = 2)
#give the BFuu slice a name "Hu"
dimnames(BF_bind1)[[2]][length(dimnames(BF_bind1)[[2]])]<-"Hu"

attributes(BF_bind1)<-c(attributes(BF_bind1),
                        att)

saveRDS(BF_bind1, file="Part I/pre-processing/output_ShinyApp/BF_data_1par.rds")
