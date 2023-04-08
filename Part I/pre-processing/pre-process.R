# This scripts combines the simulation files created with the simulate.R script

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

attributes(BF_bind)<-c(attributes(BF_bind),
                       att)


saveRDS(BF_bind, "Part I/pre-processing/output/BF_data.rds")





