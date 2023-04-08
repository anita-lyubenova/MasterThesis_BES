# This scripts combines the simulation files created with the simulate.R script

library(dplyr)
library(abind)

source("Part I/pre-processing/pre-processing functions.R")

#Load files 
filenames<-list.files("Part I/data simulation/output", pattern = ".rds")
path<-"Part I/data simulation/output"

filenames<-filenames[5:6]

varnames<-paste0("BF_pop_", 1:length(filenames))


for(i in 1:length(filenames)){
  assign(varnames[i],readRDS(paste0(path,"/", filenames[i])))
}
BF_pop_1

#Simulation conditions
n = attributes(BF_pop_1)$n
studies<-attributes(BF_pop_1)$studies
iterations=attributes(BF_pop_1)$iterations

hypothesis<-attributes(BF_pop_1)$hypothesis



pop_names<-sub("\\.rds.*", "", filenames)  

for(i in 1:length(pop_names)){
  assign(pop_names[i],  reshapeBFs(BF_list=eval(parse(text=varnames[i])), # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
                                   n=n,
                                   studies=studies,
                                   iterations=iterations)
         )
}

#save attributes that are not about dimensions
att<-attributes(eval(parse(text=pop_names[1])))
att<-att[names(att)[-grep("dim", names(att))]]

#bind the populations together along the 5th dimension of the array
BF_bind <-do.call(abind, args=list(mget(pop_names), along=5)) %>% 
  #and reorder the dims such that [t,hyp, pop, iter,n]
  aperm(perm=c(1,2,5,3,4)) 

attributes(BF_bind)<-c(attributes(BF_bind),
                       att)


saveRDS(BF_bind, "Part I/pre-processing/output/BF_data.rds")





