#This scripts simulates data used in the thesis and the shiny app

#CAUTION: long computation times!
#The script was run on a high-performace computer with 46 cores and ran for approx. 4 days
#If you intend to run the script adjust the number of cores to a desired number (variable "ncores")

#OUTPUT 'res3par' : a list where each element uses a single combination of the conditions r2 x pcor x ratio_beta x p x n:  
#         - there are 1188 combinations/elements in total
#         - each element is a dataframe with 30 000 rows (studies*iter) and 6 columns (3 hypotheses and their complements);
#         -each value in the dataframe is a Bayes factor of a hypothesis against the unconstrained hypothesis

source("simulation/simulation functions.R")
library(parallel)
## 1 par ------------------------------------

#remove(BFresults)

#parallelize the outermost loop
delta=c(-0.2,-0.1,0,0.2,0.5)
tau<-c(0,0.15, 0.2, 0.3, 0.45)
n = c(25,35,50,75,100,150,200,300)
studies=30
iter=1000
hypothesis="X1>X0"

#create combinations of conditions
cond<-expand.grid(delta,tau, n) %>% 
  rename(delta=Var1, tau=Var2, n=Var3) %>% 
  mutate(pop_name=paste0("delta", delta, "_tau", tau,"_n",n))

ncores<-7
seed=1000

print( paste0("Prep cluster: ",Sys.time()))

cl<-makeCluster(ncores)
clusterSetRNGStream(cl, iseed=seed)

clusterEvalQ(cl, {
  library(MASS)
  library(magrittr)
  library(tidyverse)
  library(BFpack)
})
clusterExport(cl=cl, c("gen_BF","sim_t_x_i"))

clusterExport(cl=cl, varlist=c("cond","hypothesis", "seed", "iter", "studies"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

BFresults<-parLapply(cl,
                   1:nrow(cond),
                   function(i){
                     
                     listel<-sim_t_x_i(cond$delta[i],
                                       cond$tau[i],
                                       cond$n[i], 
                                       hypothesis=hypothesis,
                                       studies=studies,
                                       iterations=iter
                     )
                     
                     attributes(listel)$pop_name<-cond[i,"pop_name"]
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

#Time estimate
# 200 cond; 3 studiess, 10 iter;
#From: 15:19:29 to 15:19:58
# 30 sec 
# there will be 1000 more itertions =>
# Time estimate = 30sec*1000iter/3600 = 8.33 hours


nams<- sapply(BFresults,function(x) return(attributes(x)$pop_name))
names(BFresults)<-nams

attributes(BFresults)<-list(hypothesis=hypothesis,
                          complexity=0.5,
                          delta=delta,
                          tau=tau,
                          seed=seed,
                          iterations=iter,
                          studies=studies,
                          n=n)

saveRDS(BFresults,file="simulation/output/BFresults.rds")


BFresults[1:10]



# opposite hypothesis --------------------------------------

delta=c(-0.2,0,0.2)
tau<-c(0,0.15, 0.3, 0.45)
n = c(200,300)
studies=40
iter=1000
hypothesis="X1<X0"

#create combinations of conditions
cond<-expand.grid(delta,tau, n) %>% 
  rename(delta=Var1, tau=Var2, n=Var3) %>% 
  mutate(pop_name=paste0("delta", delta, "_tau", tau,"_n",n))

ncores<-7
seed=1000

print( paste0("Prep cluster: ",Sys.time()))

cl<-makeCluster(ncores)
clusterSetRNGStream(cl, iseed=seed)

clusterEvalQ(cl, {
  library(MASS)
  library(magrittr)
  library(tidyverse)
  library(BFpack)
})
clusterExport(cl=cl, c("gen_BF","sim_t_x_i"))

clusterExport(cl=cl, varlist=c("cond","hypothesis", "seed", "iter", "studies"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

BFresults<-parLapply(cl,
                     1:nrow(cond),
                     function(i){
                       
                       listel<-sim_t_x_i(cond$delta[i],
                                         cond$tau[i],
                                         cond$n[i], 
                                         hypothesis=hypothesis,
                                         studies=studies,
                                         iterations=iter
                       )
                       
                       attributes(listel)$pop_name<-cond[i,"pop_name"]
                       return(listel)
                     }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

#Time estimate
# 200 cond; 3 studiess, 10 iter;
#From: 15:19:29 to 15:19:58
# 30 sec 
# there will be 1000 more itertions =>
# Time estimate = 30sec*1000iter/3600 = 8.33 hours


nams<- sapply(BFresults,function(x) return(attributes(x)$pop_name))
names(BFresults)<-nams

attributes(BFresults)<-list(hypothesis=hypothesis,
                            complexity=0.5,
                            delta=delta,
                            tau=tau,
                            seed=seed,
                            iterations=iter,
                            studies=studies,
                            n=n)

saveRDS(BFresults,file="simulation/output/BFresults_opposite hypothesis.rds")
