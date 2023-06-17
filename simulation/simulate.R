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
## 3 par ------------------------------------
#parallelize the outermost loop
beta_mu=c(-0.2,0,0.3)
beta_tau<-c(0.15, 0.3, 0.45, 0.75)
n = c(15,25,35,50,75,100,150,200,300)
studies=5
iter=10
hypothesis="X>0"

#create combinations of conditions
cond<-expand.grid(beta_mu,beta_tau, n) %>% 
  rename(beta_mu=Var1, beta_tau=Var2, n=Var3) %>% 
  mutate(pop_name=paste0("beta_mu", beta_mu, "_beta_tau", beta_tau,"_b","_n",n))

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
clusterExport(cl=cl, c("obtain_BF", "gen_dat","sim_t_x_i"))

clusterExport(cl=cl, varlist=c("cond","hypothesis", "seed", "iter", "studies"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

res1par<-parLapply(cl,
                   1:nrow(cond),
                   function(i){
                     
                     listel<-sim_t_x_i(cond$beta_mu,
                                       cond$beta_tau,
                                       cond$n, 
                                       hypothesis="X>0",
                                       studies=5,
                                       iterations=10
                     )
                     
                     attributes(listel)$pop_name<-cond[i,"pop_name"]
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

nams<- sapply(res1par,function(x) return(attributes(x)$pop_name))
names(res1par)<-nams

attributes(res1par)<-list(hypothesis=hypothesis,
                          complexity=0.5,
                          beta_mu=beta_mu,
                          beta_tau=beta_tau,
                          seed=seed,
                          iterations=iter,
                          studies=studies,
                          n=n)

saveRDS(res1par,file="simulation/output/res1par.rds")


