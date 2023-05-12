#This scripts simulates data used in the thesis and the shiny app

#CAUTION: long computation times!
#The script was run on a high-performace computer with 46 cores and ran for approx. 4 days
#If you intend to run the script adjust the number of cores to a desired number (variable "ncores")

#OUTPUT 'res3par' : a list where each element uses a single combination of the conditions r2 x pcor x ratio_beta x p x n:  
#         - there are 1188 combinations/elements in total
#         - each element is a dataframe with 30 000 rows (studies*iter) and 6 columns (3 hypotheses and their complements);
#         -each value in the dataframe is a Bayes factor of a hypothesis against the unconstrained hypothesis

source("simulation/simulation functions.R")

## 3 par ------------------------------------
#parallelize the outermost loop
r2<-c(0.09,0.13,0.4)
pcor<-c(0,0.3,0.6)
p<-c(0,0.30,0.5,0.7,0.86,1.1)
n = c(15,25,35,50,75,100,150,200,300,500,800)
ratio_beta<-c("c(3,2,1)",
              "c(1,2,3)")
studies=30
iter=1000

#create combinations of conditions
cond<-expand.grid(r2,pcor,p, ratio_beta, n) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4,n=Var5) %>% 
  mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p, "_n",n))

ncores<-46
seed=3000

print( paste0("Prep cluster: ",Sys.time()))

cl<-makeCluster(ncores)
clusterSetRNGStream(cl, iseed=seed)

clusterEvalQ(cl, {
  library(MASS)
  library(magrittr)
  library(tidyverse)
  library(BFpack)
})
clusterExport(cl=cl, c("single_sim_ln", "q_glm", "gen_dat", "cormat", "coefs", "run_sim_ln"))

clusterExport(cl=cl, varlist=c("cond", "seed", "iter", "studies"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

res3par<-parLapply(cl,
                   1:nrow(cond),
                   function(i){
                     
                     listel<-run_sim_ln(r2=cond[i,]$r2,  # R-squared of the regression model
                                        pcor=cond[i,]$pcor,  # correlation between the predictors
                                        ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
                                        # Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                                        p=cond[i,]$p, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
                                        hypothesis=c(c.01="V1>V2>V3>0", c.13="V1>V2>V3",  c.31="V1>V2 & 0.6666667*V1>V3"),
                                        n=cond[i,]$n,  #sample size 
                                        model="linear",
                                        studies=studies,
                                        iterations=iter)
                     
                     attributes(listel)$pop_name<-cond[i,"pop_name"]
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

nams<- sapply(res3par,function(x) return(attributes(x)$pop_name))
names(res3par)<-nams

attributes(res3par)<-list(hypothesis=c(c.01="V1>V2>V3>0", c.13="V1>V2>V3",  c.31="V1>V2 & 0.6666667*V1>V3"),
                          complexity="check",
                          r2=r2,
                          pcor=pcor,
                          ratio_beta=ratio_beta,
                          p=p,
                          model="linear",
                          seed=seed,
                          iterations=length(iter),
                          studies=length(studies),
                          n=n)

saveRDS(res3par,file="simulation/output/res3par_shiny.rds")

# ## 2 par -------------------------
# #parallelize the outermost loop
r2<-c(0.09,0.13,0.4)
pcor<-c(0,0.3,0.6)
p<-c(0,0.30,0.5,0.7,0.86,1.1)
n = c(15,25,35,50,75,100,150,200,300,500,800)
ratio_beta<-c("c(3,2,1)",
              "c(1,2,3)")
studies=30
iter=1000


cond<-expand.grid(r2,pcor,p, ratio_beta, n) %>%
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4,n=Var5) %>%
  mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p, "_n",n))

ncores<-46
seed=3000

print( paste0("Prep cluster: ",Sys.time()))

cl<-makeCluster(ncores)
clusterSetRNGStream(cl, iseed=seed)

clusterEvalQ(cl, {
  library(MASS)
  library(magrittr)
  library(tidyverse)
  library(BFpack)
})
clusterExport(cl=cl, c("single_sim_ln", "q_glm", "gen_dat", "cormat", "coefs", "run_sim_ln"))

clusterExport(cl=cl, varlist=c("cond", "seed", "iter", "studies"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

res2par<-parLapply(cl,
                   1:nrow(cond),
                   function(i){

                     listel<-run_sim_ln(r2=cond[i,]$r2,  # R-squared of the regression model
                                        pcor=cond[i,]$pcor,  # correlation between the predictors
                                        ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
                                        # Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                                        p=cond[i,]$p, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
                                        hypothesis=c(c.01="V1>V2>0", c.13="V1>V2"),
                                        n=cond[i,]$n,  #sample size
                                        model="linear",
                                        studies=studies,
                                        iterations=iter)

                     attributes(listel)$pop_name<-cond[i,"pop_name"]
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

nams<- sapply(res2par,function(x) return(attributes(x)$pop_name))
names(res2par)<-nams

attributes(res2par)<-list(hypothesis=c(c.01="V1>V2>0", c.13="V1>V2"),
                          complexity="check",
                          r2=r2,
                          pcor=pcor,
                          ratio_beta=ratio_beta,
                          p=p,
                          model="linear",
                          seed=seed,
                          iterations=length(iter),
                          studies=length(studies),
                          n=n)

saveRDS(res2par,file="simulation/output/res2par_shiny.rds")

# 
# ## 1 par -------------------------
# #parallelize the outermost loop
# r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
# pcor<-c(0,0.15,0.3,0.45,0.6,0.75)
# p<-c(0,0.15,0.30,0.45,0.6,0.75,0.86,1)
# ratio_beta<-c("c(1)")
# 
# cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
#   rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4)%>% 
#   mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p))
# 
# seed=1000
# ncores<-7
# 
# 
# print( paste0("Prep cluster: ",Sys.time()))
# 
# cl<-makeCluster(ncores)
# clusterSetRNGStream(cl, iseed=seed)
# 
# clusterEvalQ(cl, {
#   library(MASS)
#   library(magrittr)
#   library(tidyverse)
#   library(BFpack)
# })
# clusterExport(cl=cl, c("single_sim_ln", "q_glm", "gen_dat", "cormat", "coefs", "run_sim_ln"))
# 
# clusterExport(cl=cl, varlist=c("cond", "seed"),envir = environment())
# 
# print(paste0("Start sim: ",Sys.time()))
# 
# res1par<-parLapply(cl,
#                    1:nrow(cond),
#                    function(i){
#                      
#                      listel<-run_sim_ln(r2=cond[i,]$r2,
#                                         pcor=cond[i,]$pcor,
#                                         hypothesis=c(c.5="V1>0"),
#                                         ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),
#                                         p=cond[i,]$p,
#                                         n = c(15,25,35,50,75,100,150,200,300,500,800),
#                                         model="linear",
#                                         studies=2,
#                                         iterations=2,
#                                         ncores=1,
#                                         seed=seed)
#                      names(listel)<-c(15,25,35,50,75,100,150,200,300,500,800)
#                      attributes(listel)$pop_name<-cond[i,"pop_name"]
#                      return(listel)
#                    }
# )
# 
# stopCluster(cl)
# print(paste0("End sim: ",Sys.time()))
# 
# saveRDS(res1par,file="simulation/res1par.rds")

