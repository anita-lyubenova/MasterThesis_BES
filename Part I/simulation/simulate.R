#all_hyp<-c(c.01="V1>V2>V3>0",c.50="V1-0.5*V2-V3>0")
source("~/simulation/simulation functions.R")

# 
# # Full simulation ---------------------------
# 
# r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
# pcor<-c(0,0.15,0.3,0.45,0.6,0.75)
# p<-c(0,0.15,0.30,0.45,0.6,0.75,0.86,1)
# 
# ratio_beta<-c("c(6,3,1)",
#                  "c(3,2,1)",
#                  "c(2,1.5,1)",
#                  "c(1,3,6)",
#                  "c(1,2,3)",
#                  "c(1,1.5,2)"
#                  )
# ratio_beta<-c("c(3,1)",
#               "c(2,1)",
#               "c(1.5,1)",
#               "c(1,3)",
#               "c(1,2)",
#               "c(1,1.5)"
# )
# 
# ratio_beta<-c("c(1)")
# cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
#   rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4) %>% 
#   mutate(seed=seq(from=1000, by=1, length.out=nrow(.)))
# 
# 6*6*8*6
#  subsample---------------------------------------------

## 3 par ------------------------------------
#parallelize the outermost loop
r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.3,0.75)
p<-c(0,0.45,0.86,1)

ratio_beta<-c("c(3,2,1)",
              "c(1,2,3)"
)

cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4) %>% 
  mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p))

ncores<-7
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

clusterExport(cl=cl, varlist=c("cond", "seed"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

res3par<-parLapply(cl,
          1:nrow(cond),
          function(i){
  
  listel<-run_sim_ln(r2=cond[i,]$r2,
             pcor=cond[i,]$pcor,
             hypothesis=c(c.01="V1>V2>V3>0", c.13="V1>V2>V3",  c.31="V1>V2 & 0.6666667*V1>V3"),
             ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),
             p=cond[i,]$p,
             n = c(15,25,35,50,75,100,150,200,300,500,800),
             model="linear",
             studies=3,
             iterations=2,
             ncores=1,
             seed=seed)
  names(listel)<-c(15,25,35,50,75,100,150,200,300,500,800)
  
  attributes(listel)$pop_name<-cond[i,"pop_name"]
  return(listel)
  }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

saveRDS(res3par,file="~/simulation/res3par.rds")



## 2 par -------------------------
#parallelize the outermost loop
r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.3,0.75)
p<-c(0,0.45,0.86,1)

ratio_beta<-c("c(2,1)",
              "c(1,2)"
)


cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4) %>% 
  mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p))


ncores<-7
seed=2000

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

clusterExport(cl=cl, varlist=c("cond", "seed"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

res2par<-parLapply(cl,
                   1:nrow(cond),
                   function(i){
                     
                     listel<-run_sim_ln(r2=cond[i,]$r2,
                                        pcor=cond[i,]$pcor,
                                        hypothesis=c(c.01="V1>V2>0", c.13="V1>V2"),
                                        ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),
                                        p=cond[i,]$p,
                                        n = c(15,25,35,50,75,100,150,200,300,500,800),
                                        model="linear",
                                        studies=2,
                                        iterations=2,
                                        ncores=1,
                                        seed=seed)
                     names(listel)<-c(15,25,35,50,75,100,150,200,300,500,800)
                     attributes(listel)$pop_name<-cond[i,"pop_name"]
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

saveRDS(res2par,file="~/simulation/res2par.rds")


## 1 par -------------------------
#parallelize the outermost loop
r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.3,0.75)
p<-c(0,0.45,0.86,1)
ratio_beta<-c("c(1)")

cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4)%>% 
  mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p))

seed=1000
ncores<-7


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

clusterExport(cl=cl, varlist=c("cond", "seed"),envir = environment())

print(paste0("Start sim: ",Sys.time()))

res1par<-parLapply(cl,
                   1:nrow(cond),
                   function(i){
                     
                     listel<-run_sim_ln(r2=cond[i,]$r2,
                                        pcor=cond[i,]$pcor,
                                        hypothesis=c(c.5="V1>0"),
                                        ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),
                                        p=cond[i,]$p,
                                        n = c(15,25,35,50,75,100,150,200,300,500,800),
                                        model="linear",
                                        studies=2,
                                        iterations=2,
                                        ncores=1,
                                        seed=seed)
                     names(listel)<-c(15,25,35,50,75,100,150,200,300,500,800)
                     attributes(listel)$pop_name<-cond[i,"pop_name"]
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

saveRDS(res1par,file="~/simulation/res1par.rds")




# ###################################################################################################################################
#   Full simulation
#  ###################################################################################################################################
# 
# ## 3 par ------------------------------------
# #parallelize the outermost loop
# r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
# pcor<-c(0,0.15,0.3,0.45,0.6,0.75)
# p<-c(0,0.15,0.30,0.45,0.6,0.75,0.86,1)
# 
# ratio_beta<-c("c(6,3,1)",
#               "c(3,2,1)",
#               "c(2,1.5,1)",
#               "c(1,3,6)",
#               "c(1,2,3)",
#               "c(1,1.5,2)"
# )
# 
# cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
#   rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4) %>% 
#   mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p))
# 
# ncores<-7
# seed=3000
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
# res3par<-parLapply(cl,
#                    1:nrow(cond),
#                    function(i){
#                      
#                      listel<-run_sim_ln(r2=cond[i,]$r2,
#                                         pcor=cond[i,]$pcor,
#                                         hypothesis=c(c.01="V1>V2>V3>0", c.13="V1>V2>V3",  c.31="V1>V2 & 0.6666667*V1>V3"),
#                                         ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),
#                                         p=cond[i,]$p,
#                                         n = c(15,25,35,50,75,100,150,200,300,500,800),
#                                         model="linear",
#                                         studies=3,
#                                         iterations=2,
#                                         ncores=1,
#                                         seed=seed)
#                      names(listel)<-c(15,25,35,50,75,100,150,200,300,500,800)
#                      
#                      attributes(listel)$pop_name<-cond[i,"pop_name"]
#                      return(listel)
#                    }
# )
# 
# stopCluster(cl)
# print(paste0("End sim: ",Sys.time()))
# 
# saveRDS(res3par,file="~/simulation/res3par.rds")
# str(res3par)
# res3par[[1]]
# 
# 
# ## 2 par -------------------------
# #parallelize the outermost loop
# r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
# pcor<-c(0,0.15,0.3,0.45,0.6,0.75)
# p<-c(0,0.15,0.30,0.45,0.6,0.75,0.86,1)
# 
# ratio_beta<-c("c(3,1)",
#               "c(2,1)",
#               "c(1.5,1)",
#               "c(1,3)",
#               "c(1,2)",
#               "c(1,1.5)"
# )
# 
# 
# cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
#   rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4) %>% 
#   mutate(pop_name=paste0("r", r2, "_pcor", pcor,"_b",readr::parse_number(as.character(ratio_beta)), "_p", p))
# 
# 
# ncores<-7
# seed=2000
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
# res2par<-parLapply(cl,
#                    1:nrow(cond),
#                    function(i){
#                      
#                      listel<-run_sim_ln(r2=cond[i,]$r2,
#                                         pcor=cond[i,]$pcor,
#                                         hypothesis=c(c.01="V1>V2>0", c.13="V1>V2"),
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
# saveRDS(res2par,file="~/simulation/res2par.rds")
# 
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
# saveRDS(res1par,file="~/simulation/res1par.rds")

