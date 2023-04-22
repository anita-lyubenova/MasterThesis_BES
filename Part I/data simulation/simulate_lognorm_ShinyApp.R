#all_hyp<-c(c.01="V1>V2>V3>0",c.50="V1-0.5*V2-V3>0")
source("Part I/data simulation/simulation functions.R")



# H1_r.13_pcor.3_b321_p0_linear ---------------------------------------------------------------------------------

H1_r.13_pcor.3_b321_p0_linear <-
  run_sim_ln(r2=0.13,
             pcor=0.3,
             hypothesis=all_hyp,
             ratio_beta=c(3,2,1),
             p=0,
             n = c(25,50,75,100,150,200,300),
             model="linear",
             studies=40,
             iterations=1000,
             ncores=7,
             seed=123)

saveRDS(H1_r.13_pcor.3_b321_p0_linear,"Part I/data simulation/ouput_lognorm_ShinyApp/H1_r.13_pcor.3_b321_p0_linear.rds")

# Full simulation ---------------------------

r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.15,0.3,0.45,0.6,0.75)
p<-c(0,0.15,0.30,0.45,0.6,0.75,0.86,1)

ratio_beta<-c("c(6,3,1)",
                 "c(3,2,1)",
                 "c(2,1.5,1)",
                 "c(1,3,6)",
                 "c(1,2,3)",
                 "c(1,1.5,2)"
                 )
ratio_beta<-c("c(3,1)",
              "c(2,1)",
              "c(1.5,1)",
              "c(1,3)",
              "c(1,2)",
              "c(1,1.5)"
)

ratio_beta<-c("c(1)")

# cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
#   rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4) %>% 
#   mutate(seed=seq(from=1000, by=1, length.out=nrow(.)))
# 
# 6*6*8*6
#  subsample---------------------------------------------

## 3 par
#only 3 parameters, few iterations and few studies
# 
# ### ratio_beta = 3,2,1   
# r2<-c(0.13,0.5)
# pcor<-c(0,0.75)
# p<-c(0,0.86)
# ratio_beta<-c("c(3,2,1)",
#               "c(1,2,3)"
# )
# 
# 
# cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
#   rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4)%>% 
#   mutate(seed=seq(from=3000, by=1, length.out=nrow(.)))
# 
# #hypothesis=c(c.01="V1>V2>V3>0", c.13="V1>V2>V3",  c.31="V1>V2 & 0.6666667*V1>V3")
# # res3par<-list()
# # 
# # for(i in 1:nrow(cond)){
# #  # varname<-paste0("r",cond[i,"r2"], "_pcor", cond[i,"pcor"],"_b321", "_p", cond[i,"p"],"_seed", cond[i,"seed"])
# #   print(paste0("Condition ", i))
# #   system.time({
# #     res3par[[i]]<-run_sim_ln(r2=cond[i,]$r2,
# #                              pcor=cond[i,]$pcor,
# #                              hypothesis=c(c.01="V1>V2>V3>0", c.13="V1>V2>V3",  c.31="V1>V2 & 0.6666667*V1>V3"),
# #                              ratio_beta=eval(parse(text = as.character(cond[i,]$ratio_beta))),
# #                              p=cond[i,]$p,
# #                              n = c(15,25,35,50,75,100,150,200,300,500,800),
# #                              model="linear",
# #                              studies=2,
# #                              iterations=2,
# #                              ncores=1,
# #                              seed=cond[i,]$seed)
# #   names(res3par)[i]<-paste0("r",cond[i,"r2"], "_pcor", cond[i,"pcor"],"_b",readr::parse_number(as.character(cond[i,]$ratio_beta)), "_p", cond[i,"p"],"_seed", cond[i,"seed"])
# #   })
# # }
# #saveRDS(res3par,file="Part I/data simulation/output_lognorm_ShinyApp/res3par.rds")

## 3 par ------------------------------------
#parallelize the outermost loop
r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.3,0.75)
p<-c(0,0.45,0.86,1)

ratio_beta<-c("c(3,2,1)",
              "c(1,2,3)"
)


cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4)

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
             studies=30,
             iterations=1000,
             ncores=1,
             seed=seed)
  names(listel)<-c(15,25,35,50,75,100,150,200,300,500,800)
 # names(listel)<-paste0("r",cond[i,"r2"], "_pcor", cond[i,"pcor"],"_b",readr::parse_number(as.character(cond[i,]$ratio_beta)), "_p", cond[i,"p"],"_seed", cond[i,"seed"])
  return(listel)
  }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

saveRDS(res3par,file="Part I/data simulation/output_lognorm_ShinyApp/res3par.rds")
str(res3par)
res3par[[1]]


## 2 par -------------------------
#parallelize the outermost loop
r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.3,0.75)
p<-c(0,0.45,0.86,1)

ratio_beta<-c("c(2,1)",
              "c(1,2)"
)


cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4)

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
                     # names(listel)<-paste0("r",cond[i,"r2"], "_pcor", cond[i,"pcor"],"_b",readr::parse_number(as.character(cond[i,]$ratio_beta)), "_p", cond[i,"p"],"_seed", cond[i,"seed"])
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

saveRDS(res2par,file="Part I/data simulation/output_lognorm_ShinyApp/res2par.rds")


## 1 par -------------------------
#parallelize the outermost loop
r2<-c(0.05,0.13,0.2,0.3, 0.4,0.5)
pcor<-c(0,0.3,0.75)
p<-c(0,0.45,0.86,1)

ratio_beta<-c("c(1)"
)


cond<-expand.grid(r2,pcor,p, ratio_beta) %>% 
  rename(r2=Var1, pcor=Var2, p=Var3, ratio_beta=Var4)

ncores<-3
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
                     # names(listel)<-paste0("r",cond[i,"r2"], "_pcor", cond[i,"pcor"],"_b",readr::parse_number(as.character(cond[i,]$ratio_beta)), "_p", cond[i,"p"],"_seed", cond[i,"seed"])
                     return(listel)
                   }
)

stopCluster(cl)
print(paste0("End sim: ",Sys.time()))

saveRDS(res1par,file="Part I/data simulation/output_lognorm_ShinyApp/res1par.rds")
