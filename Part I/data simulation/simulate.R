library(parallel)
library(foreach)
library(doParallel)
library(doRNG)
library(doSNOW)
library(abind)

source("Part I/data simulation/simulation functions.R")

# H1_r.13_pcor.3_b321_p0_linear ---------------------------------------------------------------------------------


H1_r.13_pcor.3_b321_p0_linear <-
      run_sim(r2=0.13,
             pcor=0.3,
             hypothesis="V1>V2>V3",
             ratio_beta=c(3,2,1),
             p=0,
             n = c(50,75,100,150,200,300),
             model="linear",
             studies=30,
             iterations=1000,
             ncores=7,
             chunkSize=2,
             seed=123)

saveRDS(H1_r.13_pcor.3_b321_p0_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p0_linear.rds")




# H1_r.13_pcor.3_b321_p.50_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p0_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.50,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=1000,
          ncores=7,
          chunkSize=2,
          seed=123)
saveRDS(H1_r.13_pcor.3_b321_p.50_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p.50_linear.rds")




# H1_r.13_pcor.3_b321_p.68_linear ---------------------------------------------------------------------------------

H1_r.13_pcor.3_b321_p.68_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.68,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=1000,
          ncores=7,
          chunkSize=2,
          seed=123)

saveRDS(H1_r.13_pcor.3_b321_p.68_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p.68_linear.rds")


# Hc_r.13_pcor.3_b123_p0_linear ---------------------------------------------------------------------------------

  
Hc_r.13_pcor.3_b123_p0_linear <- 
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(1,2,3),
          p=0,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=1000,
          ncores=7,
          chunkSize=2,
          seed=123)

saveRDS(Hc_r.13_pcor.3_b123_p0_linear,"Part I/data simulation/output/Hc_r.13_pcor.3_b123_p0_linear.rds")






#reproducibility test ---------------------------------------------------------------------------------------
#run twice the same small scale simulation to confirm the structure and check the reproducibility


test1<-run_sim(r2=0.13,
        pcor=0.3,
        hypothesis="V1>V2>V3",
        ratio_beta=c(3,2,1),
        p=0,
        n = c(50,75),
        model="linear",
        studies=2,
        iterations=5,
        ncores=3,
        chunkSize=2,
        seed=123
)

test2<-run_sim(r2=0.13,
               pcor=0.3,
               hypothesis="V1>V2>V3",
               ratio_beta=c(3,2,1),
               p=0,
               n = c(50,75),
               model="linear",
               studies=2,
               iterations=5,
               ncores=3,
               chunkSize=2,
               seed=123
)

#the results replicate
test1[[1]]==test2[[1]]
test1[[2]]==test2[[2]]

saveRDS(test1,"Part I/data simulation/output/test1.rds")
saveRDS(test2,"Part I/data simulation/output/test2.rds")
