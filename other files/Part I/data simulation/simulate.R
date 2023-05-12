
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
             iterations=10000,
             ncores=7,
             seed=123)

saveRDS(H1_r.13_pcor.3_b321_p0_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p0_linear.rds")



# H1_r.13_pcor.3_b321_p.5_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p.5_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.50,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=10000,
          ncores=7,
          seed=234)
saveRDS(H1_r.13_pcor.3_b321_p.5_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p.5_linear.rds")




# H1_r.13_pcor.3_b321_p.68_linear ---------------------------------------------------------------------------------
#Linden & Honekopp T=0.18+0.3*d
#avg d=.47 => T = 0.18 + 0.3 * 0.47 = 0.32 ::: T=p*d => p = T/d = 0.32/0.47 = 0.6808511

H1_r.13_pcor.3_b321_p.68_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.68,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=10000,
          ncores=7,
          seed=345)

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
          iterations=10000,
          ncores=7,
          seed=456)

saveRDS(Hc_r.13_pcor.3_b123_p0_linear,"Part I/data simulation/output/Hc_r.13_pcor.3_b123_p0_linear.rds")



# Additionals: heterogeneity p -----------------------------------------------------------
#Linden & Honekopp 

# H1_r.13_pcor.3_b321_p.75_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p.75_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.75,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=10000,
          ncores=7,
          seed=111)
saveRDS(H1_r.13_pcor.3_b321_p.75_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p.75_linear.rds")

#H1_r.13_pcor.3_b321_p.86_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p.86_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.86,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=10000,
          ncores=7,
          seed=111)
saveRDS(H1_r.13_pcor.3_b321_p.86_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p.86_linear.rds")


# H1_r.13_pcor.3_b321_p1.09_linear ---------------------------------------------------------------------------------
#Linden & Honekopp: T=p*d => p=T/d 
#compute p for all included meta-analyses in L&H
#average p = 1.09 (SD=0.82)

H1_r.13_pcor.3_b321_p1.09_linear <-
  run_sim(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=1.09,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=30,
          iterations=10000,
          ncores=7,
          seed=222)
saveRDS(H1_r.13_pcor.3_b321_p1.09_linear,"Part I/data simulation/output/H1_r.13_pcor.3_b321_p1.09_linear.rds")

#reproducibility test ---------------------------------------------------------------------------------------
#run twice the same small scale simulation to confirm the structure and check the reproducibility

test1<-run_sim(r2=0.13,
               pcor=0.3,
               hypothesis="V1>V2>V3",
               ratio_beta=c(3,2,1),
               p=0,
               n = c(50,75,100),
               model="linear",
               studies=30,
               iterations=10,
               ncores=2,
               seed=123)

test2<-run_sim(r2=0.13,
               pcor=0.3,
               hypothesis="V1>V2>V3",
               ratio_beta=c(3,2,1),
               p=0,
               n = c(50,75,100),
               model="linear",
               studies=30,
               iterations=10,
               ncores=2,
               seed=123)

#the results reproduce
identical(test1, test2)

