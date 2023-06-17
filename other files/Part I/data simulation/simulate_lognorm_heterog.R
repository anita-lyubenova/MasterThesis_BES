source("other files/Part I/data simulation/simulation functions.R")

# H1_r.13_pcor.3_b321_p0_linear ---------------------------------------------------------------------------------

H1_r.13_pcor.3_b321_p0_linear <-
  run_sim_ln(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=40,
          iterations=1000,
          ncores=7,
          seed=123)

saveRDS(H1_r.13_pcor.3_b321_p0_linear,"Part I/data simulation/output_lognorm/H1_r.13_pcor.3_b321_p0_linear.rds")



# H1_r.13_pcor.3_b321_p.5_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p.5_linear <-
  run_sim_ln(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.50,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=40,
          iterations=1000,
          ncores=7,
          seed=234)
saveRDS(H1_r.13_pcor.3_b321_p.5_linear,"Part I/data simulation/output_lognorm/H1_r.13_pcor.3_b321_p.5_linear.rds")



# Hc_r.13_pcor.3_b123_p0_linear ---------------------------------------------------------------------------------


Hc_r.13_pcor.3_b123_p0_linear <- 
  run_sim_ln(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(1,2,3),
          p=0,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=40,
          iterations=1000,
          ncores=7,
          seed=456)

saveRDS(Hc_r.13_pcor.3_b123_p0_linear,"Part I/data simulation/output_lognorm/Hc_r.13_pcor.3_b123_p0_linear.rds")


# H1_r.13_pcor.3_b321_p.75_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p.75_linear <-
  run_sim_ln(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.75,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=40,
          iterations=1000,
          ncores=7,
          seed=111)
saveRDS(H1_r.13_pcor.3_b321_p.75_linear,"Part I/data simulation/output_lognorm/H1_r.13_pcor.3_b321_p.75_linear.rds")

#H1_r.13_pcor.3_b321_p.86_linear ---------------------------------------------------------------------------------
H1_r.13_pcor.3_b321_p.86_linear <-
  run_sim_ln(r2=0.13,
          pcor=0.3,
          hypothesis="V1>V2>V3",
          ratio_beta=c(3,2,1),
          p=0.86,
          n = c(50,75,100,150,200,300),
          model="linear",
          studies=40,
          iterations=1000,
          ncores=7,
          seed=111)
saveRDS(H1_r.13_pcor.3_b321_p.86_linear,"Part I/data simulation/output_lognorm/H1_r.13_pcor.3_b321_p.86_linear.rds")

