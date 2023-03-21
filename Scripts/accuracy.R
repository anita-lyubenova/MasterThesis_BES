#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [compl_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

load("Outputs/accuracy/dat_merged.RData")
source("Scripts/accuracy/accuracy_functions.R")



#
aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 40) %>% 
  accuracyPMP(hyp_to_pop = c(H1="HETEROG_H1p.1", Hu="TRUE_H0")) %>% 
  acc_corrplot()/
  aggregatePMP(x=dat,
               h=c("H1" ,"Hu"),
               studies = 40) %>% 
  accuracyPMP(hyp_to_pop = c(H1="HETEROG_H1p.3", Hu="TRUE_H0")) %>% 
  acc_corrplot()



aggregatePMP(x=dat,
             h=c("H1","Hc"),
             studies = 40) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="HETEROG_H1p.5")) %>% 
  acc_corrplot()
