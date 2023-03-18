#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [comple_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

library(tidyverse)
load("Outputs/accuracy/dat (merged simulated files).RData")

#transform BFs to PMPs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies #number of studies to aggregate over, max 40
){
  hyp_index<-substr(hyp,2,2)
  BF<-x$BF
  BF<-BF[1:studies,substr(dimnames(BF)[[2]],3,3) %in% hyp_index,,,,drop=FALSE]
  
  nom<-aperm(BF, perm=c(1,3,4,5,2))
  denom<-rowSums(nom, dims = 4)
  PMP_perm<-nom/replicate(length(hyp),denom)
  #PMP<-aperm(PMP_perm, perm = c(1,5,2,3,4))

  PMP_t<-PMP_perm

  for(t in 2:studies){
    nom_t<-PMP_t[t-1,,,,]*nom[t,,,,]
    denom_t<-rowSums(nom_t, dims=3)
    PMP_t[t,,,,]<-nom_t/replicate(length(hyp), denom_t)
  }
  
  PMP_t<-aperm(PMP_t, perm = c(1,5,2,3,4))
  dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))
  return(PMP_t)
}
a<-aggregatePMP(dat,
                hyp=c("H1","Hc", "Hu"),
                studies = 10)
a[,,,,5]
