#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [comple_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

library(tidyverse)
load("Outputs/accuracy/dat (merged simulated files).RData")

x<-dat
T<-10
#transform BFs to PMPs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu")
){
  hyp_index<-substr(hyp,2,2)
  BF<-x$BF
  BF<-BF[,substr(dimnames(BF)[[2]],3,3) %in% hyp_index,,,,drop=FALSE]
  
  nom<-aperm(BF, perm=c(1,3,4,5,2))
  denom<-rowSums(nom, dims = 4)
  PMP_perm<-nom/replicate(length(hyp),denom)
  PMP<-aperm(PMP_perm, perm = c(1,5,2,3,4))


  
  # PMP_temp<-PMP_perm[-40,,,,]
  # BF_temp<-nom[-1,,,,]
  # nom_t<-PMP_temp*BF_temp
  # denom_t<-rowSums(nom_t, dims = 4)
  # PMP_t<-nom_t/replicate(length(hyp),denom_t)
  # PMP_t<-aperm(PMP_t,c(1,5,2,3,4))
  
  
return(PMP_t)
}
aggregatePMP(dat, hyp=c("H1","Hc", "Hu"))

##############################
a<-matrix(1:8, ncol=2, nrow=4, byrow = T)

b<-array(1:12, c(2,3,2))
replicate(2, b)





#function to aggregate the PMPs for a set of specified hypotheses
#reteruns the aggregate PMPs and sim conditions in a list
aggregatePMP<-function(x, #a list created with sim_BES()
                        hyp=c("H1", "Hc", "Hu"), #which hypothesis are to be tested interest
                        # iter=1000,
                        studies=NULL #number of studies to aggregate over
                        
){
  BF<-x$BF
  iter<-x$iter
  n.hyp<-length(hyp)
  hyp_index<-substr(hyp,2,2)
  n<-x$n
  
  #if none specified, all studies will be aggregated over - this may take longer time
  if(is.null(studies)){
    studies<-x$studies
  }
  #subset only tested hypotheses, eg Hi vs. Hc vs. Hu (exlude H0)
  BF.temp<-BF[,substr(dimnames(BF)[[2]],3,3) %in% hyp_index,,,drop=FALSE]  
  
  #placeholder for the aggregate PMPs
  aggrPMP<-BF.temp
  
  for(i in 1:iter){       # for each iteration t
    for(t in 1:studies){    # for each study t
      for(h in 1:n.hyp){  #for each hypothesis h
        for(r in 1:length(dimnames(BF.temp)[[3]])){ #for each population 
          #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
          aggrPMP[t,h,r,i]<-prod(BF.temp[1:t,h,r,i])/sum(apply(matrix(BF.temp[1:t,,r,i],nrow = t,ncol = n.hyp),2,prod))
          
        }
      } 
    }
  }
  
  dimnames(aggrPMP)[[2]]<-paste0("PMP_", substr(dimnames(BF.temp)[[2]],3,3))
  
  return(list(aggrPMP=aggrPMP,
              r2=x$r2,
              pcor=x$pcor,
              n=n,
              hypotheses=hyp,
              populations=x$populations,
              model=x$model,
              iter=iter,
              studies=studies
  ))
}









