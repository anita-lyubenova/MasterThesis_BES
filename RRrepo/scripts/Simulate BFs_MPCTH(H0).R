library(doParallel)
library(plyr)

#H0 is the most parsimonious common true hypothesis (MPCTH)

source("RRrepo/scripts/functions.R")

## Data simulation 1: ES_Hi = ES_Hc 
#Conditions: 
# results in BF[studies, hypotheses, ratio H1:Hc, iter, sample size n]

#Simulation conditions --------------------------------------------------------
r2=.13 #effect size r-squared
pcor<-0.3 #correlation between the predictor variables
n<-c(100,350,500,900) #sample sizes
studies<-40 #number of studies
hypothesis<-"V1=V2=V3; V1>V2>V3" #tested hypotheses
models <- c("normal") #linear, logistic or probit regression


#specify number of iterations only if there is no object "iter" in the environment
if(!exists("iter")){
  iter<-1000 
}

#ratio between the regression coefficients b1:b2:b3>; larger number corresponds to larger coefficient
ratio_beta.H0<-c(1,1,1)
# ratio_beta.Hi<-c(3,2,1)
# ratio_beta.Hc<-c(1,2,3)

ratio_HiHc<-0

#Placeholder for the results-----------------------------------------------------
BF<-array(data = NA,
          dim=c(studies, 
                4, # hypotheses
                length(ratio_HiHc), #1:1 or 3:1 ratio of studies coming from Hi and H
                iter,
                length(n) #different sample sizes
          ),
          dimnames = list(c(1:studies),
                          c("BF0u", "BFiu", "BFcu", "BFu"),
                          c("Hi:Hc=0:0"),
                          c(1:iter),
                          paste0("n = ", n)
          ))

#The BF of Hu is always 1
BF[,4,,,]<-1

#Simulation loop---------------------------------------------------------------
i<-1
s<-1
r<-1
t<-2

set.seed(123)

# loop to fill in BFiucu
for(i in 1:iter){
  
  for(s in 1:length(n)){
    
    for(r in 1:1){ #length(ratio_HiHc)
      
      for(t in 1:studies){
        
        print(paste("MPCTH(H1):: Iteration i:",i, "Sample size s:", s, "Study t:", t, "Ratio r:",r))
        
  
        ratio_beta<-ratio_beta.H0
          
        #obtain BF_u for Hi, Hc and H0
        BF[t,1:3,r,i,s]<-gen_dat(r2=r2,
                                 betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                                 rho=cormat(pcor, length(ratio_beta)),
                                 n=n[s],
                                 "normal")%$%
          lm(Y ~ V1 + V2 +V3) %>%
          bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
        
        
      } #end studies loop t
    }#end H1:Hc ratio loop r
  }# end sample size loop s
} #end iterations loop i


#Attempt to parallelize code---------------------------------------------------------------------------------------
#i.e. have the sample size conditions run on separate cores

##Placeholder for the results-----
#remove n


simH0_n<-function(n){
  r2=.13 #effect size r-squared
  pcor<-0.3 #correlation between the predictor variables
 # n<-c(100,350,500,900) #sample sizes
  studies<-40 #number of studies
  hypothesis<-"V1=V2=V3; V1>V2>V3" #tested hypotheses
  models <- c("normal") #linear, logistic or probit regression

  iter<-3
  #ratio between the regression coefficients b1:b2:b3>; larger number corresponds to larger coefficient
  ratio_beta.H0<-c(1,1,1)
  ratio_HiHc<-0
  
  BF<-array(data = NA,
            dim=c(studies, 
                  4, # hypotheses
                  length(ratio_HiHc), #1:1 or 3:1 ratio of studies coming from Hi and H
                  iter
            ),
            dimnames = list(c(1:studies),
                            c("BF0u", "BFiu", "BFcu", "BFu"),
                            c("Hi:Hc=0:0"),
                            c(1:iter)
            ))
  
  #The BF of Hu is always 1
  BF[,4,,]<-1
  
  for(i in 1:iter){
    
      for(r in 1:1){ #length(ratio_HiHc)
        
        for(t in 1:studies){
          
          print(paste("MPCTH(H1):: Iteration i:",i, "Study t:", t, "Ratio r:",r))
          
          
          ratio_beta<-ratio_beta.H0
          
          #obtain BF_u for Hi, Hc and H0
          BF[t,1:3,r,i]<-gen_dat(r2=r2,
                                   betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                                   rho=cormat(pcor, length(ratio_beta)),
                                   n=n,
                                   "normal")%$%
            lm(Y ~ V1 + V2 +V3) %>%
            bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
          
          
        } #end studies loop t
      }#end H1:Hc ratio loop r
  } #end iterations loop i
  
  return(BF)
}


a<-simH0_n(n=10)
dimnames(a)
library(magrittr)
n<-c(100, 350)

cores <- detectCores()
cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl, cores=2)

clusterEvalQ(cl, { library(magrittr); library(bain); })
#clusterEvalQ(cl,  library(magrittr))
#b<-llply(n, simH0_n, .parallel = TRUE) #
b<-parLapply(cl=cl, X=n, simH0_n)

stopCluster(cl)
stopImplicitCluster()
getDoParWorkers()

b[[1]]
b[[2]]
bb<-abind::abind(b[[1]],b[[2]],along=5)


dim(bb)
save.image(file="RRrepo/workspaces/Simulate BFs_MPCTH(H0).RData")

# Aggregate PMPs--------------------------------------
load("RRrepo/workspaces/Simulate BFs_MPCTH(H0).RData")

source("RRrepo/scripts/Aggregate PMPs.R")

aggrPMPic_H1TRUE<-
  aggregatePMP(BF=BF, #the simulated BFs from the simulate_BF_.R scripts
               hyp=c("i", "c"), #which hypotheses are of interest
               iter=iter,
               studies=studies
  )
aggrPMPicu_H1TRUE<-
  aggregatePMP(BF=BF, #the simulated BFs from the simulate_BF_.R scripts
               hyp=c("i", "c", "u"), #which hypotheses are of interest
               iter=iter,
               studies=studies
  )

aggrPMP0iu_H1TRUE<-
  aggregatePMP(BF=BF, #the simulated BFs from the simulate_BF_.R scripts
               hyp=c("0","i", "u"), #which hypotheses are of interest
               iter=iter,
               studies=studies
  )

aggrPMP0icu_H1TRUE<-
  aggregatePMP(BF=BF, #the simulated BFs from the simulate_BF_.R scripts
               hyp=c("0","i", "c", "u"), #which hypotheses are of interest
               iter=iter,
               studies=studies
  )

#Save ------------------------------------------

PMP_H1TRUE<-list(ic=aggrPMPic_H1TRUE,
                 icu=aggrPMPicu_H1TRUE,
                 iu0=aggrPMP0iu_H1TRUE,
                 icu0=aggrPMP0icu_H1TRUE,
                 iter=iter,
                 n_studies=studies,
                 n=n,
                 ratio_beta.Hi=ratio_beta.Hi,
                 ratio_beta.Hc=NA,
                 r2=r2,
                 pcor=pcor,
                 models=models
)

save(PMP_H1TRUE,file="RRrepo/workspaces/PMPs/PMP_H1TRUE.RData")


