#THe script is the same as Simulate_BFs_ES(H1)=ES(Hc).R apart from that all studies come from H1 and thus
#H1 is the most parsimonious common true hypothesis (MPCTH)

source("scripts/functions.R")

## Data simulation 1: ES_Hi = ES_Hc 
#Conditions: 
# results in BF[studies, hypotheses, ratio H1:Hc, iter, sample size n]

#Simulation conditions --------------------------------------------------------
r2=.13 #effect size r-squared
pcor<-0.3 #correlation between the predictor variables
n<-c(100,350) #sample sizes
studies<-40 #number of studies
hypothesis<-"V1=V2=V3; V1>V2>V3" #tested hypotheses
models <- c("normal") #linear, logistic or probit regression


#specify number of iterations only if there is no object "iter" in the environment
if(!exists("iter")){
  iter<-1000 
}

#ratio between the regression coefficients b1:b2:b3>; larger number corresponds to larger coefficient
ratio_beta.Hi<-c(3,2,1)
ratio_beta.Hc<-c(1,2,3)

#determines the number of strudies from each population H1 or Hc:
#whether Hi:Hc = 1:1 or 3:1, respectively
#ratio_HiHc<-c(2,4)
#in this condition all studies come from H1
ratio_HiHc<-1

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
                          c("Hi:Hc=1:0"),
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
    
    for(r in 1:length(ratio_HiHc)){
      
      for(t in 1:studies){
        
        print(paste("MPCTH(H1):: Iteration i:",i, "Sample size s:", s, "Study t:", t, "Ratio r:",r))
        
        #have ratio_beta conform with Hc or with Hi
        #Hi:Hc study ratio will be either 1:1 (every 2nd study comes from Hc)
        #or 1:3, every fourth study comes from Hc)
        if(t %% ratio_HiHc[r] == 0){ #all studies  => Hi
          ratio_beta<-ratio_beta.Hi
        }else{                       #else Hc (in this script: never)
          ratio_beta<-ratio_beta.Hc 
        }
        
        #obtain BFiu
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

save.image(file="workspaces/Simulate BFs_MPCTH(H1).RData")

# Aggregate PMPs--------------------------------------
load("RRrepo/workspaces/Simulate BFs_MPCTH(H1).RData")

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


