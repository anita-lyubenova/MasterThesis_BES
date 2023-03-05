#H0 is the most parsimonious common true hypothesis (MPCTH)

source("RRrepo/scripts/functions.R")

## Data simulation 1: ES_Hi = ES_Hc 
#Conditions: 
# results in BF[studies, hypotheses, ratio H1:Hc, iter, sample size n]

#Simulation conditions --------------------------------------------------------
r2=.13 #effect size r-squared
pcor<-0.3 #correlation between the predictor variables
n<-c(100,350,600,1000) #sample sizes
studies<-40 #number of studies
hypothesis<-"V1=V2=V3; V1>V2>V3" #tested hypotheses
models <- c("normal") #linear, logistic or probit regression


#specify number of iterations only if there is no object "iter" in the environment
iter<-1000 


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
        
        print(paste("MPCTH(H0):: Iteration i:",i, "Sample size s:", s, "Study t:", t, "Ratio r:",r))
        
  
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


save.image(file="RRrepo/workspaces/Simulate BFs_MPCTH(H0).RData")

# Aggregate PMPs--------------------------------------
load("RRrepo/workspaces/Simulate BFs_MPCTH(H0).RData")

source("RRrepo/scripts/Aggregate PMPs.R")

hyp_index<-c("i", "c", "u","0")
two.way<-combn(hyp_index, 2) %>% t() %>% apply(1, function(x) paste0(x, collapse = ""))
three.way<-combn(hyp_index, 3) %>% t() %>% apply(1, function(x) paste0(x, collapse = ""))
four.way<-combn(hyp_index, 4) %>% t() %>% apply(1, function(x) paste0(x, collapse = ""))
all.hyps<-c(two.way, three.way, four.way)

PMP_H0TRUE<-list()
for(i in 1:length(all.hyps)){
  PMP_H0TRUE[[all.hyps[i]]] <- aggregatePMP(BF=BF, #the simulated BFs from the simulate_BF_.R scripts
                     hyp=unlist(strsplit(all.hyps[i],split = "")), #which hypotheses are of interest
                     iter=iter,
                     studies=studies
  )
  
}


#Save ------------------------------------------

#save the simulation conditions
PMP_H0TRUE[c("iter", "n_studies", "n", "ratio_beta", "r2", "pcor", "models")]<-list(iter, studies, n, ratio_beta.H0, r2, pcor, models)

save(PMP_H0TRUE,file="RRrepo/workspaces/PMPs/PMP_H0TRUE.RData")


