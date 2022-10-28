#3-way-testing: testing 
#  H0: b1 > -MESOI & b1 > MESOI
#  H1 b1 > 0ub, where 0up is 0 upper bound which is between 0 and MESOI
#  H2 - opposite of H1, ie. b1<0lb, wheere 0lb is 0 lower bound which is between 0 and -MESOI

#Simulation conditions:
# fixed n=632,
# b1:b2 ranges from 1:1 to 4.8 to 1 (which makes q range from 0 to 0.438)
#

load("Outputs/exploration/q.general.perf.RData")


q<-q.general.perf[1:25] # I arbitrarily chose teh first 25 effect sizes to reduce computation times (while there is still enough variation in power)

#Determine MESOI and 0-upper-bound
#MESOI could be what Cohen defined as small q = 0.1 =>
#   b1:b2 = 1.3
#
#But how to determine the 0-upperbound in general?
#  -0up matters for (1) power of H1: 0-ub is the starting point of H1
#                   (2) power for H0: 0-ub is the maximum effect size that has to be considered under H0 with sufficient level of power
#
# - For now I would 0-ub to be q=0.063 (b1:b2 = 1.2:1) without clear rationale apart from being the q lower than the MESOI

#Determine hypotheses H0, H1 and H2
#Since my ES is not regarding the betas I have to manually check what is the difference between the betas to 
# define the hypothesis such that it reflects effect size q
#    Determine H0: q c (-0.097, 0.097) 
#    find betas for when q = 0.097 (i.e., b1:b2 = 1.3:1): 
      models <- c("normal")
      r2=.09
      pcor<-0.3
      n<-632
      ratio_beta<-c(1.3,1)
      coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal") #[1] 0.2093630 0.1610484
      #   => b1-b2 = 0.2093630 - 0.1610484 = 0.0483146
      hypothesis0 <- "V1 < V2 + 0.0483146; V1 > V2 - 0.0483146"
      hypothesis0 <- "V1 < V2 + 0.06; V1 > V2 - 0.05"   
      hypothesis0 <- "V2 - 0.0483146 < V1 < V2 + 0.0483146"
      

#    Determine H1: q c(0.063, +Inf) 
#    find betas for when q = 0.063 (i.e., b1:b2 = 1.2:1)
      models <- c("normal")
      r2=.09
      pcor<-0.3
      n<-632
      ratio_beta<-c(1.2,1)
      coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal") #[1]  0.2025158 0.1687632
      #   => b1-b2 = 0.2025158 - 0.1687632 = 0.0337526
      hypothesis1 <- "V1 > V2 + 0.0337526"
      
#    H2 is the opposite of H1 =>
      hypothesis2 <- "V1 < V2 - 0.0337526"
      
      
#... n=632(const) -------------------------------------------------------------------------
r2=.09
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 10, by=0.1)[1:25]
iter<-5000
models <- c("normal")


#q<-c()r
# BF1u<-matrix(NA, nrow = iter, ncol = length(ratio1))
# BF2u<-matrix(NA, nrow = iter, ncol = length(ratio1))
# BF0u<-matrix(NA, nrow = iter, ncol = length(ratio1))

BFiu<-array(NA,
            dim=c(iter, length(q), 3),
            dimnames = list(c(1:iter),
                            paste0("q = ",round(q,3)),
                            c("BF1u", "BF2u", "BF0u")
))

for(s in 1:length(ratio1)){

  ratio_beta<-c(ratio1[s],1)
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFic 
    mod<-gen_dat(r2=r2,
                betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                rho=cormat(pcor, length(ratio_beta)),
                n=n,
                "normal")%$%
      lm(Y ~ V1 + V2)
    
    BFiu[i,s,1]<-bain(mod, hypothesis = hypothesis1)%$%fit$BF[1]
    
    BFiu[i,s,2]<-bain(mod, hypothesis = hypothesis2)%$%fit$BF[1]
    
    BFiu[i,s,3]<-bain(mod, hypothesis = hypothesis0)%$%fit$BF[1]
    
    
  }
}


load("Outputs/variation of PMPs/BFiu.RData")
















