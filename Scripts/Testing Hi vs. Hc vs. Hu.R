# Background: to ensure the interpretability of the aggregate support from BES (BF or PMP),
# it is necessary to assume that a common true hypothesis for all studies is included in the hypothesis set
# Then, the hypothesis with the highest PMP is the most parsimonious common true hypothesis.

# This assumption is automatically satisfied if the unnconstrained hypothesis is included

# If including Hu is sufficient to ensure interpretability, then  Hu must get the highest PMP in
# situations when Hu is the only common true hypothesis is, e.g.
#       -when testing Hi vs Hc, while the truth is on the boundary
#       -testign Hi vs Hc and for some studies Hi is true and for other Hc is true



#Research question 1:
# When testing Hi vs Hc vs Hu while the truth is on the boundary, how often is the PMP_u the highest?


#Research question 2:
# When testing Hi vs Hc vs Hu while for some studies Hi is true while for other Hc is true,
# how often is the PMP_u the highest?

load("Outputs/exploration/q.general.perf.RData")
q<-q.general.perf

#Sim RQ 1 -------------------------------------

r2=.13
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
n<-c(100,200, 300, 400,500,600,700,800,900,1000)
iter<-3
hypothesis<-"V1>V2"
models <- c("normal")
complement<-TRUE

#THe true value is on the boundary
ratio_beta<-c(1,1)

BFiucu<-array(data = NA, dim=c(iter, length(n), 3), dimnames = list(c(1:iter),
                                                                         paste0("n = ", n),
                                                                         c("BFiu", "BFcu", "BFu")
))

BFiucu[,,3]<-1

for(s in 1:length(n)){
  
  print(paste("Condition:", s))
 
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFiu
    
    
    BFiucu[i,s,1:2]<-gen_dat(r2=r2,
                             betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                             rho=cormat(pcor, length(ratio_beta)),
                             n=n[s],
                             "normal")%$%
      lm(Y ~ V1 + V2) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,3)]
    
  }
}

denoms<-matrix(NA, nrow = iter, ncol = length(n))
for(s in 1:length(n)){
  for(i in 1:iter){
    denoms[i,s] <- sum(BFiucu[i,s,])
  }
}
PMP<-BFiucu/3

aggrPMP<-matrix(NA, nrow = 3, ncol = length(n))
for(s in 1:length(n)){
  for (j in 1:3) #hypothesis loop
  aggrPMP[j,s] <- prod(PMP[,s,j])
}




