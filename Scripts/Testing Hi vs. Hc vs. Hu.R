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
iter<-200
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



BFiucu[1:5,,]

#calculate aggregate BFs per hypothesis
aggrBF<-BFiucu

for(s in 1:length(n)){
  
  for (j in 1:3) {#hypothesis loop
    
    for(i in 1:iter){
      
      aggrBF[i,s,j]<- prod(BFiucu[1:i,s,j]) 
      
    }#end iterations loop i

  } #end hypothesis loop j
} # end sample size loop s


aggrBF[1:5,,]
PMP<-aggrBF

dimnames(PMP)[[3]]<-c("PMP_i", "PMP_c", "PMP_u")

for(s in 1:length(n)){
  
  for (j in 1:3) {#hypothesis loop
    
    for(i in 1:iter){
      
      PMP[i,s,j]<- aggrBF[i,s,j]/sum(aggrBF[i,s,]) 
      
    }#end iterations loop i
    
  } #end hypothesis loop j
} # end sample size loop s

PMP[1:10,,]


PMP[1:50,6,] %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = c("PMP_i",  "PMP_c" ,  "PMP_u"),
               names_to = "Hypothesis",
               values_to = "aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
         ) %>% 
  ggplot(aes(x=t, y=aggrPMP, group=Hypothesis, color=Hypothesis))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  labs(title = "Change of the aggregate value for each hypothesis when adding t number of studies (from a single iteration)",
       x="Size of the study set T",
       y="Aggregate PMP")
  

# Sim RQ2 -----------------------

#Research question 2:
# When testing Hi vs Hc vs Hu while for some studies Hi is true while for other Hc is true,
# how often is the PMP_u the highest?

## RQ2.1: ES.Hi = ES.Hc --------------
r2=.13
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
n<-c(100,200, 300, 400,500,600,700,800,900,1000)
iter<-1000
studies<-40
hypothesis<-"V1=V2; V1>V2"
models <- c("normal")
complement<-TRUE

#THe true value is on the boundary
ratio_beta.Hi<-c(2,1)
ratio_beta.Hc<-c(1,2)

BFs<-array(data = NA,
              dim=c(studies,
                    length(n),
                    4,
                    iter),
              dimnames = list(c(1:studies),
                              paste0("n = ", n),
                              c("BF0u", "BFiu", "BFcu", "BFu"),
                              c(1:iter)
))


BFs[,,4,]<-1
s<-5
# loop to fill in BFiucu

for(i in 1:iter){
  for(s in 1:length(n)){
    
    
    for(t in 1:studies){
      
      print(paste("Iteration i:",i, "Condition s:", s, "Study t:", t))
     
      #have ratio_beta conform with Hi if the study is odd numbered and with Hi if it is even
      #(that is every second study comes from Hi and the rest com from Hc)
      if(t %% 2 == 0){
        ratio_beta<-ratio_beta.Hi
      }else{
        ratio_beta<-ratio_beta.Hc
      }
      
      #obtain BFiu
      BFs[t,s,1:3,i]<-gen_dat(r2=r2,
                              betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                              rho=cormat(pcor, length(ratio_beta)),
                              n=n[s],
                              "normal")%$%
        lm(Y ~ V1 + V2) %>%
        bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
      
    } #end studies loop t
  }# end sample size loop s
} #end iterations loop i

BF.RQ2.1<-BFs
save(file = "Outputs/Testing Hi vs Hc vs Hu/workspace_RQ2.1.RData")
## RQ2.2 ES.Hi > ES.Hc-------------------------
r2=.13
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
n<-c(100,200, 300, 400,500,600,700,800,900,1000)
iter<-1000
studies<-40
hypothesis<-"V1=V2; V1>V2"
models <- c("normal")
complement<-TRUE

#THe true value is on the boundary
ratio_beta.Hi<-c(3,1)
ratio_beta.Hc<-c(1,1.5)

BF.RQ2.2<-array(data = NA,
           dim=c(studies,
                 length(n),
                 4,
                 iter),
           dimnames = list(c(1:studies),
                           paste0("n = ", n),
                           c("BF0u", "BFiu", "BFcu", "BFu"),
                           c(1:iter)
           ))


BF.RQ2.2[,,4,]<-1
# loop to fill in BFiucu

for(i in 1:iter){
  for(s in 1:length(n)){
    
    
    for(t in 1:studies){
      
      print(paste("Iteration i:",i, "Condition s:", s, "Study t:", t))
      
      #have ratio_beta conform with Hi if the study is odd numbered and with Hi if it is even
      #(that is every second study comes from Hi and the rest com from Hc)
      if(t %% 2 == 0){
        ratio_beta<-ratio_beta.Hi
      }else{
        ratio_beta<-ratio_beta.Hc
      }
      
      #obtain BFiu
      BF.RQ2.2[t,s,1:3,i]<-gen_dat(r2=r2,
                              betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                              rho=cormat(pcor, length(ratio_beta)),
                              n=n[s],
                              "normal")%$%
        lm(Y ~ V1 + V2) %>%
        bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
      
    } #end studies loop t
  }# end sample size loop s
} #end iterations loop i

save(file = "Outputs/Testing Hi vs Hc vs Hu/workspace_RQ2.RData")