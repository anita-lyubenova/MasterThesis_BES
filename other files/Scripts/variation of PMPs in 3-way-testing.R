#3-way-testing: testing 
#  H0: b1 > -MESOI & b1 > MESOI
#  H1 b1 > 0ub, where 0up is 0 upper bound which is between 0 and MESOI
#  H2 - opposite of H1, ie. b1<0lb, wheere 0lb is 0 lower bound which is between 0 and -MESOI

#Simulation conditions:
# fixed n=632,
# b1:b2 ranges from 1:1 to 4.8 to 1 (which makes q range from 0 to 0.438)
#
load("Outputs/variation of PMPs/variation of PMPs in 3-way-testing_workspace.RData")

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
# 1) Hi, Hopposite, and H0 ------------------------------------------
#Hypotheses ------------------------
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
ratio1<-seq(1, 10, by=0.1)[1:35]
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

BF120u<-BFiu

PMP<-array(NA,
           dim=c(iter, length(q), 3),
           dimnames = list(c(1:iter),
                           paste0("q = ",round(q,3)),
                           c("PMP1", "PMP2", "PMP0")
           ))


denoms<-matrix(NA, nrow=nrow(BFiu), ncol = ncol(BFiu))



for(i in 1:nrow(BFiu)){
  
  for(s in 1:ncol(BFiu)){
    
    denoms[i,s]<-sum(BFiu[i,s,])
    
    for(m in 1:3){
      
      PMP[i,s,m]<-BFiu[i,s,m]/denoms[i,s]
      
    }
  }
}

# Plot: variation of PMP0, PMP1, PMP2 -------------------------------------------
PMP %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5, na.rm=TRUE),
            PIlb=quantile(PMP, probs = .025, na.rm=TRUE),
            PIub=quantile(PMP, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(PMP>.5, na.rm=TRUE)/iter
  ) %>% 
  mutate(hypothesis = factor(rep(c("H0", "H1", "H2"), times=25), levels = c("H2", "H0", "H1")))%>% 
  ggplot(aes(x=as.factor(ratio), y=median, color=hypothesis))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()+
  scale_color_manual(values=c("red", "black", "blue"))



#only for H1
df.PMP1<-PMP[,,1] %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5, na.rm=TRUE),
            PIlb=quantile(PMP, probs = .025, na.rm=TRUE),
            PIub=quantile(PMP, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(PMP>.5, na.rm=TRUE)/iter
  )

df.PMP1%>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()


# PMP Histograms -----------------------------------------

par(mfrow = c(1, 1))
par(mfrow = c(4, 5))
#critical for q[8] and q[9] (50% power- chance)
# the PMP1 are clustered either around 0 or around 1
for(s in c(1:20)){
  PMP[,s,1] %>% hist(xlab="PMP",
                     main=paste0("Histogram of PMPs for q = ", round(q[s], 2), ", eta = ", df.PMP1[s,5])
  )
}

#Plot BFiu ------------------------
BFiu %>%
  log %>% 
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BFiu") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BFiu, probs = .5, na.rm=TRUE),
            PIlb=quantile(BFiu, probs = .025, na.rm=TRUE),
            PIub=quantile(BFiu, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(BFiu>.0, na.rm=TRUE)/iter
  ) %>% 
  mutate(hypothesis = factor(rep(c("H0", "H1", "H2"), times=25), levels = c("H2", "H0", "H1")))%>% 
  ggplot(aes(x=as.factor(ratio), y=median, color=hypothesis))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()+
  scale_color_manual(values=c("red", "black", "blue"))+
  labs(title="Variation of BF2u, BF0u and BF1u across effect sizes")

df.BF1u<-log(BFiu[,,1]) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF1u") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF1u, probs = .5, na.rm=TRUE),
            PIlb=quantile(BF1u, probs = .025, na.rm=TRUE),
            PIub=quantile(BF1u, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(BF1u>0, na.rm=TRUE)/iter
  )

df.BF1u%>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()


#BFiu Histograms -----------------------------
par(mfrow = c(1, 1))
par(mfrow = c(4, 5))
for(s in c(1:20)){
  BFiu[,s,1] %>% 
    log() %>% 
    hist(xlab="BF",
                      main=paste0("Histogram of BF1u for q = ", round(q[s], 2), ", eta = ", df.BF1u[s,5])
  )
}

df.BF0u<-log(BFiu[,,3]) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF0u") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF0u, probs = .5, na.rm=TRUE),
            PIlb=quantile(BF0u, probs = .025, na.rm=TRUE),
            PIub=quantile(BF0u, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(BF0u>0, na.rm=TRUE)/iter
  )

par(mfrow = c(1, 1))
par(mfrow = c(4, 5))
for(s in c(1:20)){
  BFiu[,s,3] %>% 
    log() %>% 
    hist(xlab="BF",
         main=paste0("Histogram of BF0u for q = ", round(q[s], 2), ", eta = ", df.BF0u[s,5])
    )
}

#2) Hi, Hc and H0 ------------------------------------
#Test H1: b1-b2>0 vs Hc: b1-b2<0 vs. H0: 0-lb < b1-b2 < 0-ub

#how to determine 0-lb and 0-ub?
#the ES for which H1 and Hc will have sufficient power

#in the current conditions q = 0.124 yields a probability of BF1c > 1 of 0.83 (see BFs.per.q.general.df, in variation of BF in individual studies_workspace.RData)
#thus, I set for q 0-lb = -0.124 and 0-ub = 0.124


#Determine 0-lb adn 0-upb in terms of difference between b1 and b2
#q[5] is 0.124 and it corresponds to ratio_beta = c(1.4,1)
q[5]
models <- c("normal")
r2=.09
pcor<-0.3
n<-632
ratio_beta<-c(ratio1[5],1)
#compute the regr coefficients
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal") #0.2154555 0.1538968
#   => b1-b2 = 0.2154555 - 0.1538968 = 0.0615587 -- this is the effect size in terms of b1-b2 that corresponds to q=0.124

hypothesis0 <- "V2 - 0.0615587 < V1 < V2 + 0.0615587"
hypothesis1<- "V1-V2>0"
hypothesis2<- "V1-V2<0"


q<-q.general.perf[1:35]
r2=.09
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 10, by=0.1)[1:35]
iter<-1000
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
    
    BFiu[i,s,1]<-bain(mod, hypothesis = hypothesis1)%$%fit$BF.u[1]
    
    BFiu[i,s,2]<-bain(mod, hypothesis = hypothesis2)%$%fit$BF.u[1]
    
    BFiu[i,s,3]<-bain(mod, hypothesis = hypothesis0)%$%fit$BF.u[1]
    
  }
  
}

BF.1c0.u <- BFiu
save(BF.1c0.u, file="Outputs/variation of PMPs/BF.1c0.u.RData")

#PMPs

PMP.1c0.u<-array(NA,
           dim=c(iter, length(q), 3),
           dimnames = list(c(1:iter),
                           paste0("q = ",round(q,3)),
                           c("PMP1", "PMP2", "PMP0")
           ))


denoms<-matrix(NA, nrow=nrow(BFiu), ncol = ncol(BFiu))



for(i in 1:nrow(BFiu)){
  
  for(s in 1:ncol(BFiu)){
    
    denoms[i,s]<-sum(BFiu[i,s,])
    
    for(m in 1:3){
      
      PMP.1c0.u[i,s,m]<-BFiu[i,s,m]/denoms[i,s]
      
    }
  }
}


# Plot: variation of PMP0, PMP1, PMP2 -------------------------------------------
PMP.1c0.u %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5, na.rm=TRUE),
            PIlb=quantile(PMP, probs = .025, na.rm=TRUE),
            PIub=quantile(PMP, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(PMP>.5, na.rm=TRUE)/iter
  ) %>% 
  mutate(hypothesis = factor(rep(c("H0", "H1", "H2"), times=25), levels = c("H2", "H0", "H1")))%>% 
  ggplot(aes(x=as.factor(ratio), y=median, color=hypothesis))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()+
  scale_color_manual(values=c("red", "black", "blue"))


#only for H1
df.PMP.1c0.u.H1<-PMP.1c0.u[,,1] %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5, na.rm=TRUE),
            PIlb=quantile(PMP, probs = .025, na.rm=TRUE),
            PIub=quantile(PMP, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(PMP>.5, na.rm=TRUE)/iter
  )

df.PMP.1c0.u.H1%>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()

#PMP histograms ------------------------
par(mfrow = c(4, 5))
#critical for q[8] and q[9] (50% power- chance)
# the PMP1 are clustered either around 0 or around 1
for(s in c(1:20)){
  PMP.1c0.u[,s,1] %>% hist(xlab="PMP",
                     main=paste0("Histogram of PMP1 for q = ", round(q[s], 2), ", eta = ", df.PMP.1c0.u.H1[s,5])
  )
}



#BFiu histograms -----------------------------

df.BF.1c0.u.H1<-log(BF.1c0.u[,,1]) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF1u") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF1u, probs = .5, na.rm=TRUE),
            PIlb=quantile(BF1u, probs = .025, na.rm=TRUE),
            PIub=quantile(BF1u, probs = .975, na.rm=TRUE),
            P.BF.larger.than1=sum(BF1u>0, na.rm=TRUE)/iter
  )

par(mfrow = c(1, 1))
par(mfrow = c(4, 5))
for(s in c(1:20)){
  BF.1c0.u[,s,1] %>% 
    log() %>% 
    hist(xlab="BF",
         main=paste0("Histogram of BF1u for q = ", round(q[s], 2), ", eta = ", df.BF.1c0.u.H1[s,5])
    )
}
