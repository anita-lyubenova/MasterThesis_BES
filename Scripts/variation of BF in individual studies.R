
# RANGE OF BFic -----------------------------------

# Theses simulations (and the pseudo-forest-plots) show that:
# -if the study is underpowered => conclusions from the BF are largely unreliable (inconsistent conclusions across iterations)
# -If the study has enough power => the conclusions from the BF are more reliable (consistent across iterations),
#  however the BF can take on very different values under the same conditions (same power, ES and N)

#Load the workspace necessary for the plots
load("Outputs/exploration/variation of BF in individual studies_workspace.RData")
q<-q.general.perf
#x=BFic, y=q, n=632(const) -------------------------------------------------------------------------
r2=.09
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 10, by=0.1)
iter<-10000
hypothesis<-"V1>V2"
models <- c("normal")
complement<-TRUE

#q<-c()
BFic<-matrix(NA, nrow = iter, ncol = length(ratio1))


for(s in 1:length(ratio1)){
  
  print(paste("Condition:", s))
  
  ratio_beta<-c(ratio1[s],1)
  
  # ###obtain ES q
  # part.cor<-gen_dat(r2=r2,
  #                   betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
  #                   rho=cormat(pcor, length(ratio_beta)),
  #                   n=1000000,
  #                   "normal")%$%
  #   lm(Y ~ V1 + V2) %>%
  #   summ(part.corr=TRUE)%$%coeftable[2:3,5]
  # 
  # 
  # z1<-log((1+part.cor[1])/(1-part.cor[1]))
  # z2<-log((1+part.cor[2])/(1-part.cor[2]))
  # 
  # q[s]=z1-z2
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFic 
    BF<-gen_dat(r2=r2,
                betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                rho=cormat(pcor, length(ratio_beta)),
                n=n,
                "normal")%$%
      lm(Y ~ V1 + V2) %>%
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory %>% as.data.frame()%$% BF
    
    BFic[i,s]<-BF[1]/BF[2]
    
  }
}
names(q)<-paste0(ratio1, ":1")
colnames(BFic)<-paste0("q = ",round(q,3))
BFic.general.perf<-BFic
q.general.perf<-q

save(q.general.perf, file="Outputs/exploration/q.general.perf.RData")
save(BFic.general.perf, file="Outputs/exploration/BFic.general.perf.RData")

#transform BFic to PMPs
PMPic.general.perf<-BFic.general.perf/(BFic.general.perf+1)



## Plots -------------------
### BFic --------------------
#Median BFic per effect size q with  an interval containing 95% of the BFs
BFs.per.q.general.df<-log(BFic.general.perf) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  )


BFs.per.q.general.df%>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()

###PMPs ----------------
PMPs.per.q.general<-PMPic.general.perf %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5, na.rm=TRUE),
            PIlb=quantile(PMP, probs = .05, na.rm=TRUE),
            PIub=max(PMP, na.rm=TRUE),
            P.BF.larger.than1=sum(PMP>.5, na.rm=TRUE)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()


PMPs.per.q.general


#x=BFic, y=q, n=100(const) ---------------------------------------
# the same as above but with n=100
#Question: is the "unrealiability" (i.e. all over hte place) of the BFic wheb truth is on the boundary arising from low power?
# If yes => BFic will be unreliable for way more effect sizes (for which n=100 is not enough)
# Implication: The problem is not really in the true value being on the boundary but in that the true value is
# smaller than can be detected with the given sample size
n<-100

BFic<-matrix(NA, nrow = iter, ncol = length(ratio1))

for(s in 1:length(ratio1)){
  
  print(paste("Condition:", s))
  
  ratio_beta<-c(ratio1[s],1)
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFic
    BF<-gen_dat(r2=r2,
                betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                rho=cormat(pcor, length(ratio_beta)),
                n=n,
                "normal")%$%
      lm(Y ~ V1 + V2) %>%
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory %>% as.data.frame()%$% BF
    
    BFic[i,s]<-BF[1]/BF[2]
    
  }
}
names(q)<-paste0(ratio1, ":1")
colnames(BFic)<-paste0("q = ",round(q.general.perf,3))
BFic.general.perf.n100<-BFic

save(BFic.general.perf.n100, file="Outputs/exploration/BFic.general.perf.n100.RData")


### Plot (BFic)--------


BFs.per.q.general.n100<-log(BFic.general.perf.n100) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()
BFs.per.q.general.n100 %>% ggplotly()


#x=BFic, y=N, q=const ------------------------------------
r2=.09
pcor<-0.3
n<-c(100,200, 300, 400,500,600,700,800,900,1000)
ratio_beta<-c(2,1)
iter<-10000
hypothesis<-"V1>V2"
models <- c("normal")
complement<-TRUE

#q<-c()
BFic<-matrix(NA, nrow = iter, ncol = length(n))


for(s in 1:length(n)){

  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFic 
    BF<-gen_dat(r2=r2,
                betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                rho=cormat(pcor, length(ratio_beta)),
                n=n[s],
                "normal")%$%
      lm(Y ~ V1 + V2) %>%
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory %>% as.data.frame()%$% BF
    
    BFic[i,s]<-BF[1]/BF[2]
    
  }
}

colnames(BFic)<-paste0("n.",n)
BFic.same.ES.diff.N<-BFic


##Plot -----
log(BFic.same.ES.diff.N) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "n", values_to = "BF") %>%
  mutate(n=factor(n, levels = colnames(BFic))) %>% 
  group_by(n) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  ) %>% 
  ggplot(aes(x=n, y=median, label=P.BF.larger.than1))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  geom_label(nudge_y = 2)+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()

save(BFic.same.ES.diff.N, file="Outputs/exploration/BFic.same.ES.diff.N.RData")




#RANGE OF BFiu ------------------------------------------------------
# x=BFiu, y=q, n=632(const) ----------




r2=.09
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 10, by=0.1)
iter<-10000
hypothesis<-"V1>V2"
models <- c("normal")
complement<-TRUE

#the q for each ratio is in q.general.perf

BFiu<-matrix(NA, nrow = iter, ncol = length(ratio1))
BFcu<-matrix(NA, nrow = iter, ncol = length(ratio1))

for(s in 1:length(ratio1)){
  
  print(paste("Condition:", s))
  
  ratio_beta<-c(ratio1[s],1)
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFiu
    BF<-gen_dat(r2=r2,
                betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                rho=cormat(pcor, length(ratio_beta)),
                n=n,
                "normal")%$%
      lm(Y ~ V1 + V2) %>%
      BF(hypothesis = hypothesis, complement = TRUE) %$%
      BFtable_confirmatory %>% as.data.frame()%$% BF
    
    BFiu[i,s]<-BF[1]
    BFcu[i,s]<-BF[2]
    
  }
}



colnames(BFiu)<-colnames(BFcu)<-paste0("q = ",round(q,3))

BFiu.n632<-BFiu
BFcu.n632<-BFcu
save.image(file = "Outputs/exploration/variation of BF in individual studies_workspace.RData")
## Plot --------------


#Median BFiu per effect size q with  an interval containing 95% of the BFs
BFiu.per.q.n632<-log(BFiu.n632)%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  scale_y_continuous(breaks = seq(-3,2, by=0.25))+
  coord_flip()
BFiu.per.q.n632 #%>% ggplotly()

BFcu.per.q.n632<-log(BFcu.n632)%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
 # scale_y_continuous(breaks = seq(-3,2, by=0.25))+
  coord_flip()
BFcu.per.q.n632 #%>% ggplotly()


#overlay the plots of the ranges of BFiu and BFcu
df.BFiu<-log(BFiu.n632)%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  ) 

df.BFcu<-log(BFcu.n632)%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>0)/iter
  ) 

#overlayed plot
ggplot(df.BFiu, aes(x=as.factor(ratio), y=median)) + 
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub), size=0.9)+
  geom_point(data=df.BFcu, color="red")+
  geom_errorbar(data=df.BFcu,aes(ymin=PIlb, ymax=PIub), color="red")+
 # scale_y_continuous(breaks = seq(0,1, by=0.10))+
  coord_flip()+
  # geom_hline(yintercept = -0.70, linetype="dotted", # the opposite of BFmax =0.7
  #            color = "blue", size=1.5)+
  geom_hline(yintercept = 0,  # log-BF=0
             color = "green", size=1)+
  labs(title="Variation of log-BFiu (black) and log-BFcu (red) for increasing effect sizes (with fixed n=632)",
       caption = "H1: b1>b2; q=effect size for the increasing ratio b1:b2; points represent the median BF; errorbars represent 95% of the BFs (two-sided)"
       )+
  ylab("log-BF")+
  xlab("Effect size q (reflects b1:b2")

  



## Histograms  BFiu and BFcu -----------------------------
BFiu.n632[,1] %>% 
  log() %>%
  hist(main=paste0("Histogram of log-BFiu for H1: b1>b2 for q = ", round(q[1], digits=2)),
       xlab="log-BF",
       ylab="Frequency")

abline(v = median(log(BFiu.n632[,1])),                     
       col = "blue",
       lwd = 3)

text(x=-7, y=2500,
     paste("Median =", round(median(log(BFiu.n632[,1])), digits=2)),
     col = "blue",
     cex = 1)



BFiu.n632[,2] %>% log() %>% hist()
BFiu.n632[,3] %>% log() %>%hist()
BFiu.n632[,4] %>%log() %>% hist()
BFiu.n632[,5] %>% log() %>%hist()
BFiu.n632[,10] %>% log() %>%  hist()

#A frame with histograms on 3 rows and 5 columns
#each row represents a different power level: 1st = low power, 2=adequate power, 3= very high power
#across columns power also increases but only slightly
par(mfrow = c(1, 1))
par(mfrow = c(3, 5))
for(i in c(1:5,10:14, 60:64)){
  BFiu.n632[,i] %>% 
    log() %>%
    hist(main=paste0("Histogram of log-BFiu for H1: b1>b2 for q = ", round(q[i], digits=3)),
         xlab="log-BFiu",
         ylab="Frequency",
         xlim=c(-8,2))
  
  abline(v = median(log(BFiu.n632[,i])),                     
         col = "blue",
         lwd = 1)
  
  text(x=-0, y=2500,
       paste("Median =", round(median(log(BFiu.n632[,i])), digits=2)),
       col = "blue",
       cex = 1)
  
}


BFcu.n632[,1] %>% log() %>%  hist()
BFcu.n632[,2] %>% log() %>%  hist()
BFcu.n632[,3] %>% log() %>%  hist()
BFcu.n632[,4] %>% log() %>%  hist()
BFcu.n632[,5] %>% log() %>%  hist()

BFcu.n632[,10] %>% log() %>%  hist()
BFcu.n632[,11] %>% log() %>%  hist()
BFcu.n632[,51] %>% log() %>%  hist()
BFcu.n632[,70] %>% log() %>%  hist()
BFcu.n632[,90] %>% log() %>%  hist()

#A frame with histograms on 3 rows and 5 columns
#each row represents a different power level: 1st = low power, 2=adequate power, 3= very high power
#across columns power also increases but only slightly
par(mfrow = c(1, 1))
par(mfrow = c(3, 5))
for(i in c(1:5,10:14, 60:64)){
  BFcu.n632[,i] %>% 
    log() %>%
    hist(main=paste0("Histogram of log-BFcu for H1: b1>b2 for q = -", round(q[i], digits=3)),
         xlab="log-BFcu",
         ylab="Frequency",
         xlim=c(-26, 2
                ))
  
  abline(v = median(log(BFcu.n632[,i])),                     
         col = "blue",
         lwd = 1)
  
  text(x=-5, y=1500,
       paste("Median =", round(median(log(BFcu.n632[,i])), digits=2)),
       col = "blue",
       cex = 1)
  
}


which(names(q)=="4.8:1")
q[1:39]




## PMPs of BFiu, BFcu and BFuu--------------------
#Comparing 3 hypotheses Hi, Hc, and Hu
BFiu.n632
BFcu.n632

BFiucu.n632<-array(data = NA, dim=c(iter, length(q), 3), dimnames = list(c(1:iter),
                                                                         paste0("q = ",round(q,3)),
                                                                         c("PMPi", "PMPc", "PMPu")
                                                                         ))

BFiucu.n632[,,1]<-BFiu.n632
BFiucu.n632[,,2]<-BFcu.n632



PMPiucu.n632<-BFiucu.n632/3
PMPiucu.n632[,,3]<-1 - (PMPiucu.n632[,,1]+PMPiucu.n632[,,2])


#PMPs for Hi
PMPi.per.q.n632<-PMPiucu.n632[,,1]%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5),
            PIlb=quantile(PMP, probs = .025),
            PIub=quantile(PMP, probs = .975),
            P.BF.larger.than1=sum(BF>.50)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  scale_y_continuous(breaks = seq(0,1, by=0.10))+
  coord_flip()

PMPi.per.q.n632

#PMPs for Hc
PMPc.per.q.n632<-PMPiucu.n632[,,2]%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5),
            PIlb=quantile(PMP, probs = .025),
            PIub=quantile(PMP, probs = .975),
            P.BF.larger.than1=sum(BF>.50)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  scale_y_continuous(breaks = seq(0,1, by=0.10))+
  coord_flip()
PMPc.per.q.n632 #%>% ggplotly()

#PMPs for Hu
PMPu.per.q.n632<-PMPiucu.n632[,,3]%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5),
            PIlb=quantile(PMP, probs = .025),
            PIub=quantile(PMP, probs = .975),
            P.BF.larger.than1=sum(BF>.50)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  scale_y_continuous(breaks = seq(0,1, by=0.10))+
  coord_flip()
PMPu.per.q.n632 #%>% ggplotly()


#overlay PMPi and PMPc

df.iu<-PMPiucu.n632[,,1]%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5),
            PIlb=quantile(PMP, probs = .1),
            PIub=quantile(PMP, probs = .90),
            P.PMP.larger.than1=sum(PMP>.50)/iter
  )

df.cu <- PMPiucu.n632[,,2]%>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "PMP") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(PMP, probs = .5),
            PIlb=quantile(PMP, probs = .1),
            PIub=quantile(PMP, probs = .90),
            P.PMP.larger.than1=sum(PMP>.50)/iter
  ) 

#olverlayed plot
ggplot(df.iu, aes(x=as.factor(ratio), y=median)) + 
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub), size=0.9)+
  geom_point(data=df.cu, color="red")+
  geom_errorbar(data=df.cu,aes(ymin=PIlb, ymax=PIub), color="red")+
  scale_y_continuous(breaks = seq(0,1, by=0.10))+
  coord_flip()+
  geom_hline(yintercept = 0.33, color="blue",linetype="dotted", size=1.5)+
  annotate("text", y=c(0.06, 0.36, 0.60), x=70, label=c("PMP-Hc", "PMP-Hu", "PMP-Hi"),
           colour=c("red", "blue", "black"))


#

























