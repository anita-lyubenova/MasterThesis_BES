
# Range of BFic given effect size q ------------------------------------

# Theses simulations (and the pseudo-forest-plots) show that:
# -if the study is underpowered => conclusions from the BF are largely unreliable (inconsistent conclusions across iterations)
# -If the study has enough power => the conclusions from the BF are more reliable (consistent across iterations),
#  however the BF can take on very different values under the same conditions (same power, ES and N)

#n=632 -------------------------------------------------------------------------
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
  
  # ###obtain ES q ---------------------
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
    
    #obtain BFic ---------------------
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
BFic.general.perf.n200<-BFic

save(BFic.general.perf.n200, file="Outputs/exploration/BFic.general.perf.n200.RData")


### Plots ---------------------------------------
#Median Bf per effect size q with  an interval containing 95% of the BFs
BFs.per.q.general<-log(BFic.general.perf) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>1)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()
BFs.per.q.general %>% ggplotly()

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


BFs.per.q.general.n200<-log(BFic.general.perf.n200) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
  mutate(ratio=as.factor(ratio)) %>% 
  group_by(ratio) %>% 
  summarise(median=quantile(BF, probs = .5),
            PIlb=quantile(BF, probs = .025),
            PIub=quantile(BF, probs = .975),
            P.BF.larger.than1=sum(BF>1)/iter
  ) %>% 
  ggplot(aes(x=as.factor(ratio), y=median))+
  geom_point()+
  geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
  #scale_y_continuous(trans = "pseudo_log")+
  coord_flip()
BFs.per.q.general.n200 %>% ggplotly()


# 
#fine grained -------------------
# r2=.09
# pcor<-0.3
# n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
# ratio1<-seq(1, 2, by=0.01)
# iter<-10000
# hypothesis<-"V1>V2"
# models <- c("normal")
# complement<-TRUE
# 
# q<-c()
# BFic<-matrix(NA, nrow = iter, ncol = length(ratio1))
# colnames(BFic)<-paste0(ratio1, ":1")
# 
# for(s in 1:length(ratio1)){
#   ratio_beta<-c(ratio1,1)
#   
#   
# ###obtain ES q 
#       part.cor<-gen_dat(r2=r2,
#                         betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
#                         rho=cormat(pcor, length(ratio_beta)),
#                         n=1000000,
#                         "normal")%$%
#         lm(Y ~ V1 + V2) %>%
#         summ(part.corr=TRUE)%$%coeftable[2:3,5]
#       
#       
#       z1<-log((1+part.cor[1])/(1-part.cor[1]))
#       z2<-log((1+part.cor[2])/(1-part.cor[2]))
#       
#       q[s]=z1-z2
#       
#       for(i in 1:iter){
#         
#         print(paste("Condition s:", s, ", Iteration i:", i))
#         
#         #obtain BFic 
#         BF<-gen_dat(r2=r2, 
#                     betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
#                     rho=cormat(pcor, length(ratio_beta)),
#                     n=n,
#                     "normal")%$%
#           lm(Y ~ V1 + V2) %>%
#           BF(hypothesis = hypothesis, complement = complement) %$%
#           BFtable_confirmatory %>% as.data.frame()%$% BF
#         
#         c[i,s]<-BF[1]/BF[2]
#         
#       }
# }
# 
# names(q)<-paste0(ratio1, ":1")
# colnames(BFic)<-paste0("q = ",round(q,3))
# 
# BFic.finegrained.perf<-BFic
# q.finegrained.perf<-q
# 
# save(BFic.finegrained.perf,
#      q.finegrained.perf, file="")
# 
# 
# BFs.per.q.finegrained<-BFic %>%
#   as.data.frame() %>%
#   pivot_longer(cols = everything(), names_to = "ratio", values_to = "BF") %>%
#   mutate(ratio=as.factor(ratio)) %>% 
#   group_by(ratio) %>% 
#   summarise(median=quantile(BF, probs = .5),
#             PIlb=quantile(BF, probs = .025),
#             PIub=quantile(BF, probs = .975),
#             P.BF.larger.than1=sum(BF>1)/iter
#   ) %>% 
#   ggplot(aes(x=as.factor(ratio), y=median))+
#   geom_point()+
#   geom_errorbar(aes(ymin=PIlb, ymax=PIub))+
#   scale_y_continuous(trans = "pseudo_log")+
#   coord_flip()
# 
# 
# 
# 

#Range of BFic fir fixed effect size and varying n ------------------------------------
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
