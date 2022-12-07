# Background: to ensure the interpretability of the aggregate support from BES (BF or PMP),
# it is necessary to assume that a common true hypothesis for all studies is included in the hypothesis set
# Then, the hypothesis with the highest PMP is the most parsimonious common true hypothesis.

# This assumption is automatically satisfied if the unnconstrained hypothesis is included

# If including Hu is sufficient to ensure interpretability, then  Hu must get the highest PMP in
# situations when Hu is the only common true hypothesis is, e.g.
#       -when testing Hi vs Hc, while the truth is on the boundary
#       -testign Hi vs Hc and for some studies Hi is true and for other Hc is true


#Research Questions --------------------------------
#____________________________________________________________________________________________________________________________________
#Research question 1
#Conditions:
#   -testing Hi vs Hc vs Hu 
#   -for every second study Hi is true while for the rest Hc is true (that is, Hu is the only common true hypothesis)
#   -the effect size for Hi and Hc is the same (ratio_beta for Hi 3:2:1 vs Hc: 1:2:3 )

#Question:
#   -how often is the aggregate PMP_u the highest?

#______________________________________________________________________________________________________________
#Research question 2: add H0
#When testing H0 vs Hi vs Hc vs Hu while for some studies Hi is true while for other Hc is true (ie. Hu is the only common true hypothesis)
# AND the effect size for Hi and Hc is the same ,
# how often is the PMP_u the highest?

#Conditions:
#   -testing H0 Hi vs Hc vs Hu 
#   -for every second study Hi is true while for the rest Hc is true (that is, Hu is the only common true hypothesis)
#   -the effect size for Hi and Hc is the same (ratio_beta for Hi 3:2:1 vs Hc: 1:2:3 )

#Question:
#   -how often is the aggregate PMP_u the highest?
#______________________________________________________________________________________________________________


#Dertermines ample sizes n ------------------------------------------------

r2=.13
pcor<-0.3
n<-c(100,200,300,550)
n<-600
iter<-10000
studies<-40
hypothesis<-"V1>V2>V3"
models <- c("normal")
complement<-TRUE

i<-1
ratio_beta.Hi<-c(3,2,1)
ratio_beta.Hc<-c(1,2,3)

BF.Hi.power.accept<-matrix(NA, nrow = iter, ncol = length(n))
BF.Hi.power.reject<-matrix(NA, nrow = iter, ncol = length(n))
for(i in 1:iter){
  for(s in 1:length(n)){
    print(paste("Iteration i:",i, "Sample size s:", s))
    BF.Hi.power.accept[i,s]<-gen_dat(r2=r2,
                            betas=coefs(r2, ratio_beta.Hi, cormat(pcor, length(ratio_beta.Hi)), "normal"),
                            rho=cormat(pcor, length(ratio_beta.Hi)),
                            n=n[s],
                            "normal")%$%
      lm(Y ~ V1 + V2 + V3) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[1]
    
    BF.Hi.power.reject[i,s]<-gen_dat(r2=r2,
                            betas=coefs(r2, ratio_beta.Hc, cormat(pcor, length(ratio_beta.Hc)), "normal"),
                            rho=cormat(pcor, length(ratio_beta.Hc)),
                            n=n[s],
                            "normal")%$%
      lm(Y ~ V1 + V2 + V3) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[1]
    
  }# end sample size loop
  
} #end iterations i loop

#save(BF.Hi.power.accept, BF.Hi.power.reject, file="Outputs/Research Report/BF.Hi.power.RData")
colnames(BF.Hi.power.accept)<-n
colnames(BF.Hi.power.reject)<-n

apply(BF.Hi.power.accept, 2, function(x){
  sum(x>1)/iter
})
apply(BF.Hi.power.reject, 2, function(x){
  sum(x<1)/iter
})
Hi.power<-rbind(apply(BF.Hi.power.accept, 2, function(x){
                        sum(x>1)/iter
                      }),
                apply(BF.Hi.power.reject, 2, function(x){
                        sum(x<1)/iter
}))
rownames(Hi.power)<-c("accept Hi when TRUE", "reject Hi when FALSE")#
Hi.power
## Data simulation 1: ES_Hi = ES_Hc -----------
#Conditions: 
# results in BF[studies, hypotheses, ratio H1:Hc, iter, sample size n]

r2=.13
pcor<-0.3
n<-c(100,350,600,900)
iter<-1000
studies<-40
hypothesis<-"V1=V2=V3; V1>V2>V3"
models <- c("normal")
complement<-TRUE

ratio_beta.Hi<-c(3,2,1)
ratio_beta.Hc<-c(1,2,3)

#determines whether Hi:Hc = 1:1 or 3:1, respectively
ratio_HiHc<-c(2,4)

BF<-array(data = NA,
                dim=c(studies, 
                      4, # hypotheses
                      2, #1:1 or 3:1 ratio of studies coming from Hi and H
                      iter,
                      length(n) #different sample sizes
                      ),
                dimnames = list(c(1:studies),
                                c("BF0u", "BFiu", "BFcu", "BFu"),
                                c("Hi:Hc=1:1","Hi:Hc=3:1"),
                                c(1:iter),
                                paste0("n = ", n)
                ))


BF[,4,,,]<-1
i<-1
s<-2
r<-1
t<-1

set.seed(123)
# loop to fill in BFiucu
for(i in 1:iter){
  
  for(s in 1:length(n)){
    
    for(r in 1:length(ratio_HiHc)){
      
      for(t in 1:studies){
      
      
        print(paste("Iteration i:",i, "Condition s:", s, "Study t:", t, "Ratio r:",r))
        
        #have ratio_beta conform with Hc or with Hi
        #Hi:Hc study ratio will be either 1:1 (every 2nd study comes from Hc)
        #or 1:3, every fourth study comes from Hc)
        if(t %% ratio_HiHc[r] == 0){ #2nd or 4th study => Hc
          ratio_beta<-ratio_beta.Hc
        }else{                       #else Hi
          ratio_beta<-ratio_beta.Hi 
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
BF.v3<-BF
save(BF.v3,file="Outputs/Research Report/BF.v3.RData")
#save.image(file="Outputs/Research Report/workspace.RData")


### Individual study power ---------------------
n.hyp<-2
r<-1

h<-2
s<-1


power.table.reject<-array(NA,dim=c(studies,n.hyp,length(n)),
                   dimnames=list(1:studies,
                                 c("BFiu.reject", "BFcu.reject"),
                                 paste0("n = ", n)
                                 
                   )
)

power.table.accept<-array(NA,dim=c(studies,n.hyp,length(n)),
                          dimnames=list(1:studies,
                                        c("BFiu.accept", "BFcu.accept"),
                                        paste0("n = ", n)
                                        
                          )
)

for(s in 1:length(n)){
  for(t in 1:studies){
    for(h in 1:2){
      #r=1 (H1:Hc=1:1)
     power.table.reject[t,h,s]<- sum(BF[t,h+1,r,,s]<1)/iter
     power.table.accept[t,h,s]<- sum(BF[t,h+1,r,,s]>1)/iter
    }
  }
}

#Studies for which Hc is true
Hc.true.accept<-power.table.accept[seq(2,40, by=2),,]
Hc.true.reject<-power.table.reject[seq(2,40, by=2),,]
colMeans(Hc.true.accept)
colMeans(Hc.true.reject)

#Studies for which Hi is true
Hi.true.accept<-power.table.accept[-seq(2,40, by=2),,]
Hi.true.reject<-power.table.reject[-seq(2,40, by=2),,]
Hi.true.power<-rbind(colMeans(Hi.true.accept),colMeans(Hi.true.reject))[,]

#power for Hc:
# accept Hc when Hc = TRUE: 
# AND
# reject Hc when Hi=TRUE
Hc.power<-rbind(colMeans(Hc.true.accept)[2,], colMeans(Hi.true.reject)[2,])
rownames(Hc.power)<-c("BFcu.accept", "BFcu.reject")
Hc.power

#power for Hi: accept Hi when Hi = TRUE and reject Hi when Hc=TRUE
Hi.power.obs<-rbind(colMeans(Hi.true.accept)[1,], colMeans(Hc.true.reject)[1,])
rownames(Hi.power.obs)<-c("BFiu.accept", "BFiu.reject")
Hi.power.obs

#aggregate PMPs---------------------
#for now continue with BF.v2
BF<-BF.v2
dimnames(BF)
## with Hu -------------------------
#BF[studies, hypotheses, ratioHi:Hc, iter, n]
#ratio Hi:Hc = 1:1

n.hyp<-3
# Hi vs. Hc vs. Hu
BF.noH0<-BF[,2:4,,,]
aggrPMP<-BF.noH0

s<-2
i<-1
t<-1
h<-1
r<-1


for(s in 1:length(n)){  #for each sample size s
  for(i in 1:iter){       # for each iteration t
    for(t in 1:studies){    # for each study t
      for(h in 1:n.hyp){  #for each hypothesis h
        for(r in 1:2){ #for each ratio Hi:Hc
          #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
          aggrPMP[t,h,r,i,s]<-prod(BF.noH0[1:t,h,r,i,s])/sum(apply(matrix(BF.noH0[1:t,,r,i,s],nrow = t,ncol = n.hyp),2,prod))
          
        }
      } 
    }
  }
}

dimnames(aggrPMP)[[2]]<-c("PMP_i","PMP_c","PMP_u")

#aggregated PMPs for simulation 1 (Hi:Hc=1:1); Hi vs. Hc vs Hu
aggrPMP1<-aggrPMP[,,1,,]
#aggregated PMPs for simulation 1 (Hi:Hc=3:1); Hi vs. Hc vs Hu
aggrPMP2<-aggrPMP[,,2,,]

## without Hu ------------------------------

n.hyp<-2
#consider only Hi vs. Hc 
BF.noHu<-BF[,2:3,,,]
aggrPMP.noHu<-BF.noHu

for(s in 1:length(n)){  #for each sample size s
  for(i in 1:iter){       # for each iteration t
    for(t in 1:studies){    # for each study t
      for(h in 1:n.hyp){  #for each hypothesis h
        for(r in 1:2){ #for each ratio Hi:Hc
          #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
          aggrPMP.noHu[t,h,r,i,s]<-prod(BF.noHu[1:t,h,r,i,s])/sum(apply(matrix(BF.noHu[1:t,,r,i,s],nrow = t,ncol = n.hyp),2,prod))
          
        }
      } 
    }
  }
}
dimnames(aggrPMP.noHu)[[2]]<-c("PMP_i","PMP_c")

aggrPMP1.noHu<-aggrPMP.noHu[,,1,,]
aggrPMP2.noHu<-aggrPMP.noHu[,,2,,]


#Simulation 1 ---------------------

#Conditions:
#   -testing Hi vs Hc  AND  Hi vs Hc vs Hu 
#   -for every second study Hi is true while for the rest Hc is true (that is, Hu is the only common true hypothesis)
#   -the effect size for Hi and Hc is the same (ratio_beta for Hi 3:2:1 vs Hc: 1:2:3 )

#Question:
#   -how often is the aggregate PMP_u the highest?


## Median PMP plot: with Hu, N=600 ----------------------
#aggrPMP1[studies, hypothesis, iter, n]
dimnames(aggrPMP1)
# set desired dodge width
pd <- position_dodge(width = 0.4)

s<-2 #N=350
#the median aggregate value
medians<-apply(aggrPMP1[,,,s],c(1,2),median)%>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = c("PMP_i",  "PMP_c" ,  "PMP_u"),
               names_to = "Hypothesis",
               values_to = "median_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )


#lower bounds of the aggregate values per hypothesis h per study number t
lbs<-apply(aggrPMP1[,,,s],c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1)[[2]],
               names_to = "Hypothesis",
               values_to = "lb_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

ubs<-apply(aggrPMP1[,,,s],c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1)[[2]],
               names_to = "Hypothesis",
               values_to = "ub_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

median.PMP1.df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
  left_join(., ubs, by=c("t", "Hypothesis"))

library(plotly)
library(RColorBrewer)
col.H1<-"#7fc97f"
col.Hc<-"#fdc086"
col.Hu<-"#beaed4"
median.PMP1.plot<-median.PMP1.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP), position = pd)+
  theme_minimal()+
  labs(#title = "",
       subtitle = "H1 vs. Hc vs. Hu",
       x="Number of aggregated studies",
       y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.Hc), times=20)),
        legend.title = element_text(size = 7),
        legend.title.align=0.5,
         plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
        # legend.text = element_text(size = 10),
        )+
  guides(colour = guide_legend(nrow = 1))+
  annotate("text", x = 43, y = 0.5, label = "H1:Hc = 1:1",angle = 270)+
  coord_cartesian(xlim = c(0, 40), clip = "off")+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")
  
 # scale_colour_viridis_d()
  
median.PMP1.plot

ggplotly(median.PMP1.plot)


## Median PMP plot: no Hu, N=350 -------------------------

s<-2 #N=350, power = 0.91

#the median aggregate value
medians<-apply(aggrPMP1.noHu[,,,s],c(1,2),median)%>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "median_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )


#lower bounds of the aggregate values per hypothesis h per study number t
lbs<-apply(aggrPMP1.noHu[,,,s],c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "lb_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

ubs<-apply(aggrPMP1.noHu[,,,s],c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "ub_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

median.PMP1.noHu.df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
  left_join(., ubs, by=c("t", "Hypothesis"))

library(plotly)
median.PMP1.noHu.plot<-median.PMP1.noHu.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
       subtitle = "Hi vs. Hc",
       x="Number of aggregated studies",
       y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.Hc), times=20)),
        legend.title = element_text(size = 7),
         plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

median.PMP1.noHu.plot

ggplotly(median.PMP1.plot)


## Median PMP plot: with Hu, N=100 ----------------------
#aggrPMP1[studies, hypothesis, iter, n]
dimnames(aggrPMP1)

s<-1 #N=600
#the median aggregate value
medians<-apply(aggrPMP1[,,,s],c(1,2),median)%>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = c("PMP_i",  "PMP_c" ,  "PMP_u"),
               names_to = "Hypothesis",
               values_to = "median_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )


#lower bounds of the aggregate values per hypothesis h per study number t
lbs<-apply(aggrPMP1[,,,s],c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1)[[2]],
               names_to = "Hypothesis",
               values_to = "lb_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

ubs<-apply(aggrPMP1[,,,s],c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1)[[2]],
               names_to = "Hypothesis",
               values_to = "ub_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

median.PMP1.n100.df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
  left_join(., ubs, by=c("t", "Hypothesis"))

library(plotly)
library(RColorBrewer)
col.H1<-"#7fc97f"
col.Hc<-"#fdc086"
col.Hu<-"#beaed4"
median.PMP1.n100.plot<-median.PMP1.n100.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u"))))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP))+
  theme_minimal()+
  labs(#title = "",
    #subtitle = "",
    x="Number of aggregated studies",
    y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.Hc), times=20)),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

# scale_colour_viridis_d()

median.PMP1.n100.plot

ggplotly(median.PMP1.plot)


## Median PMP plot: no Hu, N=100 -------------------------

s<-1 #N=600

#the median aggregate value
medians<-apply(aggrPMP1.noHu[,,,s],c(1,2),median)%>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "median_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )


#lower bounds of the aggregate values per hypothesis h per study number t
lbs<-apply(aggrPMP1.noHu[,,,s],c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "lb_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

ubs<-apply(aggrPMP1.noHu[,,,s],c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP1.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "ub_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

median.PMP1.n100.noHu.df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
  left_join(., ubs, by=c("t", "Hypothesis"))

library(plotly)
median.PMP1.n100.noHu.plot<-median.PMP1.n100.noHu.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c"))))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP))+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
    #subtitle = "Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs",
    x="Number of aggregated studies",
    y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.Hc), times=20)),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

median.PMP1.n100.noHu.plot

## ggarrange(median PMP plots)----------------------------------
library(ggpubr)

a<-ggarrange(median.PMP1.noHu.plot + rremove("xlab"),
          median.PMP1.plot,
          labels = c("a)", "b)"),
          legend="right",
          nrow=1,
          legend.grob = get_legend(median.PMP1.plot)
          ) 
a
median.PMP1.n100.noHu.plot
a<-ggarrange(median.PMP1.noHu.plot,
             median.PMP1.plot,
             median.PMP1.n100.noHu.plot,
             median.PMP1.n100.plot,
             legend="bottom",
             nrow=2,
             ncol = 2,
             legend.grob = get_legend(median.PMP1.plot)
) 
a
ggsave("Outputs/Research Report/Fig1.png", plot = a, width = 5, height = 3, units = "in", dpi = 300, bg="white")
# %>% 
#   annotate_figure(top = text_grob("Change of the aggregate PMPs for each hypothesis with increasing number of aggregared studies
#                                   <small>Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs</small>
#                                   ", size = 14))


## Support Hu------------------------------------------
support.Hu<-array(NA,
                  dim=c(iter,
                        studies,
                        length(n)
                  ),
                  dimnames = list(
                    1:iter,
                    1:studies,
                    paste0("n = ",n)
                  )
)
for(i in 1:iter){
  for(t in 1:studies){
    for(s in 1:length(n))
      support.Hu[i,t,s]<-ifelse(max(aggrPMP1[t,,i,s])==aggrPMP1[t,,i,s][3], 1,0)
    
  }
}
support.Hu
#ANSWER to the Research question: proporiton of times Hu received most support 
colSums(support.Hu[,,1])/iter
colSums(support.Hu[,,3])/iter

Fig2a.df<-cbind(colSums(support.Hu[,,1])/iter,colSums(support.Hu[,,3])/iter)
colnames(Fig2a.df)<-dimnames(support.Hu[,,])[[3]][c(1,3)]


Fig2a.df %>%
  as.data.frame() %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = colnames(Fig2a.df),
               names_to = "n",
               values_to = "prop.highest.PMP_u"
               ) %>% 
  mutate(n=as.factor(n),
         t=factor(t, levels = unique(sort(as.numeric(t))))
         ) %>% 
  ggplot(aes(x=t, y=prop.highest.PMP_u, group=n, color=n))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  labs(x="Size of the study set ",
       y="Proportion of times",
       title="Proportion of times the PMP of Hu was the highest")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.Hc), times=20)),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_discrete(name = "Sample size")
#Simulation 2 ---------------------

#Conditions:
#   -testing  Hi vs Hc vs Hu 
#  ! -for every 4th study Hc is true while for the rest Hi is true(Hi:Hc = 3:1) (that is, Hu is still 
#      the only common true hypothesis but Hi is more prevalent)
#   -the effect size for Hi and Hc is the same (ratio_beta for Hi 3:2:1 vs Hc: 1:2:3 )

#Question:
#   -how often is the aggregate PMP_u the highest?


## Median PMP plot (with Hu) ----------------------
#aggrPMP2[studies, hypothesis, iter, n]

s<-2 #N=600
#the median aggregate value
medians<-apply(aggrPMP2[,,,s],c(1,2),median)%>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = c("PMP_i",  "PMP_c" ,  "PMP_u"),
               names_to = "Hypothesis",
               values_to = "median_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )


#lower bounds of the aggregate values per hypothesis h per study number t
lbs<-apply(aggrPMP2[,,,s],c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP2)[[2]],
               names_to = "Hypothesis",
               values_to = "lb_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

ubs<-apply(aggrPMP2[,,,s],c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP2)[[2]],
               names_to = "Hypothesis",
               values_to = "ub_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

median.PMP2.df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
  left_join(., ubs, by=c("t", "Hypothesis"))

library(plotly)

median.PMP2.plot<-median.PMP2.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
  theme_minimal()+
  labs(#title = "",
    #subtitle = "",
    x="Number of aggregated studies",
    y="Aggregate PMP")+
  scale_color_discrete(labels=c("H1c", "H1", "Hu"))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.H1,col.H1,col.Hc), times=10)),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  annotate("text", x = 43, y = 0.5, label = "H1:Hc = 1:1",angle = 270)+
  coord_cartesian(xlim = c(0, 40), clip = "off")+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

median.PMP2.plot

ggplotly(median.PMP1.plot)


## Median PMP plot(no Hu) -------------------------

s<-2 #N=350

#the median aggregate value
medians<-apply(aggrPMP2.noHu[,,,s],c(1,2),median)%>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP2.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "median_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )


#lower bounds of the aggregate values per hypothesis h per study number t
lbs<-apply(aggrPMP2.noHu[,,,s],c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP2.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "lb_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

ubs<-apply(aggrPMP2.noHu[,,,s],c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var="t") %>% 
  pivot_longer(cols = dimnames(aggrPMP2.noHu)[[2]],
               names_to = "Hypothesis",
               values_to = "ub_aggrPMP") %>%
  mutate(Hypothesis=as.factor(Hypothesis),
         t=factor(t, levels = unique(t))
  )  

median.PMP2.noHu.df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
  left_join(., ubs, by=c("t", "Hypothesis"))


library(plotly)
median.PMP2.noHu.plot<-median.PMP2.noHu.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
    #subtitle = "Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs",
    x="Number of aggregated studies",
    y="Aggregate PMP")+
  scale_color_discrete(labels=c("H1c", "H1"))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5, colour = rep(c(col.H1,col.H1,col.H1,col.Hc), times=10)),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
        # plot.subtitle = element_text(size = 6),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

median.PMP2.noHu.plot

ggplotly(median.PMP1.plot)

## ggarrange(median PMP plots)----------------------------------
library(ggpubr)

b<-ggarrange(median.PMP2.noHu.plot+ rremove("xlab"),
             median.PMP2.plot,
             labels = c("a)", "b)"),
             legend="right",
             nrow=2
) 
b

b<-ggarrange(median.PMP1.noHu.plot+ rremove("xlab"),
             median.PMP1.plot+ rremove("xylab"),
             median.PMP2.noHu.plot,
             median.PMP2.plot+ rremove("ylab"),
             legend="bottom",
             nrow=2,
             ncol = 2,
             legend.grob = get_legend(median.PMP1.plot)
) 
b

ggsave("Outputs/Research Report/Fig1_v2.png", plot = b, width = 9, height = 4.6, units = "in", dpi = 300, bg="white")

# %>% 
#   annotate_figure(top = text_grob("Change of the aggregate PMPs for each hypothesis with increasing number of aggregared studies
#                                   <small>Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs</small>
#                                   ", size = 14))




## Support Hu ------------------------------------------
support.Hu2<-array(NA,
                  dim=c(iter,
                        studies,
                        length(n)
                  ),
                  dimnames = list(
                    1:iter,
                    1:studies,
                    paste0("n = ",n)
                  )
)
for(i in 1:iter){
  for(t in 1:studies){
    for(s in 1:length(n))
      support.Hu2[i,t,s]<-ifelse(max(aggrPMP2[t,,i,s])==aggrPMP2[t,,i,s][3], 1,0)
    
  }
}
support.Hu
#ANSWER to the Research question: proporiton of times Hu received most support 
colSums(support.Hu2[,,1])/iter
colSums(support.Hu2[,,2])/iter

plot(colSums(support.Hu2[,,2])/iter, type="l" )

