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


## Data simulation 1: ES_Hi = ES_Hc -----------
r2=.13
pcor<-0.3
n<-c(100,1200)
iter<-1000
studies<-40
hypothesis<-"V1=V2=V3; V1>V2>V3"
models <- c("normal")
complement<-TRUE

ratio_beta.Hi<-c(3,2,1)
ratio_beta.Hc<-c(1,2,3)

#determines whetheer Hi:Hc = 1:1 or 3:1, respectively
ratio_HiHc<-c(2,4)

BF<-array(data = NA,
                dim=c(studies, 
                      4, # hypotheses
                      2, #ratio Hi:Hc=1:1 or 3:1
                      iter,
                      length(n) # #1:1 or 3:1 ratio of studies coming from Hi and Hc
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

# loop to fill in BFiucu
for(i in 1:iter){
  
  for(s in 1:length(n)){
    
    for(r in 1:length(ratio_HiHc)){
      
      for(t in 1:studies){
      
      
        print(paste("Iteration i:",i, "Condition s:", s, "Study t:", t, "Ratio r:",r))
        
        #have ratio_beta conform with Hc or with Hi
        #Hi:Hc study ratio will be either 1:1 (every 2nd study comes from Hc)
        #or 1:3, every fourth study comes from Hc)
        if(t %% ratio_HiHc[r] == 0){ 
          ratio_beta<-ratio_beta.Hc
        }else{                       
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

#save.image(file="Outputs/Research Report/workspace.RData")

### Individual study power ---------------------
n.hyp<-3
r<-1

h<-2
s<-1

power.table<-array(NA,dim=c(studies,2,n.hyp,length(n)),
                      dimnames=list(1:studies,
                                    c("supportH", "rejectH"),
                                    c("BF0u", "BFiu", "BFcu"),
                                    paste0("n = ", n)
                                    
                            )
                   )

power.table<-array(NA,dim=c(studies,n.hyp,length(n)),
                   dimnames=list(1:studies,
                                 c("BF0u", "BFiu", "BFcu"),
                                 paste0("n = ", n)
                                 
                   )
)

for(s in 1:length(n)){
  for(t in 1:studies){
    for(h in 1:3){
      #r=1 (H1:Hc=1:1)
     power.table[t,h,s]<- sum(BF[t,h,r,,s]<1)/iter
    }
  }
}

Hc<-power.table[seq(2,40, by=2),,1]
colMeans(Hc)
Hi<-power.table[seq(1,39, by=2),,1]
colMeans(Hi)
## Data Simulation 2: ES_Hi > ES_Hc----------------------------



#aggregate PMPs---------------------

## with Hu -------------------------
#BF[studies, hypotheses, ratioHi:Hc, iter, n]
#ratio Hi:Hc = 1:1
r<-1

n.hyp<-3
# Hi vs. Hc vs. Hu
BF.noH0<-BF[,2:4,,,]
aggrPMP<-BF.noH0

s<-2
i<-1
t<-1
h<-1

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
aggrPMP2<-aggrPMP[,,1,,]

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


## Median PMP plot (with Hu) ----------------------
#aggrPMP1[studies, hypothesis, iter, n]

s<-2 #N=1200
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

median.PMP1.plot<-median.PMP1.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u"))))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP))+
  theme_minimal()+
  labs(#title = "",
       #subtitle = "",
       x="Size of the study set T",
       y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
        )+
  scale_colour_manual(values = c("#21908CFF", "#FDE725FF", "#440154FF" ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")
  
 # scale_colour_viridis_d()
  
median.PMP1.plot

ggplotly(median.PMP1.plot)


## Median PMP plot(no Hu) -------------------------

s<-2 #N=1200

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
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP))+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
       #subtitle = "Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs",
       x="Size of the study set T",
       y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 6),
        # legend.text = element_text(size = 10),
        )+
  scale_colour_manual(values = c("#21908CFF", "#FDE725FF" ),labels=c("H1", "H1c"),name = "Hypothesis")

median.PMP1.noHu.plot

ggplotly(median.PMP1.plot)

## ggarrange(median PMP plots)----------------------------------
library(ggpubr)

a<-ggarrange(median.PMP1.noHu.plot+ rremove("xlab"),
          median.PMP1.plot,
          labels = c("a)", "b)"),
          legend="right",
          nrow=2
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
colSums(support.Hu[,,2])/iter


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

s<-2 #N=1200
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
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP))+
  theme_minimal()+
  labs(#title = "",
    #subtitle = "",
    x="Size of the study set T",
    y="Aggregate PMP")+
  scale_color_discrete(labels=c("H1c", "H1", "Hu"))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c("#21908CFF", "#FDE725FF", "#440154FF" ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

median.PMP2.plot

ggplotly(median.PMP1.plot)


## Median PMP plot(no Hu) -------------------------

s<-2 #N=1200

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
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP))+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
    #subtitle = "Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs",
    x="Size of the study set T",
    y="Aggregate PMP")+
  scale_color_discrete(labels=c("H1c", "H1"))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 5),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 6),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c("#21908CFF", "#FDE725FF" ),labels=c("H1", "H1c"),name = "Hypothesis")

median.PMP1.noHu.plot

ggplotly(median.PMP1.plot)

## ggarrange(median PMP plots)----------------------------------
library(ggpubr)

Fig2<-ggarrange(median.PMP2.noHu.plot+ rremove("xlab"),
             median.PMP2.plot,
             labels = c("a)", "b)"),
             legend="right",
             nrow=2
) 
#ggsave("Outputs/Research Report/Fig2.png", plot = Fig2, width = 5, height = 3, units = "in", dpi = 300, bg="white")

# %>% 
#   annotate_figure(top = text_grob("Change of the aggregate PMPs for each hypothesis with increasing number of aggregared studies
#                                   <small>Points represent median PMPs, errorbars indicate the 2.5th and 97.5th percentile of the PMPs</small>
#                                   ", size = 14))




## Support Hu------------------------------------------
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

