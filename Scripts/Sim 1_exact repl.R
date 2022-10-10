# Research Question:
# Given that all studies in the study set have the same power level (eta) to
# support the true hypothesis , what is the minimum power level eta 
# needed ao that the overall power of BES (zeta) is acceptable?

#where
#eta = P(BFic > 1| Hi) = P(BFci > 1 | Hc)  (power in indiviudal studies)
#zeta = P(PMPaggr.ic > thres | Hi) = P(PMPaggr.ci < 1-thres | Hc) (BES-power)???

library(beepr)
library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
library(MASS)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(plotly)
library(readxl)
library(mvtnorm)
library(highcharter)
library(lavaan)
library(writexl)
library(ggplot2)
library(ggrepel)
#

#load the functions
load("Outputs/functions.RData")

#load the sample sizes used for the simulation
planned.n<-read_xlsx("Simulations planning.xlsx", sheet = "Sim1")


# Simulation 1 -----------------------------------------------------------
## k=2 -------------
# size of the study set: T= 10
# power levels: c(.50, .60, .70, .80, .90, .95, .99) [to obtain BFic > 1 for the correct hypothesis (Hi or Hc) ]

#Research Question: Given that all studies have the same power to support the true hypothesis over the complement,
#                   what power level is enough to produce reliable BES-aggregate support?





### Hi == TRUE-------------------------
models <- c("normal")
pcor <- c(0.2)
r2<-0.0409782632894305
ratio_beta <- c(2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
d=betas[1]-betas[2]
hypothesis<-"V1 > V2"

complement<-TRUE

iter<-10000

row.names<-paste0("Iter.", seq(1:iter))
column.names<-c(paste0("Study.", seq(1:10)), "log.aggr.BF", "aggr.PMP")
slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))

BFic1<-array(NA, dim = c(iterations=iter, studies=length(column.names), conditions=nrow(planned.n)),
                  dimnames = list(row.names,column.names, slice.names))


seed<-123

#for each condition (manipulated sample size distribution in the set of studies) (slice m in the array)
for(m in 1:nrow(planned.n)){
  
  n<-planned.n[m, 3:12] %>% as.numeric()
  
  #for each iteration (row i in the array)
  for(i in 1:iter) {
    
    #for each study; column s in the array
    for(s in 1:length(n)){
      
      seed=seed+1
      set.seed(seed)
      
      print(paste("Condition m:", m, ", Iteration i:", i, "Study s:", s))
      
      BF<-gen_dat(r2=r2, 
                  betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                  rho=cormat(pcor, length(ratio_beta)),
                  n=n[s],
                  "normal")%$%
        lm(Y ~ V1 + V2) %>%
        BF(hypothesis = hypothesis, complement = complement) %$%
        BFtable_confirmatory %>% as.data.frame()%$% BF
      
      BFic1[i,s,m]<-BF[1]/BF[2] 
      
      
    }# end iterations loop i
    
    #after all 10 studies in the set were simulated and evaluated in iteration i => calculate the log aggregate BF for iteration i
    BFic1[i,11,m] <-sum(log(BFic1[i,1:10,m]))
    #calculate the aggregate PMPs
    BFic1[i,12,m] <- prod(BFic1[i,1:10,m])/(prod(BFic1[i,1:10,m]) + 1)
    
  }# end study loop s
  
}#end conditions loop; THE END





#### Violin plots(aggr.PMPs) ----------------------------------------------------

vioplot.ic1.df<-data.frame(BFic1[,"aggr.PMP",1:nrow(planned.n)]) %>% 
  pivot_longer(cols = paste0("Condition.", 1:nrow(planned.n)),
               names_to = "condition",
               values_to = "aggr.PMP") %>% 
  arrange(match(condition, paste0("Condition.", 1:nrow(planned.n)))) %>% 
  mutate(power=rep(planned.n$power, each=iter))

vioplot.ic1.df$condition<-factor(vioplot.ic1.df$condition, levels = unique(vioplot.ic1.df$condition))


correct.aggr.ic1<-vioplot.ic1.df %>% 
  group_by(condition,power) %>% 
  summarize(correct.75 = sum(aggr.PMP>.75)/iter,
            correct.90 = sum(aggr.PMP>.90)/iter,
            correct.95 = sum(aggr.PMP>.95)/iter
            )



vioplot.ic1<-vioplot.ic1.df %>% 
  #boxplot with the PMPs per condition 
  ggbetweenstats(x = condition,
                 y = aggr.PMP,
                 pairwise.comparisons = FALSE,
                 results.subtitle=FALSE,
                 type = "nonparametric",
                 plot.type = "boxviolin",
                 centrality.plotting=FALSE
  ) +
  labs(
    x = "Condition",
    y = "aggregate PMP",
    title = paste("Distribution of aggregate PMPs from 10 studies with equal power (eta) when testing Hi:",hypothesis ," against Hc across", iter, "iterations
                  when Hi is true in the population"),
    subtitle = "Each point represents an aggregate PMP from 10 studies from one iteration",
    caption = paste("Population specifications: pcor:",pcor, ";r2 =", r2 , "; b1:b2 = ",ratio_beta[1],":",ratio_beta[2],"; d = b1 - b2 =", d, "; Hi:", hypothesis)
    
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=8)
  )+ 
  geom_hline(yintercept=c(0.75, 0.90, 0.95), linetype="dashed", 
             color = "red", size=0.8)+
  scale_x_discrete(labels=paste(paste("eta =", planned.n$power,"\n"), "n = ", planned.n$Study.1)
                   )+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.77), times=nrow(planned.n)),
           label =paste("P(PMP>.75) =", correct.aggr.ic1$correct.75),
           size=2.7)+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.92), times=nrow(planned.n)),
           label =paste("P(PMP>.90) =", correct.aggr.ic1$correct.90),
           size=2.7)+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.97), times=nrow(planned.n)),
           label =paste("P(PMP>.95) =", correct.aggr.ic1$correct.95),
           size=2.7)

vioplot.ic1



### Hc == TRUE -------------------------
# Ratio_beta is switched
models <- c("normal")
pcor <- c(0.2)
r2<-0.0409782632894305
ratio_beta <- c(1,2)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
d=betas[1]-betas[2]
hypothesis<-"V1 > V2"

complement<-TRUE

iter<-10000

row.names<-paste0("Iter.", seq(1:iter))
column.names<-c(paste0("Study.", seq(1:10)), "log.aggr.BF", "aggr.PMP")
slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))

BFci1<-array(NA, dim = c(iterations=iter, studies=length(column.names), conditions=nrow(planned.n)),
            dimnames = list(row.names,column.names, slice.names))


seed<-123

#for each condition (manipulated sample size distribution in the set of studies) (slice m in the array)
for(m in 1:nrow(planned.n)){
  
  n<-planned.n[m, 3:12] %>% as.numeric()
  
  #for each iteration (row i in the array)
  for(i in 1:iter) {
    
    #for each study; column s in the array
    for(s in 1:length(n)){
      
      seed=seed+1
      set.seed(seed)
      
      print(paste("Condition m:", m, ", Iteration i:", i, "Study s:", s))
      
      BF<-gen_dat(r2=r2, 
                  betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                  rho=cormat(pcor, length(ratio_beta)),
                  n=n[s],
                  "normal")%$%
        lm(Y ~ V1 + V2) %>%
        BF(hypothesis = hypothesis, complement = complement) %$%
        BFtable_confirmatory %>% as.data.frame()%$% BF
      
      BFci1[i,s,m]<-BF[1]/BF[2] 
      
      
    }# end iterations loop i
    
    #after all 10 studies in the set were simulated and evaluated in iteration i => calculate the log aggregate BF for iteration i
    BFci1[i,11,m] <-sum(log(BFci1[i,1:10,m]))
    #calculate the aggregate PMPs
    BFci1[i,12,m] <- prod(BFci1[i,1:10,m])/(prod(BFci1[i,1:10,m]) + 1)
    
  }# end study loop s
  
}#end conditions loop; THE END



#### Violin plots(aggr.PMPs) ----------------------------------------------------

vioplot.ci1.df<-data.frame(BFci1[,"aggr.PMP",1:nrow(planned.n)]) %>% 
  pivot_longer(cols = paste0("Condition.", 1:nrow(planned.n)),
               names_to = "condition",
               values_to = "aggr.PMP") %>% 
  arrange(match(condition, paste0("Condition.", 1:nrow(planned.n)))) %>% 
  mutate(power=rep(planned.n$power, each=iter))

vioplot.ci1.df$condition<-factor(vioplot.ci1.df$condition, levels = unique(vioplot.ci1.df$condition))

correct.aggr.ci1<-vioplot.ci1.df %>% 
  group_by(condition,power) %>% 
  summarize(correct.25 = sum(aggr.PMP<1-.75)/iter,
            correct.10 = sum(aggr.PMP<1-.90)/iter,
            correct.05 = sum(aggr.PMP<1-.95)/iter
  )



vioplot.ci1<-vioplot.ci1.df %>% 
  #boxplot with the PMPs per condition 
  ggbetweenstats(x = condition,
                 y = aggr.PMP,
                 pairwise.comparisons = FALSE,
                 results.subtitle=FALSE,
                 type = "nonparametric",
                 plot.type = "boxviolin",
                 centrality.plotting=FALSE
  ) +
  labs(
    x = "Condition",
    y = "aggregate PMP",
    title = paste("Distribution of aggregate PMPs from 10 studies with equal power (eta) when testing Hi:",hypothesis ," against Hc across", iter, "iterations when Hc is true in the population"),
    subtitle = "Each point represents an aggregate PMP from 10 studies from one iteration",
    caption = paste("Population specifications: pcor:",pcor, ";r2 =", r2 , "; b1:b2 = ",ratio_beta[1],":",ratio_beta[2],"; d = b1 - b2 =", d, "; Hi:", hypothesis)
    
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=8)
  )+ 
  geom_hline(yintercept=c(0.25, 0.10, 0.05), linetype="dashed", 
             color = "red", size=0.8)+
  scale_x_discrete(labels=paste(paste("eta =", planned.n$power,"\n"), "n = ", planned.n$Study.1)
  )+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.23), times=nrow(planned.n)),
           label =paste("P(PMP<.25) =", correct.aggr.ci1$correct.25),
           size=2.7)+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.08), times=nrow(planned.n)),
           label =paste("P(PMP<.10) =", correct.aggr.ci1$correct.10),
           size=2.7)+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.03), times=nrow(planned.n)),
           label =paste("P(PMP<.5) =", correct.aggr.ci1$correct.05),
           size=2.7)

vioplot.ci1




















## k=3 -------------
power.lvls<-c(seq(from=0.5, to=0.95, by=0.05),0.99)


r2<-.13
ratio_beta <- c(3,2,1)
pcor <- c(0.2)
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
models <- c("normal")
hypothesis<-"V1 > V2 > V3"
complement<-TRUE

power.k3$n 
planned.n<-data.frame(Condition=1:length(power.lvls),
                      power=power.lvls,
                      Study.1 = power.k3$n,
                      Study.2 = power.k3$n,
                      Study.3 = power.k3$n,
                      Study.4 = power.k3$n,
                      Study.5 = power.k3$n,
                      Study.6 = power.k3$n,
                      Study.7 = power.k3$n,
                      Study.8 = power.k3$n,
                      Study.9 = power.k3$n,
                      Study.10 = power.k3$n
                      )
set.seed(123)

iter<-1000

row.names<-paste0("Iter.", seq(1:iter))
column.names<-c(paste0("Study.", seq(1:10)), "log.aggr.BF", "aggr.PMP")
slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))

BFic<-array(NA, dim = c(iterations=iter, studies=length(column.names), conditions=nrow(planned.n)),
            dimnames = list(row.names,column.names, slice.names))

seed<-123

#for each condition (manipulated sample size distribution in the set of studies) (slice m in the array)
for(m in 1:nrow(planned.n)){
  
  n<-planned.n[m, 3:12] %>% as.numeric()
  
  #for each iteration (row i in the array)
  for(i in 1:iter) {
    
    #for each study; column s in the array
    for(s in 1:length(n)){
      
      seed=seed+1
      set.seed(seed)
      
      print(paste("Condition m:", m, ", Iteration i:", i, "Study s:", s))
      
      BF<-gen_dat(r2=r2, 
                  betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                  rho=cormat(pcor, length(ratio_beta)),
                  n=n[s],
                  "normal")%$%
        lm(Y ~ V1 + V2 + V3) %>%
        BF(hypothesis = hypothesis, complement = complement) %$%
        BFtable_confirmatory %>% as.data.frame()%$% BF
      
      BFic[i,s,m]<-BF[1]/BF[2] 
      
      
    }# end iterations loop i
    
    #after all 10 studies in the set were simulated and evaluated in iteration i => calculate the log aggregate BF for iteration i
    BFic[i,11,m] <-sum(log(BFic[i,1:10,m]))
    BFic[i,12,m] <- prod(BFic[i,1:10,m])/(prod(BFic[i,1:10,m]) + 1)
    
  }# end study loop s
  
}#end conditions loop; THE END


#Visualization of results ---------------------------
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(htmltools)

#how the power of individual studies varies with the power of BES
correct.aggr.ic1.long<-correct.aggr.ic1 %>% 
  pivot_longer(cols=c("correct.75", "correct.90", "correct.95"),
               names_to = "stakes_level",
               values_to = "BES_power"
               )

correct.aggr.ic1.long$stakes_level<-as.factor(correct.aggr.ic1.long$stakes_level)
levels(correct.aggr.ic1.long$stakes_level)<-c("low (PMP>.75)", "high (PMP>.90)", "very high (PMP>.95)")

correct.aggr.ic1.long %>% 
  ggplot(aes(x=power, y=BES_power, group=stakes_level, color=stakes_level))+
  geom_line()+
  geom_point()+
  geom_text_repel(aes(y=BES_power, label=round(BES_power,2), color=stakes_level),
            size=4)+
  labs(title = "Variation of BES-power (y-axis) depending on power in the study set (x-axis) and level of the stakes (separate lines)",
       x="Study set power",
       y="BES-power",
       color="Stakes",
       caption ="Conditions: Test of the Hi V1>V2 agains Hc, when k=2, pcor = 0.2, R2 = 0.04, b1:b2 = 2:1, d = b1-b2 = 0.084; Study set: T = 10, the studies have equal power"
         )+
  scale_y_continuous(breaks = seq(0.45,1, 0.05))+
  theme_minimal()
  


vioplot.ci2





# 09.10.2022 ----------------------------------------------------------------------------
#I chose to use a small R2=.02 and small q=.11 (for k=2)
sim0.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim0")[1,]
sim1.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim1")
sim2.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim2")

sim1.k2.p2.t10.exact<-run.sim(pcor = sim0$pcor,
              r2 = sim0$r2,
              ratio_beta = eval(parse(text=sim0$ratio_beta)),
              q=sim0$q,
              iter=10000,
              seed=123,
              hypothesis = "V1 > V2",
              t=10,
              planned.n = sim1.plan,
              manipulated = "average power"
)

save(sim1.k2.p2.t10.exact, file="Outputs/sim1/sim1.k2.p2.t10.exact.RData")


sim1.k2.p2.t10.exact$plot.BESpower.per.cond
sim1.k2.p2.t10.exact$vioplot.ic

