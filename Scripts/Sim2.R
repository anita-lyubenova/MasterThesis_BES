#Research Question:
# Given that the average power is fixed, how does the distribution of power across the studies in 
# the sear affect the performance (or maybe power) of BES?


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



#load the functions
load("Outputs/functions.RData")


#load the sample sizes used for the simulation
planned.n<-read_xlsx("Simulations planning.xlsx", sheet = "Sim2")
planned.n$total.n <- apply(planned.n[3:12], 1, sum)



# Simulation 2 ---------------
models <- c("normal")
pcor <- c(0.2)
r2<-.04
ratio_beta <- c(2,1)
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
hypothesis<-"V1 > V2"
complement<-TRUE

iter<-1000

row.names<-paste0("Iter.", seq(1:iter))
column.names<-c(paste0("Study.", seq(1:10)), "log.aggr.BF", "aggr.PMP")
slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))

BFic<-array(NA, dim = c(iterations=iter, studies=length(column.names), conditions=nrow(planned.n)),
            dimnames = list(row.names,column.names, slice.names))

seed<-123

#for each condition (manipulated sample size distribution in the set of studies) (slice m in the array)
for(m in 1:nrow(planned.n)){
  
  n<-planned.n[1, 3:12] %>% as.numeric()
  
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
      
      BFic[i,s,m]<-BF[1]/BF[2] 
      
      
    }# end iterations loop i
    
    #after all 10 studies in the set were simulated and evaluated in iteration i => calculate the log aggregate BF for iteration i
    BFic[i,11,m] <-sum(log(BFic[i,1:10,m]))
    BFic[i,12,m] <- prod(BFic[i,1:10,m])/(prod(BFic[i,1:10,m]) + 1)
    
  }# end study loop s
  
}#end conditions loop; THE END





#### Violin plots(aggr.PMPs) ----------------------------------------------------

vioplot.ic.df<-data.frame(BFic[,"aggr.PMP",1:nrow(planned.n)]) %>% 
  pivot_longer(cols = paste0("Condition.", 1:nrow(planned.n)),
               names_to = "condition",
               values_to = "aggr.PMP") %>% 
  arrange(match(condition, paste0("Condition.", 1:nrow(planned.n)))) %>% 
  mutate(power=rep(planned.n$power, each=iter))

vioplot.ic.df$condition<-factor(vioplot.ic.df$condition, levels = unique(vioplot.ic.df$condition))

correct.aggr<-vioplot.ic.df %>% 
  group_by(condition,power) %>% 
  summarize(correct.75 = sum(aggr.PMP>.75)/iter,
            correct.90 = sum(aggr.PMP>.90)/iter,
            correct.95 = sum(aggr.PMP>.95)/iter
  )



vioplot.ic<-vioplot.ic.df %>% 
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
    title = paste("Distribution of aggregate PMPs from a set of 10 studies with the same average power (0.7, total N =", sum(planned.n) ,") when testing Hi against Hc across", iter, "iterations"),
    subtitle = "Each point represents an aggregate PMP from 10 studies from one iteration"
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=8)
  )+ 
  geom_hline(yintercept=c(0.75, 0.90, 0.95), linetype="dashed", 
             color = "red", size=0.8)+
  scale_x_discrete(labels=paste("eta.avg =", planned.n$power,"\n")
  )+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.77), times=nrow(planned.n)),
           label =paste("P(PMP>.75) =", correct.aggr$correct.75),
           size=2.7)+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.92), times=nrow(planned.n)),
           label =paste("P(PMP>.90) =", correct.aggr$correct.90),
           size=2.7)+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.97), times=nrow(planned.n)),
           label =paste("P(PMP>.95) =", correct.aggr$correct.95),
           size=2.7)

vioplot.ic







