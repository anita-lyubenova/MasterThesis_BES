library(beepr)
#load the functions
load("Outputs/functions.RData")

#load the sample sizes used for the simulation
planned.n<-read_xlsx("Simulations planning.xlsx", sheet = "Sim1")


# Simulation 1 -----------------------------------------------------------

# size of the study set: T= 10
# power levels: c(.50, .60, .70, .80, .90, .95, .99) [to obtain BFic > 1 for the correct hypothesis (Hi or Hc) ]

#Research Question: Given that all studies have the same power to support the true hypothesis over the complement,
#                   what power level is enough to produce reliable BES-aggregate support?

r2<-.04
ratio_beta <- c(2,1)
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
pcor <- c(0.2)
models <- c("normal")
hypothesis<-"V1 > V2"

complement<-TRUE

set.seed(123)

iter<-1000

row.names<-paste0("Iter.", seq(1:iter))
column.names<-c(paste0("Study.", seq(1:10)), "log.aggr.BF", "aggr.PMP")
slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))

BFic<-array(NA, dim = c(iterations=iter, studies=length(column.names), conditions=nrow(planned.n)),
                  dimnames = list(row.names,column.names, slice.names))

scatterp.BFic<-list()

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
  summarize(correct.aggr = sum(aggr.PMP>.75)/iter)

vioplot.ic.df <- mutate(vioplot.ic.df,correct.aggr=rep(correct.aggr$correct.aggr, each=iter))


vioplot.ic<-vioplot.ic.df %>% 
  #boxplot with the PMPs per condition 
  ggbetweenstats(x = condition,
                 y = aggr.PMP,
                 pairwise.comparisons = FALSE,
                 results.subtitle=FALSE,
                 type = "nonparametric",
                 plot.type = "boxviolin"
  ) +
  labs(
    x = "Condition",
    y = "aggregate PMP",
    title = "Distribution of aggregate PMPs from 10 studies with equal power (eta) when testing Hi against Hc",
    subtitle = "Across conditions the power level eta is manipulated"
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=8)
  )+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "grey", size=1)+
  scale_x_discrete(labels=paste(paste("eta =", planned.n$power,"\n"), "n = ", planned.n$Study.1)
                   )+
  annotate("label",
           x = seq(1:nrow(planned.n))+0.3,
           y = rep(c(0.77, 0.82), times=nrow(planned.n)/2),
           label =paste("P(PMP>.75) =", correct.aggr$correct.aggr),
           size=2.7)

vioplot.ic

beep("coin")





