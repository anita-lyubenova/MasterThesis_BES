library(patchwork)
source("analysis/analysis functions.R")
#Opposite hypothesis: H1: X1<X0

#LOAD DATA ----------------------------------------------------
dat<-readRDS("pre-processing/output/processed_data_opposite hypothesis.rds")
dimnames(dat)[[3]]
pops<-dimnames(dat)[[3]]
tau<-as.numeric(attributes(dat)$tau)
delta<-as.numeric(attributes(dat)$delta)

#  MEDIAN PLOTS ------------------------------------------------------


### delta = 0 ------------------------
a<-compl_medplot(dat,
                 studies = 30,
                 population = "delta0_tau0",
                 n="300")

b<-compl_medplot(dat,
                 studies = 30,
                 population = "delta0_tau0.15",
                 n="300")
c<-compl_medplot(dat,
                 studies = 30,
                 population = "delta0_tau0.3",
                 n="300")
d<-compl_medplot(dat,
                 studies = 30,
                 population = "delta0_tau0.45",
                 n="300")
mp.d0<-wrap_plots(a,b,c,d, ncol = 1)+
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

ggsave("analysis/output/mp.d0_opposite hypothesis.png", plot = mp.d0, width = 5, height = 7, units = "in", dpi = 300, bg="white")


# jittered PMPs ----------------------
pops
#delta0_tau0.45
PMPlist<-aggregatePMP(dat, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hc"),
                       studies=40, #number of studies to aggregate over, max 40,
                       subset=NULL
)
PMPlist$PMP[,,"delta0_tau0.45",2,"300"]
PMPlist$PMP[30,,"delta0_tau0.45",,"300"]
sum(PMPlist$PMP[30,"PMP1","delta0_tau0.45",,"300"]>0.5)

melted<-PMPlist$PMP[40,,"delta0_tau0.45",,"300"] %>% reshape2::melt()

ggplot(melted, aes(y=value, x=factor(Var1))) +
  geom_violin()+ 
  geom_jitter(height = 0.1, width = 0.1)
# After aggregating 40 studies:
#    1) the PMPs for H1 and Hc are clustered at 1 and 0
#    2) the PMP_H1 have a higher density at 1 than at 0: i.e. they are 1 in more of the iterations


melted<-PMPlist$PMP[c(5,10,30,40),,"delta0_tau0.45",,"300"] %>% reshape2::melt()

jittered_PMPs_tau0.45<-
  melted %>% 
  rename(studies=Var1,
         Hypothesis=Var2,
         aggrPMP=value) %>% 
  ggplot(aes(y=aggrPMP, x=factor(studies), color=factor(Hypothesis))) +
  geom_point(shape=1, position = position_jitterdodge(jitter.width = 0.5,
                                                      jitter.height = 0.18))+
  labs(x="studies", y="Aggregated PMPs", subtitle = "H1 vs Hc, delta=0, tau=0.45, n=300")+
  scale_y_continuous(breaks = seq(0,1, by=0.2))+
  theme_minimal()

melted %>% 
  group_by(Var1,Var2) %>% 
  summarise(prop_larger.5=sum(value>0.5)/1000)

