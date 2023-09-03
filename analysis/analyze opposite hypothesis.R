library(patchwork)
source("analysis/analysis functions.R")

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

