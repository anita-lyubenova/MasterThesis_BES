library(patchwork)
library(abind)
library(ggplot2)
source("analysis/analysis functions.R")

extra<-readRDS(file="pre-processing/output/processed_data_BF_d0_t0.45.rds")
dat<-readRDS("pre-processing/output/processed_data_combined.rds")
dimnames(dat)[[3]]

# tau=0.45 ---------------------------------
main<-dat[,,"delta0_tau0.45",,, drop=FALSE]

t60<-abind(main, extra, along = 1)
dimnames(t60)[[1]]<-1:60

#adjust the dims=2
#a function to compute aggregate PMPs for selected hypotheses from the BFs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hc"),
                       studies=60, #number of studies to aggregate over, max 40,
                       subset=NULL
){
  #subset if specified, while retaining the attributes
  if(!is.null(subset)){
    att<-attributes(x)
    att<-att[names(att)[-grep("dim", names(att))]]
    x<-eval(parse(text = subset))
    attributes(x)<-c(attributes(x), att)
  }
  
  hyp_index<-substr(hyp,2,2)
  search_terms <- paste0(hyp, collapse = "|")
  BF<-x
  BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]
  
  nom<-aperm(BF, perm=c(1,3,4,5,2))
  denom<-rowSums(nom, dims = 4)
  PMP_perm<-nom/replicate(length(hyp),denom)
  
  #placeholder for tha aggregated PMPs
  PMP_t<-PMP_perm
  for(t in 2:studies){ #the PMPs of the first study remain the same, thus iterate from t=2
    nom_t<-PMP_t[t-1,,,,]*nom[t,,,,]#multiply the previous PMPs with the current BFs
    denom_t<-rowSums(nom_t, dims=2)
    PMP_t[t,,,,]<-nom_t/replicate(length(hyp), denom_t)
  }
  
  PMP_t<-aperm(PMP_t, perm = c(1,5,2,3,4))
  dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))
  
  PMP_t<-  rlist::list.append(attributes(x), PMP=PMP_t, hypothesis_test = paste(hyp, collapse = " vs. ")) 
  return(PMP_t)
}

compl_medplot(t60, 
              studies = 60,
              population = "delta0_tau0.45",
              n="300")
x<-t60
dim(extra)
PMP_t<-aggregatePMP(x=t60, # a 5 dim array with structure [t, BF, pop, iter, n]
                    hyp=c("H1", "Hc"),
                    studies=60)


dim(PMP_t$PMP)

create_median_plot_data(PMP_t,
                        "delta0_tau0.45",
                        n="300") %>% 
  median_plot()

## jittered PMPs ----------------------
melted<-PMP_t$PMP[10,,1,,"300"] %>% reshape2::melt()

ggplot(melted, aes(y=value, x=factor(Var1))) +
  geom_violin()+ 
  geom_jitter(height = 0, width = 0.1)



melted<-PMP_t$PMP[c(5,10,30,40,50,60),,1,,"300"] %>% reshape2::melt()

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


extra_PMP<-aggregatePMP(extra, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hc"),
                       studies=30
)
create_median_plot_data(extra_PMP,
                        "delta0_tau0.45",
                        n="300") %>% 
  median_plot()


# tau=0.75------------------------------------------------------------------------

tau75<-readRDS(file="pre-processing/output/processed_data_BF_d0_t0.75.rds")

tau75_PMP<-tau75 %>% aggregatePMP( # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hc"),
                       studies=60
) 
mp.d0_tau0.75<-create_median_plot_data(tau75_PMP,
                        "delta0_tau0.75",
                        n="300") %>% 
  median_plot()


ggsave("analysis/output/mp.d0_tau0.75.png", plot = mp.d0_tau0.75, width = 7, height = 4, units = "in", dpi = 300, bg="white")

melted<-tau75_PMP$PMP[c(5,10,20,30,40,50,60),,1,,"300"] %>% reshape2::melt()

jittered_PMPs_tau0.75<-
  melted %>% 
  rename(studies=Var1,
         Hypothesis=Var2,
         aggrPMP=value) %>% 
  ggplot(aes(y=aggrPMP, x=factor(studies), color=factor(Hypothesis))) +
  geom_point(shape=1, position = position_jitterdodge(jitter.width = 0.5,
                                                      jitter.height = 0.18))+
  labs(x="studies", y="Aggregated PMPs", subtitle = "H1 vs Hc, delta=0, tau=0.75, n=300")+
  scale_y_continuous(breaks = seq(0,1, by=0.2))+
  theme_minimal()




jittered<-jittered_PMPs_tau0.45/jittered_PMPs_tau0.75

ggsave("analysis/output/jittered.png", plot = jittered, width = 12, height = 12, units = "in", dpi = 300, bg="white")


#tau=0 -----------------------------------------------------------------------------
tau0_PMP<-
  dat[,,"delta0_tau0",,, drop=FALSE] %>% 
  aggregatePMP( # a 5 dim array with structure [t, BF, pop, iter, n]
    hyp=c("H1", "Hc"),
    studies=30
) 
melted<-tau0_PMP$PMP[c(5,10,20,30),,1,,"300"] %>% reshape2::melt()

jittered_PMPs_tau0<-
  melted %>% 
  rename(studies=Var1,
         Hypothesis=Var2,
         aggrPMP=value) %>% 
  ggplot(aes(y=aggrPMP, x=factor(studies), color=factor(Hypothesis))) +
  geom_point(shape=1, position = position_jitterdodge(jitter.width = 0.5,
                                                      jitter.height = 0.18))+
  labs(x="studies", y="Aggregated PMPs", subtitle = "H1 vs Hc, delta=0, tau=0, n=300")+
  scale_y_continuous(breaks = seq(0,1, by=0.2))+
  theme_minimal()

