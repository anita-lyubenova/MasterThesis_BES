library(patchwork)
library(abind)
library(ggplot2)
source("analysis/analysis functions.R")

extra<-readRDS(file="pre-processing/output/processed_data_BF_d0_t0.45.rds")
dat<-readRDS("pre-processing/output/processed_data_combined.rds")
dimnames(dat)[[3]]

main<-dat[,,"delta0_tau0.45",,, drop=FALSE]

t60<-abind(main, extra, along = 1)
dimnames(t60)[[1]]<-1:60

compl_medplot(t60, 
              studies = 60,
              population = "delta0_tau0.45",
              n="300")
x<-t60
dim(extra)
dimnames(nom_t)
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

dim(PMP_t$PMP)

create_median_plot_data(PMP_t,
                        "delta0_tau0.45",
                        n="300") %>% 
  median_plot()

dim(PMP_t)
melted<-PMP_t$PMP[10,,1,,"300"] %>% reshape2::melt()




ggplot(melted, aes(y=value, x=factor(Var1))) +
  geom_violin()+ 
  geom_jitter(height = 0, width = 0.1)



melted<-PMP_t$PMP[c(5,10,30,40,50,60),,1,,"300"] %>% reshape2::melt()

ggplot(melted, aes(y=value, x=factor(Var1), color=factor(Var2))) +
  geom_point(shape=1, position = position_jitterdodge(jitter.width = 0.5,
                                                      jitter.height = 0.2))



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
create_median_plot_data(tau75_PMP,
                        "delta0_tau0.75",
                        n="300") %>% 
  median_plot()
  
melted<-tau75_PMP$PMP[c(5,10,20,30,40,50,60),,1,,"300"] %>% reshape2::melt()
ggplot(melted, aes(y=value, x=factor(Var1), color=factor(Var2))) +
  geom_point(shape=1, position = position_jitterdodge(jitter.width = 0.5,
                                                      jitter.height = 0.2))









