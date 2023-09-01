# Start investigating anew why H1 is systematically more supported than Hc,
# especially with higher heterogeneity levels

#The problem:
#When d=0 and tau is large H1 is more supported across iterations after aggregating a enough studies
#( it seem to happen when tau is small too - it just takes more aggregated studies)

source("analysis/analysis functions.R")

#Load data
tau75<-readRDS(file="pre-processing/output/processed_data_BF_d0_t0.75.rds")
dat<-readRDS("pre-processing/output/processed_data_combined.rds")
#a function to compute aggregate PMPs for selected hypotheses from the BFs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10, #number of studies to aggregate over, max 40,
                       subset=NULL
){
  # #subset if specified, while retaining the attributes
  # if(!is.null(subset)){
  #   att<-attributes(x)
  #   att<-att[names(att)[-grep("dim", names(att))]]
  #   x<-eval(parse(text = subset))
  #   attributes(x)<-c(attributes(x), att)
  # }
  # 
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
} # end aggregatePMP

#aggregate
tau75_PMP<-tau75 %>% aggregatePMP( # a 5 dim array with structure [t, BF, pop, iter, n]
  hyp=c("H1", "Hc"),
  studies=60
) 


dimnames(dat)
dimnames(tau75)

#individual study BFs
b<-numeric(1000)
for(i in 1:1000){
  a<-tau75[,c("H1", "Hc"),,i,"300"]
  
  as<-a[,1]>a[,2]
  
  b[i]<-sum(as)/nrow(a)
}

plot(1:1000,sort(b), type = "p")
data.frame(V1=1:1000,V2=b) %>% 
ggplot(aes(x=V1,y=V2))+
  geom_point()+
  geom_vline(xintercept = 500)+
  geom_hline(yintercept = 0.5)
  #geom_jitter()
sum(b>0.5)

#Insigth: individual BFiu and BFcu are symmetric
# In about half of the iterations there are more studies in the set supporting H1,
# In the other half of the iterations, more studies in the set support Hc
#Conclusion: the simulation precedure is okay. Also the computation of BFs is okay
#The problem is in the aggregation


a<-tau75[,c("H1", "Hc"),,1,"300"]

## aggr non-log BFs-------------------
BF=tau75
hyp=c("H1", "Hc")
studies=60

hyp_index<-substr(hyp,2,2)
search_terms <- paste0(hyp, collapse = "|")
BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]

nom<-aperm(BF, perm=c(1,3,4,5,2))
denom<-rowSums(nom, dims = 4)
#Each BF is divided by the sum of all BFs
PMP_perm<-nom/replicate(length(hyp),denom)

#placeholder for the aggregated PMPs
PMP_t<-PMP_perm
for(t in 2:studies){ #the PMPs of the first study remain the same, thus iterate from t=2
  nom_t<-PMP_t[t-1,,,,]*nom[t,,,,]#multiply the previous PMPs with the current BFs
  denom_t<-rowSums(nom_t, dims=2) #sum PMP_i(t-1)*BFiu(t) + PMP_c(t-1)*BFcu(t)+ ...etc
  PMP_t[t,,,,]<-nom_t/replicate(length(hyp), denom_t)
}

PMP_t<-aperm(PMP_t, perm = c(1,5,2,3,4))
dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))

dimnames(denom)
dim(replicate(length(hyp),denom))
dim(denom)
dimnames(nom)
dimnames(nom_t)

PMP_t[,,,5,"300"]
BF[,,,5,"300"]


d<-PMP_t[,,,,"300"] %>%
  reshape2::melt() %>% 
  rename(t=Var1,
         hyp=Var2,
         iter=Var3,
         PMP=value)
d %>% filter(hyp=="PMP1") %>% 
ggplot(aes(x=t, y=PMP, group=iter))+
  geom_line()+
  geom_jitter(width = 0.1, height = 0.1)

d %>% filter(hyp=="PMPc") %>% 
  ggplot(aes(x=t, y=PMP, group=iter))+
  geom_line()+
  geom_jitter(width = 0.1, height = 0.1)

a<-tau75[,c("H1", "Hc"),,,"300",drop=FALSE]

sum(a[,1,,,]==0)
sum(a[,2,,,]==0)







dim(a[,2,,,])



