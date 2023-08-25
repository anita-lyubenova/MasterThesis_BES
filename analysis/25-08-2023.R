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
  
  as<-sapply(1:nrow(a),function(x){
    #Is aggrBFiu>aggrBFcu
    a[x,1]>a[x,2]
  })
  
  b[i]<-sum(as)/nrow(a)
}

plot(1:1000,sort(b), type = "p")
data.frame(V1=1:1000,V2=sort(b)) %>% 
ggplot(aes(x=V1,y=V2))+
  geom_point()+
  geom_jitter()

#Insigth: individual BFiu and BFcu are symmetric
# In about half of the iterations there are more studies in the set supporting H1,
# In the other half of the iterations, more studies in the set support Hc
#Conclusion: the simulation precedure is okay. Also the computation of BFs is okay
#The problem is in the aggregation


a<-tau75[,c("H1", "Hc"),,1,"300"]


BFic<-a[,1]/a[,2]
log(BFic)
cumsum(log(BFic))

la<-log(a)
la1<-log(a+1)
logBFic1<-la1[,1]-la1[,2]
aggr_logBFic1<-cumsum(logBFic1) %>% plot(type="l")

aggr60_logBFic1<-numeric(1000)
for(i in 1:1000){
  a<-tau75[,c("H1", "Hc"),,i,"300"]
  
  la1<-log(a+1)
  logBFic1<-la1[,1]-la1[,2]
  
  aggr60_logBFic1[i]<-cumsum(logBFic1)[60]
  
}

sort(aggr60_logBFic1 )%>% plot()
aggr60_logBFic1 %>% plot(type="l")

ggplot(mapping=aes(x=1:1000, y=aggr60_logBFic1))+
  geom_line()+
  geom_hline(yintercept = 0)

#no log(+1)
aggr60_logBFic<-numeric(1000)
for(i in 1:1000){
  a<-tau75[,c("H1", "Hc"),,i,"300"]
  
  la<-log(a)
  logBFic<-la[,1]-la[,2]
  
  aggr60_logBFic[i]<-cumsum(logBFic)[60]
  
}

sort(aggr60_logBFic)%>% plot()

#no log()
aggr60_BFic<-numeric(1000)
for(i in 1:1000){
  a<-tau75[,c("H1", "Hc"),,i,"300"]
  
  BFic<-a[,1]/a[,2]
  
  aggr60_BFic[i]<-cumprod(BFic)[60]
  
}

sort(aggr60_BFic)%>% plot()

#replace BF=0 with a very very small value instead
aggr60_BFic_adj<-numeric(1000)
for(i in 1:1000){
  a<-tau75[,c("H1", "Hc"),,i,"300"]
  adjustment<-0.0000001
  a[a==0]<-adjustment#0.00000000000000000001
  a[a==2]<-2-adjustment
  BFic<-a[,1]/a[,2]
  
  aggr60_BFic_adj[i]<-cumprod(BFic)[60]
  
}

PMP<-aggr60_BFic_adj/(aggr60_BFic_adj+1)
plot(PMP)
sort(aggr60_BFic_adj)%>% plot()

data.frame(V1=1:1000,V2=log(aggr60_BFic_adj)) %>% 
  ggplot(aes(x=V1,y=V2))+
  geom_point()+
  geom_jitter()
#


#replace BF=0 with a very very small value instead
#and use log()
aggr60_logBFic_adj<-numeric(1000)
for(i in 1:1000){
  a<-tau75[,c("H1", "Hc"),,i,"300"]
  adjustment<-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
  a[a==0]<-adjustment#0.00000000000000000001
  a[a==2]<-2-adjustment
  la<-log(a)
  logBFic<-la[,1]-la[,2]
  
  aggr60_logBFic_adj[i]<-cumsum(logBFic)[60]
  
}

#PMP<-aggr60_BFic_adj/(aggr60_BFic_adj+1)
sort(aggr60_logBFic_adj)%>% plot()

data.frame(V1=1:1000,V2=aggr60_logBFic_adj) %>% 
  ggplot(aes(x=V1,y=V2))+
  geom_point()

data.frame(V1=1:1000,V2=sort(aggr60_logBFic_adj)) %>% 
  ggplot(aes(x=V1,y=V2))+
  geom_point()+
  geom_vline(xintercept = 500)+
  geom_hline(yintercept = 0)
