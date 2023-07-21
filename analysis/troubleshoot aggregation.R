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

create_median_plot_data(tau75_PMP,
                                       "delta0_tau0.75",
                                       n="300") %>% 
  median_plot()


sum(tau75_PMP$PMP[30,"PMP1",1 ,,"300"] >0.5)


# troubleshoot aggregatePMP-----------------------------------------------

## aggr logBFs-------------------

hyp=c("H1", "Hc")
studies=60

hyp_index<-substr(hyp,2,2)
search_terms <- paste0(hyp, collapse = "|")
BF<-tau75
BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]
logBF<-log(BF)
#aggregate BFs
aggrlogBFiu<-apply(logBF, MARGIN = c(2,3,4,5), cumsum)

#compute aggrPMPs
nom=aperm(aggrBFiu, perm=c(1,3,4,5,2))
denom=rowSums(nom, dims = 4) 
##copy the array in a 5th dimension, so that it has the same dim as nom; e.g. denom_repl[,,,,1]==denom_repl[,,,,2]
denom_repl<-replicate(length(hyp),denom)
#compute PMPs
aggrPMP_perm<-nom/denom_repl
aggrPMP<-aperm(aggrPMP_perm,perm=c(1,5,2,3,4))
dimnames(aggrPMP)[[2]]<-paste0("PMP", c(hyp_index))


## aggr non-log BFs-------------------
x=tau75

  hyp_index<-substr(hyp,2,2)
  search_terms <- paste0(hyp, collapse = "|")
  BF<-x
  BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]
  
  #aggregate BFs
  aggrBFiu<-apply(BF, MARGIN = c(2,3,4,5), cumprod)
  
  #compute aggrPMPs
  #move the                                    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx hypotheses-dimension to the last dimension
  nom=aperm(aggrBFiu, perm=c(1,3,4,5,2))
  #sum the aggrBFiu
  denom=rowSums(nom, dims = 4) 
  ##copy the array in a 5th dimension, so that it has the same dim as nom; e.g. denom_repl[,,,,1]==denom_repl[,,,,2]
  denom_repl<-replicate(length(hyp),denom)
  #compute PMPs
  aggrPMP_perm<-nom/denom_repl
  aggrPMP<-aperm(aggrPMP_perm,perm=c(1,5,2,3,4))
  dimnames(aggrPMP)[[2]]<-paste0("PMP", c(hyp_index))



sum(BF==0)

sum(aggrBFiu==0)
sum(aggrBFiu==Inf)

sum(aggrlogBFiu==1)

sum(is.na(nom))
sum(nom==Inf)
sum(nom==0)

sum(is.na(denom))
sum(denom==0)
sum(denom_repl==0)

#all NA cases in aggrPMP_perm are the cases in which the denominator == 0
sum(which(denom_repl==0) == which(is.na(aggrPMP_perm)))
# these are the cases in which aggrPMP_perm must be 0?
# It doesn't make much sense because the denomitaor is the sum of the aggr support  for all hypotheses
# that is, if it is 0, it means that the support for all hypotheses is 0... which is impossible

denom0ind<-which(denom_repl==0, arr.ind = T)
denom0ind

nom[1:58  ,  1,  149  ,  7 ,  ]
BF[1:58,,1,149,7]
# d = 0 & large tau => 
#  the effect can be large positive and negative =>
#  some studies in the set will show max support for H1 while others will show minimum support for H1
#  for different studies in the set H1 will be 0 and 2 (max); same goes for Hc
#  when at least one Bfiu and BFcu in the set is 0 => the aggrBFiu = 0 AND aggrBFcu = 0 => 
#  aggregate support for both studies is 0 =>
#  aggrPMP = NA, because of division by 0

#This is not the case when PMP




sum(aggrPMP_perm==0, na.rm = T)
sum(is.na(aggrPMP_perm))
sum(aggrPMP_perm==Inf)

logBF[1:2,1,1,1,1]
sum(-0.2561809, -1.6508689)
aggrlogBFiu[1:2,1,1,1,1]

aggrBFiu[1:2,1,1,1,1] %>% log()

sum(round(log(aggrBFiu),digits=2)!=round(aggrlogBFiu, digits=2))
which(round(log(aggrBFiu),digits=2)!=round(aggrlogBFiu, digits=2), arr.ind = T)


log(aggrBFiu)[58,1,1,149 ,7]
aggrlogBFiu[58,1,1,149 ,7]

aggrBFiu[58,1,1,149 ,7]
log(0)

exp(aggrlogBFiu[58,1,1,149 ,7])

# functional forms of PMPs ------------------------------

bf=seq(0, 2, by=0.1)

pmp=bf/(bf+1)

plot(bf,pmp, type="l")


logbf=log(bf)
lpmp=log(bf)/(log(bf)+0) # makes no sense
logpmp=log(bf/(bf+1)) #<=>
logpmp=log(bf)-log(bf+1)
plot(log(bf),logpmp, type="l")


# bf_ic


bfic=bfiu/bfcu

ci=0.5
bfiu=fi/ci = fi/0.5

=> fi= 0.5*bfiu

bfcu = (1-fi)/(1-ci) = (1-fi)/0.5

bfcu=(1-0.5*bfiu)/0.5 = 2-bfiu

bfiu=seq(0, 2, by=0.01)
bfiu=rgamma(n=10000, shape = 1.5, rate=0.5)
density(bfiu) %>% plot(type="l")

bfcu = (1-0.5*bfiu)/0.5
bfiu[1:10]
bfcu[1:10]


pmp=bfiu/(bfiu+bfcu)
plot(bfiu, pmp, type="l")

bfic=bfiu/bfcu
pmp=bfic/(bfic+1)
plot(bfic, pmp, type="l")


logpmp=log(bfiu)-log(bfiu+bfic) # =>
logpmp = log(bfiu)- log(2)
plot(log(bfiu),logpmp, type="l")


logpmp = log(bfic) - log(bfic+1)# <=>
logpmp = log(bfic) - log1p(bfic)
plot(exp(log(bfic)),exp(logpmp), type="l")


## multiple studies -----------------------
logpmp=log(bfiu)-log(bfiu_1*bfiu_2*...*bfiu_t + bfcu_1*bfcu_2*...*bfcu_t)

plot(bfiu)
plot(log(bfiu))


#tau=0.45 -------------------------------
x=dat
studies=30
hyp=c("H1", "Hc")
#a function to compute aggregate PMPs for selected hypotheses from the BFs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10, #number of studies to aggregate over, max 40,
                       subset=NULL
){

  hyp_index<-substr(hyp,2,2)
  
  search_terms <- paste0(hyp, collapse = "|")
  BF<-x
  BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]
  
  #aggregate BFs
  aggrBFiu<-apply(BF, MARGIN = c(2,3,4,5), cumprod)
  
  #compute aggrPMPs
  #move the hypotheses-dimension to the last dimension
  nom=aperm(aggrBFiu, perm=c(1,3,4,5,2))
  #sum the aggrBFiu
  denom=rowSums(nom, dims = 4) 
  ##copy the array in a 5th dimension, so that it has the same dim as nom; e.g. denom_repl[,,,,1]==denom_repl[,,,,2]
  denom_repl<-replicate(length(hyp),denom)
  #compute PMPs
  aggrPMP_perm<-nom/denom_repl
  aggrPMP<-aperm(aggrPMP_perm,perm=c(1,5,2,3,4))
  dimnames(aggrPMP)[[2]]<-paste0("PMP", c(hyp_index))
  
  
  aggrPMP<-  rlist::list.append(attributes(x), PMP=aggrPMP, hypothesis_test = paste(hyp, collapse = " vs. ")) 
  return(aggrPMP)
} # end aggregatePMP

PMP.45<-dat %>% aggregatePMP(
             hyp=hyp,
             studies = studies)
create_median_plot_data(PMP.45,
                        "delta0_tau0.45",
                        n="300") %>% 
  median_plot()


sum(PMP.45$PMP[30,"PMP1","delta0_tau0.45" ,,"300"] >0.5)/1000

## initial aggregation ------------------------------

#a function to compute aggregate PMPs for selected hypotheses from the BFs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10, #number of studies to aggregate over, max 40,
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
    denom_t<-rowSums(nom_t, dims=3)
    PMP_t[t,,,,]<-nom_t/replicate(length(hyp), denom_t)
  }
  
  PMP_t<-aperm(PMP_t, perm = c(1,5,2,3,4))
  dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))
  
  PMP_t<-  rlist::list.append(attributes(x), PMP=PMP_t, hypothesis_test = paste(hyp, collapse = " vs. ")) 
  return(PMP_t)
}

PMP.45_old<-dat %>% aggregatePMP(
                             hyp=hyp,
                             studies = studies)
sum(PMP.45_old$PMP[30,"PMP1","delta0_tau0.45" ,,"300"] >0.5)/1000
