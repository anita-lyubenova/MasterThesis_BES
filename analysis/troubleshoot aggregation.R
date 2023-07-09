source("analysis/analysis functions.R")

#Load data
tau75<-readRDS(file="pre-processing/output/processed_data_BF_d0_t0.75.rds")

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

## aggregate logBFs-------------------

hyp=c("H1", "Hc")
studies=60

hyp_index<-substr(hyp,2,2)
search_terms <- paste0(hyp, collapse = "|")
BF<-tau75
BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]

################################################################
logBF<-log(BF)

#aggregate logBFiu and logBFcu
aggrlogBFiu<-apply(logBF, MARGIN = c(2,3,4,5), cumsum)

#Compute aggrPMPs
#put BFiu and BFic in the final dimension
nom<-aperm(exp(aggrlogBFiu), perm=c(1,3,4,5,2))
#the result in the cells is BFiu+BFcu; thus the resulting array has 4 dimension (1 dim reduced)
#denom<-rowSums(nom, dims = 4) 
denom=nom[,,,,1,drop=FALSe]+nom[,,,,2,drop=FALSe]
#copy the array in a 5th dimension; e.g. denom_repl[,,,,1]==denom_repl[,,,,2]
denom_repl<-replicate(length(hyp),denom)
#divide each BFiu/cu by the respective slice of the 5th dim of denom_repl
aggrPMP_perm<-nom/denom_repl
dimnames(aggrPMP_perm)
aggrPMP<-aperm(aggrPMP_perm,perm=c(1,5,2,3,4))
dimnames(aggrPMP)[[2]]<-paste0("PMP", c(hyp_index))

sum(aggrPMP[60,"PMP1",1 ,,"300"] >0.5)

sum(is.na(denom_repl))
sum(denom_repl==0)

BF[,1,,,,drop=FALSE]+BF[,2,,,,drop=FALSE]

###################################################################

logBF<-log(BF)

#compute logBFic
logBFic<-logBF[,1,,,,drop=FALSE]-logBF[,2,,,,drop=FALSE]
#aggregate cumulatively BFic
aggrlogBFic<-apply(logBFic, MARGIN = c(2,3,4,5), cumsum)

aggrBFic<-exp(aggrlogBFic)

aggrPMPi<-aggrBFic/(aggrBFic+1)

replicate(length(hyp),aggrPMPi)

dim(aggrPMPi)
