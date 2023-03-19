#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [compl_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

library(tidyverse)
library(abind)
load("Outputs/accuracy/dat (merged simulated files).RData")

#a function to transform BFs to aggregated PMPs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=40 #number of studies to aggregate over, max 40
){
  hyp_index<-substr(hyp,2,2)
  BF<-x$BF
  BF<-BF[1:studies,substr(dimnames(BF)[[2]],3,3) %in% hyp_index,,,,drop=FALSE]
  
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
  PMP_t<-list(PMP=PMP_t,
              r2=x$r2,
              pcor=x$pcor,
              populations=x$populations,
              hypothesis=x$hypothesis,
              model=x$model,
              iter=x$iter,
              studies=studies,
              hypothesis_test = paste(hyp, collapse = " vs. ")
              )
  
  return(PMP_t)
}
a<-aggregatePMP(dat,
                hyp=c("H1","Hc" ,"Hu"),
                studies = 10)



#a function to compute confusion matrix from aggregated PMPs
accuracyPMP<-function(listPMP, #list created with aggregatePMP()
                     hyp_to_pop # a named character vector; elements are subset of dimnames(PMP)[[3]]: "TRUE_H0", "TRUE_H1", "TRUE_Hc", "TRUE_Hu", "HETEROG_H1p.1", "HETEROG_H1p.3", "HETEROG_H1p.5", names are the hypotheses for which the populations are true
                    
){
  #subset the populations of interest
  PMP<-listPMP$PMP[,,hyp_to_pop,,]
  dim(PMP)
  hyp_index<-substr(names(hyp_to_pop),2,2)
  
  tr<-data.frame(hyp=names(hyp_to_pop),
                 PMP=paste0("PMP", hyp_index),
                 pop=hyp_to_pop,
                 correct=NA
                 )

  for(h in 1:nrow(tr)){

    correct_logical<-PMP[,tr$PMP[h],tr$pop[h],,]>PMP[,!dimnames(PMP)[[2]] %in% tr$PMP[h],tr$pop[h],,]
    correct_count<-rowSums(aperm(correct_logical, c(1,3,2)), dims = 2)

    correct_name<-paste0("c", tr$pop[h])
    assign(correct_name, correct_count)
  }
  
  paste(paste0("c", tr$pop), collapse = "+")
  acc<-eval(parse(text = paste(paste0("c", tr$pop), collapse = "+")))/(nrow(tr)*listPMP$iter)
  list(acc=acc,
       r2=listPMP$r2,
       pcor=listPMP$pcor,
       hypothesis=listPMP$hypothesis,
       model=listPMP$model,
       iter=listPMP$iter,
       studies=listPMP$studies,
       populations=listPMP$populations[hyp_to_pop],
       hypothesis_test=listPMP$hypothesis_test,
       hyp_to_pop=hyp_to_pop
       )

}

accuracyPMP(listPMP = a,
            hyp_to_pop =c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="TRUE_Hu")
            )

library(corrplot)
corrplot(A, method = "shade")



library(ggcorrplot)
Ar<-round(A, digits = 2)
p<-ggcorrplot(Ar,
           outline.col = "white",
           lab = TRUE)

p + scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5)
