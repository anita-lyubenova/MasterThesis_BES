#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [compl_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

library(tidyverse)
load("Outputs/accuracy/dat (merged simulated files).RData")

#a function to transform BFs to aggregated PMPs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies #number of studies to aggregate over, max 40
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
              r2=0.13,
              pcor=0.3,
              populations=list( # ratio beta
                TRUE_H0=c("b1:b2:b3 = 1:1:1"),
                TRUE_H1=c("b1:b2:b3 = 3:2:1"), #population H1 = TRUE
                TRUE_Hc=c("b1:b2:b3 = 1:2:3"),
                TRUE_Hu=paste(paste("50%", c("H1", "Hc")), collapse = " & "),
                HETEROG_H1p.1=c("b1:b2:b3 = 3:2:1 + +heterogeneity: SD_betas=0.1*betas"),
                HETEROG_H1p.3=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=0.3*betas"),
                HETEROG_H1p.5=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=0.5*betas")
              ),
              hypothesis="V1>V2>V3",
              model="linear",
              iter=1000,
              studies=40
              )
  
  return(PMP_t)
}
a<-aggregatePMP(dat,
                hyp=c("H1", "Hu"),
                studies = 10)

a$PMP %>% dimnames()

listPMP<-a
populations<-c(H1="TRUE_H1", Hu="TRUE_Hc", Hu="TRUE_Hu")
#a function to compute confusion matrix from aggregated PMPs
accuracyPMP<-function(listPMP, #list created with aggregatePMP()
                     populations, # a named character vector; elements are subset of dimnames(PMP)[[3]]: "TRUE_H0", "TRUE_H1", "TRUE_Hc", "TRUE_Hu", "HETEROG_H1p.1", "HETEROG_H1p.3", "HETEROG_H1p.5", names are the hypotheses for which the populations are true
                     studies #number of studies for which to evaluate the confusion matrix, 
){
  #subset the populations of interest
  PMP<-listPMP$PMP[,,populations,,]
  dim(PMP)
  hyp_index<-substr(names(populations),2,2)
  h<-1
  for(h in length(hyp_index)){
    PMPh<-paste0("PMP", hyp_index[h])
    notPMPh<-paste0("PMP", hyp_index[-h])
    a<-PMP[,PMPh,h,,]
    b<-PMP[,notPMPh,h,,]
    
    dim(a)
    dim(b)
    
  }
  
}

PMP[,,"TRUE_H1",,] %>% dim

correctH1<-PMP[,"PMP1","TRUE_H1",,]>PMP[,"PMPu","TRUE_H1",,]
cH1<-rowSums(aperm(correctH1, c(1,3,2)), dims = 2 )
cH1/1000


correctHu<-PMP[,"PMP1",-1,,]<PMP[,"PMPu",-1,,] #"-1" because 
dim(correctHu)
cHu<-rowSums(aperm(correctHu, c(1,4,2,3)), dims = 2)
cHu/2000


A<-(cH1+cHu)/3000

library(corrplot)
corrplot(A, method = "shade")



library(ggcorrplot)
Ar<-round(A, digits = 2)
p<-ggcorrplot(Ar,
           outline.col = "white",
           lab = TRUE)

p + scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5)
