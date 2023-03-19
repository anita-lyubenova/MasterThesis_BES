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
confusePMP<-function(listPMP, #array created with aggregatePMP()$PMP
                     populations, # a named character vector; elements are subset of dimnames(PMP)[[3]]: "TRUE_H0", "TRUE_H1", "TRUE_Hc", "TRUE_Hu", "HETEROG_H1p.1", "HETEROG_H1p.3", "HETEROG_H1p.5", names are the hypotheses for which the populations are true
                     studies #number of studies for which to evaluate the confusion matrix, 
){
  #subset the populations of interest
  PMP<-listPMP$PMP[,,populations,,]
  dim(PMP)
  
}

PMP[,,"TRUE_H1",,] %>% dim

correctH1<-PMP[,"PMP1","TRUE_H1",,]>PMP[,"PMPu","TRUE_H1",,]
cH1<-rowSums(aperm(correctH1, c(1,3,2)), dims = 2 )
cH1/1000
correctHu<-PMP[,"PMP1",c("TRUE_Hc","TRUE_Hu"),,]<PMP[,"PMPu",c("TRUE_Hc","TRUE_Hu"),,]
rowSums(aperm(correctHu, c(1,2,4,3)), dims = 3 )/1000

dim(correctHu)

# correctHu<-PMP[,"PMP1",c("TRUE_Hc"),,]<PMP[,"PMPu",c("TRUE_Hc"),,] | PMP[,"PMP1",c("TRUE_Hu"),,]<PMP[,"PMPu",c("TRUE_Hu"),,]
# dim(correctHu)
# cHu<-rowSums(aperm(correctHu, c(1,3,2)), dims = 2 )

(cH1+cHu)/3000


#equivalent to the 1st correctHu
correctHu<-PMP[,"PMP1",-1,,]<PMP[,"PMPu",-1,,] 
dim(correctHu)
cHu<-rowSums(aperm(correctHu, c(1,4,2,3)), dims = 2)



cp<-array(NA, dim=c(40,2,length(pop), 5))

#useless
colSums(PMP[,"PMP1",,,]>PMP[,"PMPu",,,],dim=3)

pop<-c(H1="TRUE_H1", Hu="TRUE_Hc", Hu="TRUE_Hu")
hyp<-unique(names(pop)[names(pop)!=""])
matrix(NA,
       nrow = length(hyp),
       ncol = length(pop),
       dimnames = list(hyp, pop))

#a function to transform the BFs to PMPs, create a confusion matrix, and compute power and alpha
power_matrix_BES<-function(x, # a list with BFs created with sim_BES()
                           hyp=c(H0="H0",H1= "H1",Hc= "Hc"), # a named vector with elements indicating the tested hypotheses, and the names indicating the true populaiton for this hypothesis
                           n, #sample size of the studies. Either a number or a vector. 
                           t.max #number of studies. Either a number or a vector 
                           # hyp_names = dimnames(BF)[[2]], #a character vector indicating the names of the testerd hypotheses
                           # pop_names=substr(dimnames(BF)[[3]],6,7)
){
  
  hyp_index = substr(hyp,2,2)
  pop_names<-paste0("TRUE_", names(hyp))
  n<-x$n
  #t.max=max(t.range)
  
  
  #if no aggregation is needed you can see the 1000 across all 40 studies as independent => 40 000 iterations
  if(t.max==1){
    x$populations<-x$populations[-4] # remove the population Hu
    x$BF<-apply(x$BF[,paste0("BF",hyp_index,"u"),pop_names,], c(2,3), abind::abind)
    x$iter<-nrow(x$BF)
    x$studies<-1
    x$BF %>% dimnames()
    
    #compute PMPi = BFi/sum(BF of all hypotheses of interest)
    PMP<-x$BF
    
    for(i in 1:dim(x$BF)[[1]]){ #for each iteration
      for(j in 1:dim(x$BF)[[2]]){ #for each tested hypothesis
        for(z in 1:dim(x$BF)[[3]]){ # for each population
          
          PMP[i,j,z] <- x$BF[i,j,z]/sum(x$BF[i,,z])
          
        }  
      }
    }
    
    #add a silent 4th dimension with lenght 1, so that the shape of the array is the same as in the 
    #else{} condition, where the 4th dim is t
    dim(PMP)<- c(dim(PMP),1)
    
    #If more than 1 studies should be aggregated over => BES 
  }else{
    
    x$BF<-x$BF[,paste0("BF",hyp_index,"u"),pop_names,]
    
    PMP_BES_list<-aggregatePMP2(x, hyp = hyp, studies = t.max)
    
    #reorder the dimensions such that PMP_BES[iter, PMP_hyp, pop] for t aggregated studies only
    PMP<-aperm(PMP_BES_list$aggrPMP[1:t.max,,,,drop=FALSE],#subset only the row with t number of aggregated studies studies
               c(4,2,3,1)
    )
    #print(dimnames(PMP))
    
  }
  
  
  #create an array with the same dimensions as PMP but that will be indicate only the highest PMPs
  max.PMP<-PMP
  #produce power matrix
  for(i in 1:dim(PMP)[[1]]){ #for each iteration
    for(z in 1:dim(PMP)[[3]]){ # for each population
      for(j in 1:dim(PMP)[[4]]){
        
        #get the column index of the hypothesis with the highest PMPs
        max.index<-which(PMP[i,,z,j]==max(PMP[i,,z,j]))
        # replace the max PMPs with 1 and the remaining PMPs with 0 for iteration i and population z
        max.PMP[i,max.index,z,j]<-1
        max.PMP[i,-max.index,z,j]<-0
      }
    }  
  }
  
  conf_matrix<-apply(max.PMP, c(2,3,4), sum)/dim(max.PMP)[1]
  
  acc_data<-data.frame(n=n,
                       t=1:t.max,
                       acc=NA
  )
  
  for(j in 1:dim(PMP)[[4]] ){
    acc_data$acc[j] <- na.omit(sum(diag(conf_matrix[,,j]))/sum(conf_matrix[,,j]))
    
  }
  
  return(list(#matrix=conf_matrix,
    # metrics=TPFP,
    # plot_data=plot_data,
    acc_data=acc_data,
    sim_conditions=x[c("r2", "pcor", "hypotheses","populations", "model","iter")]
  )
  )
  
}#  end power_matrix_BES()


