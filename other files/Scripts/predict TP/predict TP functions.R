
library(tidyverse)
library(bain)
library(magrittr)
library(parallel)
library(foreach)
library(doParallel)
library(doRNG)
library(doSNOW)
source("scripts/ThomVolker scripts/functions.R")

# This would take tooo much time
# get_complexity<-function(data, hypothesis, cl=2, hyp=c("H1","Hc", "Hu")){
#   
#   registerDoParallel(cl)
#   compl<-
#     foreach(i = 1:(studies*iter),
#             .combine=rbind,
#             .packages = c("bain", "magrittr")
#     )%dopar% {
#       set.seed(123)
#       lm(Y~., data=data[[1]][[i]])%>% 
#         bain(hypothesis = hypothesis)%$%fit %>%     #$BF.u[c(1,2,4)]
#         extract(c(1, nrow(.)),c("Com")) %>%   #subset only BFiu for the specified hypothesis and the complement
#         c(.,"Hu"=1)
#       
#       
#     }
#   
#   colnames(compl)<-hyp
#   list(avg_compl=colMeans(compl),
#        hist =  hist(compl[,1]))
# }

# compl<-get_complexity(test, hypothesis = hypothesis, cl=2)
# compl$avg_compl

computeBFs<-function(data, hypothesis, n, studies, iter, n.cores=7){
  
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(tidyverse)
    library(abind)
  })
  
  #add progress bar
  iterations <- length(n)*iter*studies # total number of conditions to iterate over
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(x) setTxtProgressBar(pb, x)
  opts <- list(progress = progress)
  
  #1) Compute Bfs
  system.time(
    #BF_list is a list of dataframes (containing BFs), where each data frame has studies*iter rows
    #i.e., the data frames contain all BFs within this sample size s
    BF_list <-
      foreach(s = 1:length(n),
              .options.snow = opts,
              .packages = c("bain", "magrittr", "dplyr")
      )%:%
      foreach(i = 1:(studies*iter),
              .combine = rbind
      ) %dopar% {
        lm(Y~., data=data[[s]][[i]])%>% 
          bain(hypothesis = hypothesis)%$%fit %>%     #$BF.u[c(1,2,4)]
          extract(c(1, nrow(.)),c("BF.u")) %>%  #subset only BFiu for the specified hypothesis and the complement
          c(.,"BFuu"=1)
      }
  )
  close(pb)
  

  #2) Split BF dataframes into lists for each iteration
  BF_split<-
    parLapply(cl, BF_list, function(d){
      d<-d %>%
        as.data.frame() %>% 
        mutate(spl=gl(iter, studies, studies*iter), .before =V1)
      
      d<-split(d, d$spl)
      #d<-split(d, gl(iter, studies, studies*iter))
      # return(d)
    } )
  
  # 2.5) abind the list elements to array
  BF_3darray<-parLapply(cl,BF_split, function(x){
    do.call(abind, args=list(x, along=3) )
  } )
  
  stopCluster(cl)
  
  BF_4d<-do.call(abind::abind, args=list(BF_3darray, along=4))
  BF_4d<-BF_4d[,-1,,]#remove column iter
  
  #fix dimnames
  dimnames(BF_4d)<-list(1:studies,
                        c("BF1u", "BFcu", "BFuu"),
                        1:iter,
                        n
  )
  
  class(BF_4d) <- "numeric" #transform BFs from character to numeric
  
  return(BF_4d)
}
# array structure :: BF_4d[t, hyp, iter, n]


# x<-BF_4d
#a function to transform BFs to aggregated PMPs
aggregatePMP<-function(x, # a 4 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10 #number of studies to aggregate over, max 40
){
  hyp_index<-substr(hyp,2,2)
  BF<-x
  BF<-BF[1:studies,substr(dimnames(BF)[[2]],3,3) %in% hyp_index,,,drop=FALSE]
  
  nom<-aperm(BF, perm=c(1,3,4,2))
  denom<-rowSums(nom, dims = 3)
  PMP_perm<-nom/replicate(length(hyp),denom)
  
  #placeholder for tha aggregated PMPs
  PMP_t<-PMP_perm
  for(t in 2:studies){ #the PMPs of the first study remain the same, thus iterate from t=2
    nom_t<-PMP_t[t-1,,,]*nom[t,,,]#multiply the previous PMPs with the current BFs
    denom_t<-rowSums(nom_t, dims=2)
    PMP_t[t,,,]<-nom_t/replicate(length(hyp), denom_t)
  }
  
  PMP_t<-aperm(PMP_t, perm = c(1,4,2,3))
  dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))
  dimnames(PMP_t)[[1]]<-1:studies
  
  return(PMP_t)
}



# PMP<-aggregatePMP(BF_4d, c("H1","Hu"), studies = 3)

# 4. compute the TP rates

#a function to compute confusion matrix from aggregated PMPs
TPRi<-function(PMP, #an array created with aggregatePMP()[studies, hyp, iter, n]
               true_pop = "H1" # the true population
               
){
  #subset the populations of interest
  #PMP<-listPMP$PMP[,,hyp_to_pop,,]
  dim(PMP)
  hyp_index<-substr(true_pop,2,2)
  true_pop_col<-paste0("PMP", hyp_index)
  
  correct_logical<-PMP[,true_pop_col,,]>PMP[,!dimnames(PMP)[[2]] %in% true_pop_col,,]
  #compute the number of correct classifications by summing the TRUE values
  correct_count<-rowSums(aperm(correct_logical, c(1,3,2)),# iterations -> last dim
                         dims = 2#sum over the last dimension (iterations)
  )
  TPR<-correct_count/dim(PMP)[[3]]
  
  TPR<-
    TPR %>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(PMP)[[4]],
                 names_to = "n",
                 values_to = "TPR") %>% 
    mutate(n=as.numeric(n),
           t=as.numeric(t))
  return(TPR)
  
}


add_info<-function(TPR,
                   complexity,
                   R2,
                   pcor,
                   ratio_beta
){
  #TPR[,c("R2", "pcor")]<-c(R2,pcor)
  TPR$R2<-R2
  TPR$pcor<-pcor
  TPR[,"diff_betas"]<-Reduce(`-`,coefs(R2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"), accumulate = TRUE)[2]
  TPR[,"complexity"]<-get_complexity(data=data, hypothesis=hypothesis)$avg_compl[1]
  
  return(TPR)
}