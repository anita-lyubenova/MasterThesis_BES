# Create dataset model TP as a function of 
# [population definition] R2, pcor, diff.beta, p (heterogeneity: SD_betas<-p*beta),
# [sample definition]     n, t
# [hypothesis definition] complexity, n.par

library(tidyverse)
library(bain)
library(magrittr)
library(parallel)
library(foreach)
library(doParallel)
# test.RData ---------------------------------------------------------------

load("Outputs/generate datasets/test.RData")
iter<-10
hypothesis="V1>V2>V3"
computeBFs<-function(data, hypothesis, n.cores=7, n, studies, iter){
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(tidyverse)
    library(abind)
  })
  
  #1) Compute Bfs
  system.time(
    BF_list <-
      foreach(s = 1:length(n),
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
  
  #BF_list
  #2) Split BFs into lists for each iteration
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
BFs<-computeBFs(test,
                hypothesis = hypothesis,
                 n.cores = 3,
                 n=c(25,100),
                 studies=4,
                 iter=10
                 )
system.time(
PMP<-computeBFs(test, hypothesis = hypothesis,
           n.cores = 3,
           n=c(25,100),
           studies=4,
           iter=10) %>% 
aggregatePMP(c("H1","Hu"), studies = 3)
)

# array structure :: BF_4d[t, hyp, iter, n]

## 3. compute aggregate. PMPs -----------------------------------------------------
x<-BF_4d
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



PMP<-aggregatePMP(BF_4d, c("H1","Hu"), studies = 3)

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
    pivot_longer(cols =starts_with("n"),
                 names_to = "n",
                 values_to = "TPR") %>% 
    mutate(n=as.numeric(substr(n,2,nchar(n))),
           t=as.numeric(t))
  return(TPR)
  
}




aggregatePMP(BF_4d, c("H1","Hu"), studies = 4) %>% 
  TPRi(true_pop = "H1")
















#TRUE_H1.RData --------------------------------------------------------
load("Outputs/generate datasets/TRUE_H1.RData")
n<-c(25,100,200,250,350,500, 800)
iter<-1000
studies<-40

#label the names of the list elements corresponding to different sample sizes n
names(TRUE_H1)<-paste0("n", n)

#label the names of the list elements corresponding to different study numbers t
TRUE_H1<-lapply(TRUE_H1, FUN=function(x) {
  names(x)<-paste0("t",rep(1:studies, times=iter))
  return(x)
    }
  )

TRUE_H1$n25[names(TRUE_H1$n25)=="t1"]

#from each data.frame (at the lowest level) I obtain BFiu for hypothesis Hi
# V1 > V2 > V3 >0  :: ci=0.0118714 
# (V1,V2,V3)>0     :: ci=0.0778
# V1 > V2 > V3     :: ci=0.1354515
# V1> V2 & V3>0    :: ci=0.2754828
# V1 > (V2,V3)     :: ci=0.3104396
# V1+V2+V3>0       :: ci=0.5


lm(Y~., data=TRUE_H1$n25$t1)%>% 
  bain(hypothesis = "V1>V2+V3")%$%fit %>%     #$BF.u[c(1,2,4)]
  extract(c(1, nrow(.)),c("Com","BF.u")) %>%  #subset only BFiu for the specified hypothesis and the complement
  rbind(.,1) #add BFuu = 1


a<-lapply(TRUE_H1[c("n25", "n50")], function(n){
  lapply(n, function(t){
    lm(Y~., data=t)%>% 
      bain(hypothesis = "V1>V2>V3")%$%fit %>%     #$BF.u[c(1,2,4)]
      extract(c(1, nrow(.)),c("Com","BF.u")) %>%  #subset only BFiu for the specified hypothesis and the complement
      rbind(.,1) #add BFuu = 1
      }
    )
  }
)

data.frame(R2=0.13,
           pcor=0.3,
           diff.betas=0.07
           )















