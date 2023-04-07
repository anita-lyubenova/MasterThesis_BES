library(dplyr)
library(abind)


#a function that takes a list created with run_sim() and reshapes 
#it to an array with 4 dim :: BF_4d[t, hyp, iter, n]
reshapeBFs<-function(BF_list, # a list, where each element (a dataframe, col=hypotheses, rows=iterations*studies) contains the BFs for a certain sample size n
                     n,
                     studies,
                     iterations){
  
  #2) Split BF dataframes into lists for each iteration
  system.time(
    BF_split<-
      lapply(BF_list, function(d){
        d<-d %>%
          as.data.frame() %>% 
          mutate(spl=as.numeric(gl(iterations, studies, iterations*studies)), .before=H1)
        
        d<-split(d, d$spl)
      } )
  )
  
  #abind the iterations to the 3rd dim of an array
  system.time(
    BF_3darray<-lapply(BF_split, function(x){
      do.call(abind, args=list(x, along=3) )
    } )
  )
  #abind the n variations to the 4th dim of the array
  BF_4d<-do.call(abind::abind, args=list(BF_3darray, along=4))
  BF_4d<-BF_4d[,-1,,]#remove column spl used to split iterations from studies
  
  #fix dimnames
  dimnames(BF_4d)<-list(1:studies,
                        dimnames(BF_4d)[[2]],
                        1:iterations,
                        n
  )
  
  attributes(BF_4d)<-c(attributes(BF_4d),
                          attributes(BF_list))
  
  return(BF_4d)
}


# 
# #a function to transform BFs to aggregated PMPs
# aggregatePMP<-function(x, # a 4 dim array with structure [t, BF, pop, iter, n]
#                        hyp=c("H1", "Hu"),
#                        studies=10 #number of studies to aggregate over, max 40
# ){
#   hyp_index<-substr(hyp,2,2)
#   BF<-x
#   BF<-BF[1:studies, dimnames(BF)[[2]] %in% hyp,,,drop=FALSE]
#   
#   nom<-aperm(BF, perm=c(1,3,4,2))
#   denom<-rowSums(nom, dims = 3)
#   PMP_perm<-nom/replicate(length(hyp),denom)
#   
#   #placeholder for tha aggregated PMPs
#   PMP_t<-PMP_perm
#   for(t in 2:studies){ #the PMPs of the first study remain the same, thus iterate from t=2
#     nom_t<-PMP_t[t-1,,,]*nom[t,,,]#multiply the previous PMPs with the current BFs
#     denom_t<-rowSums(nom_t, dims=2)
#     PMP_t[t,,,]<-nom_t/replicate(length(hyp), denom_t)
#   }
#   
#   PMP_t<-aperm(PMP_t, perm = c(1,4,2,3))
#   dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))
#   dimnames(PMP_t)[[1]]<-1:studies
#   
#   attributes(PMP_t)<-c(attributes(PMP_t),
#                        attributes(x)[c("hypothesis","complexity", "r2", "pcor", "ratio_beta", "p", "model", "seed")],
#                        hypothesis_test=paste0(hyp, collapse = " vs. "))
#   
#   return(PMP_t)
# }
# 
# 
# run_pipe<-function(BF_list,
#                    r2=0.13,
#                    pcor=0.3,
#                    n=c(25,100,200,250,350,500, 800),
#                    studies,
#                    iterations,
#                    hyp=c("H1", "Hu")
# ){
#   reshapeBFs(BF_list = BF_list,
#              n=n,
#              studies = studies,
#              iter = iterations
#   ) %>% 
#     aggregatePMP(studies=studies, hyp=hyp)
# }




