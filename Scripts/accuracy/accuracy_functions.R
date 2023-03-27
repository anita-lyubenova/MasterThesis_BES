library(ggcorrplot)
library(patchwork)
library(tidyverse)
library(abind)

#a function to transform BFs to aggregated PMPs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10 #number of studies to aggregate over, max 40
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
# 
# 
# listPMP<-aggregatePMP(x,
#                       hyp=c("H1", "Hu"),
#                       studies=10
#                       )
# hyp_to_pop=c(H1="TRUE_H1", Hu="TRUE_Hc", Hu="TRUE_H0")
# hyp_to_pop=c(H1="TRUE_H1", Hu="TRUE_H0")

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
                 pop=hyp_to_pop
  )
  # a character vector whose elements are the names of the variables storing the correct classifications for each population
  correct_name<-paste0("c", tr$pop)
  
  #a loop that computes the true classifications for each population and stores it into the respective variables
  for(h in 1:nrow(tr)){
    
    #a character string that specifies which comparison should be made
    # the comparison yields TRUE if is the PMPs of the true Hypothesis of the current population [h] are larger than the remaining PMPs
    comp<- paste(paste0("PMP[,tr$PMP[h],tr$pop[h],,,drop=FALSE] > ",
                        paste0("PMP[,",which(!dimnames(PMP)[[2]] %in% tr$PMP[h]), ",tr$pop[h],,,drop=FALSE]")
    ),collapse = " & ")
    
    #perform the comparison specified in the string
    correct_logical<-eval(parse(text = comp))
    
    #compute the number of correct classifications by summing the TRUE values
    correct_count<-rowSums(aperm(correct_logical, c(1,2,3,5,4)), dims = 4)[,,,,drop=TRUE]
    
    #store the number of correct classifications in the respective variable
    assign(correct_name[h], correct_count)
  } 
  #the result of the loop are variables with names specified in correct_name
  #there is a variable for each population 
  #each variable contains the true classifications for each population
  

  true<-lapply( mget(correct_name), function(x) x/listPMP$iter)
  
  acc<-eval(parse(text = paste(correct_name, collapse = "+")))/(nrow(tr)*listPMP$iter)
  
  list(acc=round(acc, digits = 3),
       TP=true,
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


acc_corrplot<-function(a, # a list created with accuracyPMP()
                       object # what should be plotted? Acc or TP?
){
  
  ggcorrplot(as.data.frame(a[object]),   #a$acc,
             outline.col = "white",
             lab = TRUE)+
    scale_fill_gradient2(limit = c(0,1),
                         low = "blue", high =  "red",
                         mid = "white",
                         midpoint = 0.87)+
    scale_x_continuous(breaks = 1:a$studies)+
    labs(title = a$hypothesis_test,
         subtitle =paste("Populations:", paste(a$hyp_to_pop, collapse = ", "), collapse = " ")
    )
}

acc_lineplot<-function(x){
  accdat<-x$acc %>%
    as.data.frame() %>% 
    rownames_to_column("t") %>% 
    pivot_longer(cols=starts_with("n"),
                 names_to = "n",
                 values_to = "acc"
    )
  accdat %>% 
    ggplot(aes(x=factor(t, levels=unique(t)), y=acc, group=n, color=n)) +
    geom_point()+
    geom_line()+
    theme_minimal()+
    labs(title = x$hypothesis_test,
         subtitle = paste0("Populations:",paste(x$hyp_to_pop, collapse = ",")),
         x="studies")+
    geom_hline(yintercept = 0.87, color="red", linetype="dashed")+
    scale_y_continuous(breaks = seq(0,1,0.1))
  
}
