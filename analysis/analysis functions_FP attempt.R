r2=0.13
pcor=0.3
hypothesis="V1>V2>V3"
ratio_beta=c(3,2,1)
p=0
model="normal"

betas<-coefs(r2=r2, ratio_beta, rho=cormat(pcor, length(ratio_beta)), model=model) 
dat<-gen_dat(r2, -betas, rho=cormat(pcor, length(ratio_beta)), n=10000, model = model)
lm(Y~V1+V2+V3, dat)

listPMP<-dat%>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30
               #   subset = "dat[,,,,-c(1,3,11)]"
  )

hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
               Hc="r0.13_pcor0.3_b123_p0"
)

a<-accuracyPMP(listPMP,hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                           Hc="r0.13_pcor0.3_b123_p0"
))

b<-accuracyPMP(listPMP,hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                                      Hu="r0.13_pcor0.3_b123_p0",
                                      Hu="r0.13_pcor0.3_b123_p.86"
))
h<-names(hyp_to_pop)[1]
h<-names(hyp_to_pop)[2]

hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
  Hu="r0.13_pcor0.3_b123_p0",
  Hu="r0.13_pcor0.3_b321_p0.86"
)
dimnames(listPMP$PMP)

#a function to compute True positive rates from aggregated PMPs
accuracyPMP<-function(listPMP, #list created with aggregatePMP()
                      hyp_to_pop # a named character vector; elements are subset of dimnames(PMP)[[3]]: "TRUE_H0", "TRUE_H1", "TRUE_Hc", "TRUE_Hu", "HETEROG_H1p.1", "HETEROG_H1p.3", "HETEROG_H1p.5", names are the hypotheses for which the populations are true
                      
){
  #subset the populations of interest
  PMP<-listPMP$PMP[,,hyp_to_pop,,]
  dimnames(PMP)
  hyp_index<-substr(names(hyp_to_pop),2,2)
  
  tr<-data.frame(hyp=names(hyp_to_pop),
                 PMP=paste0("PMP", hyp_index),
                 pop=hyp_to_pop
  )
  
  correct_count<-lapply(1:nrow(tr), function(h){
    #a character string that specifies which comparison should be made
    # the comparison yields TRUE if is the PMPs of the true Hypothesis of the current population [h] are larger than the remaining PMPs
    compTP<- paste(
              paste0(
                  "PMP[,tr$PMP[h],tr$pop[h],,,drop=FALSE] > ",  #the PMPs of the correct hypothesis
                  paste0(
                    "PMP[,",which(!dimnames(PMP)[[2]] %in% tr$PMP[h]), ",tr$pop[h],,,drop=FALSE]") #the PMPs of the incorerct hypohtesis or hypotheses
    ),collapse = " & ")
    
    #perform the comparison specified in the string
    correct_logical<-eval(parse(text = compTP))
    
    #compute the number of correct classifications by summing the TRUE values
    rowSums(aperm(correct_logical, c(1,2,3,5,4)), dims = 4)[,,,,drop=TRUE]
  })
  
  correct_prop<-lapply(correct_count, function(x) x/listPMP$dim[[4]])
  names(correct_prop)<-paste0("c", tr$pop)
  
  acc<-Reduce('+', correct_count)/(nrow(tr)*listPMP$dim[[4]])
  
  #####################   False Positives  ####################################################
  h<-names(hyp_to_pop)[1]
  h<-names(hyp_to_pop)[2]
  ############################################################
  #for each hypothesis, how often it was most supported when it was not true
  incorrect_count<-lapply(unique(names(hyp_to_pop)), function(h){
    print(h)
    #a character string that specifies which comparison should be made
    # the comparison yields TRUE if is the PMPs of the current Hypothesis h are higher than the PMPs of the correct hypothesis
    paste(paste0("PMP[,unique(tr[tr$hyp==h,]$PMP),",which(dimnames(PMP)[[3]] %in% tr[tr$hyp!=h,]$pop),",,,drop=FALSE] >"),
          "PMP[,tr[tr$hyp==h,]$PMP,tr[tr$hyp==h,]$pop,,,drop=FALSE]", collapse = " | ")

    # select the PMPs of the current hypothesis h in populations in which h is not correct
    incorrectPMP<-paste0(
      "PMP[,unique(tr[tr$hyp==h,]$PMP),", #the PMPs of the current hypothesis h
      which(dimnames(PMP)[[3]] %in% tr[tr$hyp!=h,]$pop), #the index of the population(s) the hypothesis h is not correct
      ",,,drop=FALSE] >")
    
    #PMPS of h in populations where h is correct 
   # correctPMP<-"PMP[,unique(tr[tr$hyp==h,]$PMP),tr[tr$hyp==h,]$pop,,,drop=FALSE]"
    
    correctPMP<-"PMP[,unique(tr[tr$hyp==h,]$PMP),",which(dimnames(PMP)[[3]] %in% tr[tr$hyp==h,]$pop) ," ,,,drop=FALSE]"
  
    a<-paste("(",paste(v1,v2, collapse = " | "), ")")
    b<-paste("(",paste(v1,rev(v2), collapse = " | "), ")")
    
    v3<-c(
      paste("(",paste(incorrectPMP,correctPMP, collapse = " | "), ")"),
      paste("(",paste(incorrectPMP,rev(correctPMP), collapse = " | "), ")")
    )
    paste(v3, collapse = " & ")
    
    compFP<-paste(incorrectPMP, correctPMP, collapse = " | ")
    compFP<-paste(# check {PMP_h | h!=TRUE} > {PMP_h | h==TRUE}
                  paste0(# select the PMPs of the current hypothesis h in populations in which h is not correct
                        "PMP[,unique(tr[tr$hyp==h,]$PMP),", #the PMPs of the current hypothesis h
                        which(dimnames(PMP)[[3]] %in% tr[tr$hyp!=h,]$pop), #the index of the population(s) the hypothesis h is not correct
                        ",,,drop=FALSE] >"),
                  #
                  
                  collapse = " | ")
    
    dimnames( PMP[,unique(tr[tr$hyp==h,]$PMP),1,,,drop=FALSE])[2:3]
    dimnames(PMP[,unique(tr[tr$hyp==h,]$PMP),tr[tr$hyp==h,]$pop,,,drop=FALSE])[2:3]
    PMP[,unique(tr[tr$hyp==h,]$PMP),1,,,drop=FALSE] > PMP[,unique(tr[tr$hyp==h,]$PMP),tr[tr$hyp==h,]$pop,2,,drop=FALSE]
    
    
    #perform the comparison specified in the string
    incorrect_logical<-eval(parse(text = compFP))
    
    #compute the number of correct classifications by summing the TRUE values
    rowSums(aperm(incorrect_logical, c(1,2,3,5,4)), dims = 4)[,,,,drop=TRUE]
  })
  
  
  
  incorrect_prop<-lapply(incorrect_count, function(x) x/listPMP$dim[[4]])
  names(incorrect_prop)<-paste0("c", tr$pop)
  
  list(acc=round(acc, digits = 3),
       TP=correct_prop,
       r2=listPMP$r2,
       pcor=listPMP$pcor,
       hypothesis=listPMP$hypothesis,
       model=listPMP$model,
       iter=listPMP$iter,
       studies=listPMP$studies,
       populations=listPMP$populations[hyp_to_pop],
       hypothesis_test=listPMP$hypothesis_test,
       hyp_to_pop=hyp_to_pop,
       n=listPMP$n
  )
  
}
