
#dimnames(BF)
#BF[1:studies t, 2:hypotheses h, 3:ratio H1:Hc r, 4:iteration i, 5:sample size s]

#function to aggregate the PMPs for 
aggregatePMP<-function(BF, #the simulated BFs from the simulate_BF_.R scripts
                       hyp=c("0", "i", "c", "u"), #which hypotheses are of interest
                       iter=1000,
                       studies=40
                       
                       ){
  iter<-iter
  n.hyp<-length(hyp)
  n<-dimnames(BF)[[5]]
  #subset only hypotheses Hi vs. Hc vs. Hu (exlude H0)
  BF.temp<-BF[,substr(dimnames(BF)[[2]],3,3) %in% hyp,,,]  
  
  #placeholder for the aggregate PMPs
  aggrPMP<-BF.temp
  
  for(s in 1:length(n)){  #for each sample size s
    for(i in 1:iter){       # for each iteration t
      for(t in 1:studies){    # for each study t
        for(h in 1:n.hyp){  #for each hypothesis h
          for(r in 1:length(dimnames(BF.temp)[[3]])){ #for each ratio Hi:Hc
            #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
            aggrPMP[t,h,r,i,s]<-prod(BF.temp[1:t,h,r,i,s])/sum(apply(matrix(BF.temp[1:t,,r,i,s],nrow = t,ncol = n.hyp),2,prod))
            
          }
        } 
      }
    }
  }
  dimnames(aggrPMP)[[2]]<-paste0("PMP_", hyp)
  
  return(aggrPMP)
}

#aggregatePMP(BF=BF, hyp = c("i", "c"))

