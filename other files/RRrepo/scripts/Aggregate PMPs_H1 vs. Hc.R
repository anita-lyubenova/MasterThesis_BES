# PMPs without Hu ------------------------------
#Compute aggregated Posterior Model Probabilities (PMPs) from the simulated Bayes Factors
#restricted to hypoteses H1 and Hc, excluding H0 and Hu

load("workspaces/Simulate BFs_ES(H1)=ES(Hc).RData")


BF.noHu<-BF[,2:3,,,] #consider only Hi vs. Hc
n.hyp<-2  #number of hypotheses
aggrPMP.noHu<-BF.noHu  #placeholder for the PMPs

for(s in 1:length(n)){  #for each sample size s
  for(i in 1:iter){       # for each iteration t
    for(t in 1:studies){    # for each study t
      for(h in 1:n.hyp){  #for each hypothesis h
        for(r in 1:2){ #for each ratio Hi:Hc
          #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
          aggrPMP.noHu[t,h,r,i,s]<-prod(BF.noHu[1:t,h,r,i,s])/sum(apply(matrix(BF.noHu[1:t,,r,i,s],nrow = t,ncol = n.hyp),2,prod))
          
        }
      } 
    }
  }
}
dimnames(aggrPMP.noHu)[[2]]<-c("PMP_i","PMP_c")


aggrPMP1.noHu<-aggrPMP.noHu[,,1,,]#H1:Hc = 1:1; H1 vs. Hc
aggrPMP2.noHu<-aggrPMP.noHu[,,2,,]#H1:Hc = 3:1; H1 vs. Hc

#remove unnecessary variables
remove(BF,BF.noHu,aggrPMP.noHu,h,i,s,t,r,n.hyp)

#save results
save.image(file="workspaces/Aggregate PMPs_H1 vs. Hc.RData")