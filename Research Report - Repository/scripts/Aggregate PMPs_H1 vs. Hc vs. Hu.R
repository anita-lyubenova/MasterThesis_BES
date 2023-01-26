# PMPs with Hu ------------------------------
#Compute aggregated Posterior Model Probabilities (PMPs) from the simulated Bayes Factors
#restricted to hypoteses H1, Hc and Hu, excluding H0

#load the simulated BFs
load("workspaces/Simulate BFs_ES(H1)=ES(Hc).RData")

dimnames(BF)
#BF[studies, hypotheses, ratioHi:Hc, iter, n]

#subset only hypotheses Hi vs. Hc vs. Hu (exlude H0)
BF.noH0<-BF[,2:4,,,]  
n.hyp<-3 #number of hypotheses
aggrPMP<-BF.noH0  #placeholder

s<-1
i<-1
t<-1
h<-1
r<-1

for(s in 1:length(n)){  #for each sample size s
  for(i in 1:iter){       # for each iteration t
    for(t in 1:studies){    # for each study t
      for(h in 1:n.hyp){  #for each hypothesis h
        for(r in 1:2){ #for each ratio Hi:Hc
          #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
          aggrPMP[t,h,r,i,s]<-prod(BF.noH0[1:t,h,r,i,s])/sum(apply(matrix(BF.noH0[1:t,,r,i,s],nrow = t,ncol = n.hyp),2,prod))
          
        }
      } 
    }
  }
}

dimnames(aggrPMP)[[2]]<-c("PMP_i","PMP_c","PMP_u")


aggrPMP1.withHu<-aggrPMP[,,1,,] #Hi:Hc=1:1; H1 vs. Hc vs. Hu
aggrPMP2.withHu<-aggrPMP[,,2,,] #Hi:Hc=3:1; H1 vs. Hc vs. Hu

#remove unnecessary variables
remove(BF,BF.noH0,aggrPMP,i,s,t,h,r,n.hyp,run.sim,gen_dat,data_and_model)

#save results
save.image(file="workspaces/Aggregate PMPs_H1 vs. Hc vs. Hu.RData")



