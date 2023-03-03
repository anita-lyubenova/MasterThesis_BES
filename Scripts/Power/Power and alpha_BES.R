source("scripts/load_packages.R")
source("scripts/ThomVolker scripts/functions.R")

# r2=0.13
# pcor=0.3
# hypothesis="V1=V2=V3; V1>V2>V3"
# ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
#                 H1=c(3,2,1), #population H1 = TRUE
#                 Hc=c(1,2,3),
#                 Hu=c(1,1,1)
# )
# model="linear"
# iter=3
# studies=40



# a function that 
# (1) simulates data based on specified populations in line with hypohteses of interest
# (2) Computes BFs for each hypohteses of interest in each population
sim_BES<-function(
    r2=0.13,#effect size r-squared
    pcor=0.3,#correlation between the predictor variables
    n, #sample size
    hypothesis, #tested hypotheses;
    ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
                   H1=c(3,2,1), #population H1 = TRUE
                   Hc=c(1,2,3),
                   Hu= c(1,1,1)
                   ), # definition of the populations, as determined by the ratio between the regression coefficients b1:b2:b3; should be a named list of numeric vectors, where each vector corresponds to the ratio of betas in hte respective hypothesis (note the order of hypotheses!); the names should only contain the subscript of the hypothesis (e.g "1" or "i" or"0")
    model="linear", #linear, logistic or probit regression
    iter=3,
    #BES arguments
    studies=40,
    ratio_HiHc=4 # every 2nd study comes from a different population
){
  

  # a list to store the population level coefficients for each hypothesis-population
  betas<-list()
  #a placeholder for the BFs
  BF.u<-array(NA,
              dim = c(studies,
                      length(ratio_beta),
                      length(ratio_beta),
                      iter),
              dimnames = list(1:studies,
                              paste0("BF", substr(names(ratio_beta),2,2), "u"),
                              paste0("TRUE_", names(ratio_beta)),
                              1:iter
                              ))
  
  for(t in 1:studies){
    
    #for each population deifned by ratio_beta
    for(b in 1:length(ratio_beta)){
      betas[[b]]<-coefs(r2, ratio_beta[[b]], cormat(pcor, length(ratio_beta[[b]])), "normal")
    
      
      if(b==4){
        #have the betas conform with Hc or with Hi
        #e.g. Hi:Hc study ratio could be 1:1 (where ratio_HiHc = 2; i.e. every 2nd study comes from Hc)
        #or 1:3, where ratio_HiHc = 4, i.e every fourth study comes from Hc)
        if(t %% ratio_HiHc == 0){ #2nd or 4th study => Hc
          betas[[4]]<-coefs(r2, ratio_beta[["Hc"]], cormat(pcor, length(ratio_beta[[4]])), "normal")
        }else{                       #else Hi
          betas[[4]]<-coefs(r2, ratio_beta[["H1"]], cormat(pcor, length(ratio_beta[[4]])), "normal")
        }
    }
      
      for(i in 1:iter){
        print(paste0("Sample size: ", n ,"; Study: ",t ,"; Population: ", b ,"; Iteration: ", i))
        #generate BFs
        BF.u[t,c(1,2,3),b,i]<-gen_dat(r2=r2,
                            betas=unlist(betas[[b]]),
                            rho=cormat(pcor, length(ratio_beta[[b]])),
                            n=n,
                            "normal")%$%
          lm(Y ~ V1 + V2 +V3) %>%
          bain(hypothesis = hypothesis)%$%
          fit %>%
          extract(c(1:length(unlist(strsplit(hypothesis, ";"))),nrow(.)),"BF.u")#subset only BFiu for the specified hypothesis and the complement
        
    }
   }
  }
  
  BF.u[,"BFuu",,]<-1
  return(list(BF=BF.u,
              r2=r2,
              pcor=pcor,
              n=n,
              hypothesis=hypothesis,
              populations=ratio_beta,
              model=model,
              iter=iter,
              studies=studies
  ))
}


#function to aggregate the PMPs for a set of specified hypotheses
#reteruns the aggregate PMPs and sim conditions in a list
aggregatePMP<-function(x, #a list created with sim_BES()
                       hyp=c("1", "c", "u") #which hypothesis are to be tested interest, note only the index
                      # iter=1000,
                      # studies=40
                       
){
  BF<-x$BF
  iter<-x$iter
  n.hyp<-length(hyp)
  n<-x$n
  studies<-x$studies
  #subset only tested hypotheses, eg Hi vs. Hc vs. Hu (exlude H0)
  BF.temp<-BF[,substr(dimnames(BF)[[2]],3,3) %in% hyp,,,drop=FALSE]  
  
  #placeholder for the aggregate PMPs
  aggrPMP<-BF.temp
  
    for(i in 1:iter){       # for each iteration t
      for(t in 1:studies){    # for each study t
        for(h in 1:n.hyp){  #for each hypothesis h
          for(r in 1:length(dimnames(BF.temp)[[3]])){ #for each population 
            #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
            aggrPMP[t,h,r,i]<-prod(BF.temp[1:t,h,r,i])/sum(apply(matrix(BF.temp[1:t,,r,i],nrow = t,ncol = n.hyp),2,prod))
            
          }
        } 
      }
    }
  
  dimnames(aggrPMP)[[2]]<-paste0("PMP_", hyp)
  
  return(list(aggrPMP=aggrPMP,
              r2=x$r2,
              pcor=x$pcor,
              n=n,
              hypotheses=paste0("H",hyp),
              populations=x$populations,
              model=x$model,
              iter=iter,
              studies=studies
              ))
}

#single sample size n=100 
x<-sim_BES(studies = 10, n=100, hypothesis = "V1=V2=V3; V1>V2>V3")
dimnames(x$BF)

# pmp_x<-aggregatePMP(x=x, hyp = c("1","c", "u"))
# pmp_x

# Simulate --------------------------------------------------------------------------------
# for a range of sample sizes
n<-c(50,100,150,200,300,500,800,1200)

power_BES<-list()
for (s in 1:length(n)){
  power_BES[[s]]<-sim_BES(r2=0.13,#effect size r-squared
                          pcor=0.3,#correlation between the predictor variables
                          n=n[s], #sample size
                          hypothesis = "V1=V2=V3; V1>V2>V3", #tested hypotheses;
                          ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
                                          H1=c(3,2,1), #population H1 = TRUE
                                          Hc=c(1,2,3),
                                          Hu= c(1,1,1) #the values do not matter - it will be either Hi or Hc
                          ), # definition of the populations, as determined by the ratio between the regression coefficients b1:b2:b3; should be a named list of numeric vectors, where each vector corresponds to the ratio of betas in hte respective hypothesis (note the order of hypotheses!); the names should only contain the subscript of the hypothesis (e.g "1" or "i" or"0")
                          model="linear", #linear, logistic or probit regression
                          iter=1000,
                          #BES arguments
                          studies=40,
                          ratio_HiHc=2 # every 2nd study comes from Hc, the others from Hi
  )
}

save(power_BES, file = "Outputs/power_BES.RData")

# Aggregate --------------------------------------------------------------------------------

load("Outputs/power_BES.RData")

power_BES[[1]]

PMP<-list()
for(s in 1:length(n)){
  PMP[[s]]<-aggregatePMP(power_BES[[s]])
}


#Confusion matrix: aggregated -------------------------------------------
#PMP[[1]]$aggrPMP[studies, hyp, pop, iter]
s<-1 #sample size
t<-5 #number of aggregated studies

a<-PMP[[s]]$aggrPMP
dimnames(PMP[[1]]$aggrPMP)

a[5,,,]

a[t,,,] %>% dimnames()
#reorder the dimentsions of the PMPS, such that [iter, hyp, pop]
PMPtrans <- aperm(a[t,,,], c(3,1,2))


#create an array with the same dimensions as PMP but that will indicate only the highest PMPs
max.PMP<-PMPtrans
#produce power matrix
for(i in 1:dim(PMPtrans)[[1]]){ #for each iteration
  for(z in 1:dim(PMPtrans)[[3]]){ # for each population
    
    #get the column index of the hypothesis with the highest PMPs
    max.index<-which(PMPtrans[i,,z]==max(PMPtrans[i,,z]))
    # replace the max PMPs with 1 and the remaining PMPs with 0 for iteration i and population z
    max.PMP[i,max.index,z]<-1
    max.PMP[i,-max.index,z]<-0
  }  
}

conf_matrix<-apply(max.PMP, c(2,3), sum)/dim(max.PMP)[1]


power_alpha<-matrix(NA,
                    nrow=nrow(conf_matrix),
                    ncol = 4,
                    dimnames = list(rownames(conf_matrix),
                                    c("n","t","power", "alpha")
                    )
) %>% data.frame()

power_alpha$n<-n[s]
power_alpha$t<-t

ro<-rownames(conf_matrix)
co<-colnames(conf_matrix)


hyp_index<-c("1","c", "u")
h<-1
for(h in 1:length(hyp_index)){
  power_alpha$power[h]<-conf_matrix[substr(ro,nchar(ro), nchar(ro)) %in% hyp_index[h], substr(co,nchar(co), nchar(co)) %in% hyp_index[h]]
  power_alpha$alpha[h]<-sum(conf_matrix[substr(ro,nchar(ro), nchar(ro)) %in% hyp_index[h],!substr(co,nchar(co), nchar(co)) %in% hyp_index[h]])/(ncol(conf_matrix)-1) #divided by 2 because there are two populations under which alpha is assessed
}

#Confusion matrix: indiviudual ------------------------------------------
s<-1 #sample size

power_BES[[s]]$BF %>% dimnames()
power_BES[[s]]$BF %>% dim()

#transform the array such that studies and iterations are in a single dimension 40x1000 = 40 000
BF_ind<-apply(power_BES[[s]]$BF, c(2,3), abind::abind)

dim(bapply)


#compute PMPi = BFi/sum(BF of all hypotheses of interest)
PMP_ind<-BF_ind
for(i in 1:dim(BF_ind)[[1]]){ #for each iteration
  for(j in 1:dim(BF_ind)[[2]]){ #for each tested hypothesis
    for(z in 1:dim(BF_ind)[[3]]){ # for each population
      
      PMP_ind[i,j,z] <- BF_ind[i,j,z]/sum(BF_ind[i,,z])
      
    }  
  }
}

dimnames(PMP_ind)[[2]]<-paste0("PMP_", substr(dimnames(BF_ind)[[2]],3,3))




