# Load packages -------------------------------------------------------------------
source("scripts/load_packages.R")
library(plotly)
#source("scripts/ThomVolker scripts/functions.R")

r2=0.13
pcor=0.3
hypothesis="V1=V2=V3; V1>V2>V3"
ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
                H1=c(3,2,1), #population H1 = TRUE
                Hc=c(1,2,3),
                Hu=c(1,1,1)
)
model="linear"
iter=3
studies=40



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


#Simulate --------------------------------------------------------------------------------
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

#Shape of the data
#list[[n]]($BF[t, BF, heterog, i],
#          $simulation conditions)

#save(power_BES, file = "Outputs/accuracy/simulated files/power_BES.RData")

#Number of conditions:n=8 x ratio = 4, x studies=40 x iter=1000 = 1 280 000 BFs
8*4*40*1000
# this simulation broke the fan of my laptop...

#Load power_BES  -----------------------
load("Outputs/accuracy/simulated files/power_BES.RData")