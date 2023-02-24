#Try to quantify the true positives and true negatives in hpyothesis evaluation with BF
source("scripts/ThomVolker scripts/functions.R")

# a function that 
# (1) simulates data based on specified populations in line with hypohteses of interest
# (2) Computes BFs for each hypohteses of interest in each population
bain_power<-function(
         r2=0.13,#effect size r-squared
         pcor=0.3,#correlation between the predictor variables
         n, #sample size
         hypotheses, #tested hypotheses;
         ratio_beta, # ratio between the regression coefficients b1:b2:b3; should be a named list of numeric vectors, where each vector corresponds to the ratio of betas in hte respective hypothesis (note the order of hypotheses!)
         model="linear", #linear, logistic or probit regression
         iter=1000
){
  
  # BF.u<-matrix(NA,nrow = iter, ncol = length(ratio_beta), 
  #            dimnames = list(1:iter, 
  #                            names(ratio_beta)
  #                            ))
  
  BF.u<-array(NA,
              dim = c(iter, length(ratio_beta), length(ratio_beta)),
              dimnames = list(1:iter, paste0("emp_", names(ratio_beta)), paste0("TRUE_", names(ratio_beta))))
  
  for(b in 1:length(ratio_beta)){
    print(paste("Population", b ))
    for(i in 1:iter){
      print(paste("Population", b ,"; Iteration:", i))
      BF.u[i,,b]<-gen_dat(r2=r2,
                        betas=coefs(r2, ratio_beta[[b]], cormat(pcor, length(ratio_beta)), "normal"),
                        rho=cormat(pcor, length(ratio_beta)),
                        n=n,
                        "normal")%$%
        lm(Y ~ V1 + V2 +V3) %>%
        bain(hypothesis = hypotheses)%$%fit %>%
        extract(rownames(.) %in% names(ratio_beta),"BF.u")
    }
  }
  
  return(BF.u)
}
hypotheses="V1=V2=V3; V1>V2>V3"
n<-seq(100,1000, by=100)

power_linear<-list()
for(s in 1:length(n)){
  power_linear[[s]] <-bain_power(n=n[s],
                  hypotheses="V1=V2=V3; V1>V2>V3",
                  ratio_beta=list(H1=c(1,1,1),
                                  H2=c(3,2,1),
                                  Hc=c(1,2,3)),
                  iter=1000,
                  model="linear"
  )
}

power_logistic<-list()
for(s in 1:length(n)){
  power_logistic[[s]] <-bain_power(n=n[s],
                                 hypotheses="V1=V2=V3; V1>V2>V3",
                                 ratio_beta=list(H1=c(1,1,1),
                                                 H2=c(3,2,1),
                                                 Hc=c(1,2,3)),
                                 iter=1000,
                                 model="logistic"
  )
}

power_probit<-list()
for(s in 1:length(n)){
  power_probit[[s]] <-bain_power(n=n[s],
                                   hypotheses="V1=V2=V3; V1>V2>V3",
                                   ratio_beta=list(H1=c(1,1,1),
                                                   H2=c(3,2,1),
                                                   Hc=c(1,2,3)),
                                   iter=1000,
                                   model="probit"
  )
}

save.image(file="Outputs/workspace_Confusion matrix.RData")
