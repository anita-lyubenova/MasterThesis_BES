#a function to simulate a vector of BFs according to certain simulation conditions

source("scripts/ThomVolker scripts/functions.R")

# Simulation conditions -----------------------------------------------------
# R2
# pcor : correlation between the predictors
# p : number of predictors
# hypothesis : a string with the hypotheses of interest; must be in the format of bain()
# n : sample size 
# model : "linear", #linear, logistic or probit regression

# ~~pop_ratio_beta ~~ : defines the truth in the population; the ratio beta1:beta2:beta3 as a numeric vector, where larger values indicate larger parameter value
# betas : a numeric vector with the beta coefficients;  defines the truth in the population;
# sigma_beta: variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
# iter: number of iterations
single_sim<-function(r2,  # R-squared of the regression model
                     pcor,  # correlation between the predictors
                     betas,  # a numeric vector with the beta coefficients;  defines the truth in the population;
                     Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                     hypothesis,  # the hypothesis of interest; must be in the format of bain()
                     n,  #sample size 
                     model,  #linear, logistic or probit regression
                     save_mod_coefs # should the estimated coefficients and their standard errors be saved?
){
  #n.hyp<-length(unlist(strsplit(hypothesis, ";")))
  n.hyp<-1

  #obtain BFiu
  BF<-gen_dat(r2=r2,
               betas=betas,
               rho=cormat(pcor, length(betas)),
               n=n,
               model="normal") %>% 
     lm(Y~., data=.) %>%
     bain(hypothesis = hypothesis)%$%fit %>%     #$BF.u[c(1,2,4)]
     extract(c(1:n.hyp, nrow(.)),"BF.u") #subset only BFiu for the specified hypothesis and the complement
   
}

a<-single_sim(r2=0.13,  # R-squared of the regression model
              pcor=0.3,  # correlation between the predictors
              betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
              Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
              hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
              n=100,  #sample size 
              model="linear",  #linear, logistic or probit regression
              save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
)

#Parallelization --------------------------------------------------------------
library(parallel)

## regular apply(): no parallel --------------------------------------
iter <- 1:1000
system.time({
  results <- lapply(iter, function(x){
    single_sim(r2=0.13,  # R-squared of the regression model
               pcor=0.3,  # correlation between the predictors
               betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
               Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
               hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
               n=100,  #sample size 
               model="linear",  #linear, logistic or probit regression
               save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
    )
  })
})
# System.time
# user  system elapsed 
# 36.25    0.21   37.37

## mclapply ---------------------------------------------------------
# doesn't work on windows
numCores <- detectCores()
numCores

system.time(
  results <- mclapply(iter, mc.cores = numCores, FUN=function(x){
    single_sim(r2=0.13,  # R-squared of the regression model
               pcor=0.3,  # correlation between the predictors
               betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
               Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
               hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
               n=100,  #sample size 
               model="linear",  #linear, logistic or probit regression
               save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
    )
  })
)

## foreach -----------------------------------------------
library(foreach)
library(doParallel)

system.time(
  results <- foreach (i=1:1000,
                      .combine=rbind,
                      .packages = c("tidyverse","magrittr", "furrr",
                                    "BFpack", "Rcpp", "RcppArmadillo",
                                    "MASS","mvtnorm", "bain")) %do% {
                                      single_sim(r2=0.13,  # R-squared of the regression model
                                                 pcor=0.3,  # correlation between the predictors
                                                 betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                                                 Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                                                 hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
                                                 n=100,  #sample size 
                                                 model="linear",  #linear, logistic or probit regression
                                                 save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
                                      )
                                    }
  
)
# user  system elapsed 
# 30.35    0.31   31.42 

registerDoParallel(numCores)  # use multicore, set to the number of our cores

system.time(
results <- foreach (i=1:1000,
                    .combine=rbind,
                    .packages = c("tidyverse","magrittr", "furrr",
                                  "BFpack", "Rcpp", "RcppArmadillo",
                                  "MASS","mvtnorm", "bain")) %dopar% {
      single_sim(r2=0.13,  # R-squared of the regression model
                 pcor=0.3,  # correlation between the predictors
                 betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                 Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                 hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
                 n=100,  #sample size 
                 model="linear",  #linear, logistic or probit regression
                 save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
      )
}

)
#System time
# user  system elapsed 
# 0.94    0.17   23.10 


## parallel -----------------------------------------------
# kinda hard because it does not recognize custom functions... 
library(parallel)

cl <- parallel::makeCluster(numCores)

#clusterEvalQ(cl, { library(string); library(rvest); })

iter <- 1:1000
system.time(
  results <- parLapply(cl,
                       iter ,
                       function(x){
                         single_sim(r2=0.13,  # R-squared of the regression model
                                    pcor=0.3,  # correlation between the predictors
                                    betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                                    Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                                    hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
                                    n=100,  #sample size 
                                    model="linear",  #linear, logistic or probit regression
                                    save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
                         )
                       }
  )
  
)

stopCluster(cl)















# cl <- parallel::makeCluster(2)
# parallel::stopCluster(cl)

###############################################################
r2=.13 #effect size r-squared
pcor<-0.3 #correlation between the predictor variables
n<-100 #sample sizes
hypothesis<-"V1>V2>V3" #tested hypotheses
models <- c("normal") #linear, logistic or probit regression
ratio_beta<-c(9,3,1)
betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


####################################################################
# heterogeneity of effect sizes--------------------
source("scripts/ThomVolker scripts/functions.R")
r2<-0.13
pcor<-0.3
ratio_beta<-c(9,3,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


# bcor<-0 #correlation between the coefficients
# sigma_beta<-diag(x=0.10*betas, nrow = length(betas),ncol = length(betas)) #covariance matrix of the coefficients

#How often is b1>b2>b3 for SD being the specified percentage of the coefficients
percentage=c(seq(0.01, 0.09, by=0.01),seq(0.1, 0.9, by=0.1))
iter=10000
a<-data.frame(percentage=percentage,
              prop_correct=NA,
              SDs = NA
              )
percentage[1]*betas
for(p in 1:length(percentage)){

    sigma_beta<-diag(x=(percentage[p]*betas)^2, nrow = length(betas),ncol = length(betas)) #covariance matrix of the coefficients
    #sigma_beta<-diag(x=percentage[p]*betas)#, nrow = length(betas),ncol = length(betas))
    b<-mvrnorm(n=iter, mu=betas, Sigma = sigma_beta)
    a$prop_correct[p]<- sum(b[,1]>b[,2] & b[,2]>b[,3])/iter
    a$SDs[p]<-paste(round(diag(sigma_beta), digits=3), collapse="; ")
}
a
#Conclusion: 
#(1) If the true coefficients are sampled (instead of fixed betas), even with very small 
#    SDs of the MVnorm, the sampled coefficients will not be in line with the informative hypothesis 10-20-30-40% of the time
#    ==> It will be hard/not possible to have heterogeneity of the effects and have H1 to be true for all studies...

# (2) The more parsimonious the hypothesis, the more prone to rejection it is in case there is heterogeneity


#######################################################################

