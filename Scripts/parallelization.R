# create and assess parallelized code

source("scripts/ThomVolker scripts/functions.R")
source("scripts/single_sim().R")

# parallel iter ----------------------------------------------------------

library(foreach)
library(doParallel)

registerDoParallel(numCores)  # use multicore, set to the number of our cores
system.time(
  results <- foreach (i=1:1000,
                      .combine=rbind,
                      .packages = c("tidyverse","magrittr", "furrr",
                                    "Rcpp", "RcppArmadillo", "MASS",
                                    "mvtnorm", "bain")
  ) %dopar% {
    single_sim(r2=0.13,  # R-squared of the regression model
               pcor=0.3,  # correlation between the predictors
               betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
               # Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
               propSD = 0.5,
               hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
               n=100,  #sample size 
               model="linear",  #linear, logistic or probit regression
               save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
    )
  }
)
#System time: iter=1000
# user  system elapsed 
# 0.94    0.17   23.10 

#Parallelization reduces more the elapsed time as the iterations increase
#System time: iter=1000
# user  system elapsed 
# 3.66    0.88   56.27 


# parallel iter x n----------------------------------------------------------
library(abind)

#these functions are needed to combine the results across the forloops on along the desired dimension
#combine the matrices along the third dimension,..
abind_3<-function(...){
  abind(..., along = 3)
}
#..and the 4th dimension, etc..
abind_4<-function(...){
  abind(..., along = 4)
}

registerDoParallel(3)  # use multicore, set to the number of cores

system.time(
  
  results <- 
    foreach(n = c(25, 50),
            .combine = abind_3, #bind along the 3rd dimension
            #.multicombine =TRUE,
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %dopar%{
      
      foreach(i=1:500,
              .combine=rbind,
              .packages = c("tidyverse","magrittr", "furrr",
                            "Rcpp", "RcppArmadillo", "MASS",
                            "mvtnorm", "bain")
      ) %dopar% {
        
        print(paste("Sample size:", n, "; Iter:", i))
        
        single_sim(r2=0.13,  # R-squared of the regression model
                   pcor=0.3,  # correlation between the predictors
                   betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                   Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                   hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
                   n=n,  #sample size 
                   model="linear",  #linear, logistic or probit regression
                   save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
        )
        
      }#end foreach i loop
      
    }# end foreach n loop
  
)#system.time

#Parallelized code is 4 times quicker!!
# user  system elapsed 
# 0.10    0.03   65.39 
269/65





# cl <- parallel::makeCluster(2)
# parallel::stopCluster(cl)
