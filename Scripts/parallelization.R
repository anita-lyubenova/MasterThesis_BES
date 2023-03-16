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


# parallel t x heterog x iter x n----------------------------------------------------------
#resulting data:
#list[[n]][t, BF, heterog, iter]

library(abind)

# custom functions to combine the results from the parallelized for-loops 
#add a 3rd dimension to a matrix 
abind_3<-function(...){
  abind(..., along = 3)
}
#add a 4th dimension to a matrix 
abind_4<-function(...){
  abind(..., along = 4)
}

# apply abind_3() element-wise for the elements of lists x and y
map_abind_3<-function(x,y){
  Map(abind_3,x,y)
}
# apply abind_4() element-wise for the elements of lists x and y
map_abind_4<-function(x,y){
  Map(abind_4,x,y)
}

#apply rbind() element-wise for the elements of lists x and y
#eg. rbind() BFs from each iteration
map_rbind<-function(x,y){
  Map(rbind,x,y)
}

registerDoParallel(3)  # use multicore, set to the number of cores

system.time(
  
  compl_power_BES <- 
    
    #n-loop: sample size
    foreach(n = c(50,100,150,200,300), 
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %dopar%{
    
      #i-loop: iterations
      foreach(i = 1:1000, #iterations
              .combine = map_abind_4, #bind along the 3rd dimension
              .packages = c("tidyverse","magrittr", "furrr",
                            "Rcpp", "RcppArmadillo", "MASS",
                            "mvtnorm", "bain", "foreach", "doParallel")
      ) %dopar%{
        #p-loop: heterogeneity
        foreach(p = c(0.1, 0.3, 0.5), #heterogeneity levels from low to high
                .combine = map_abind_3, #bind along the 3rd dimension
                .packages = c("tidyverse","magrittr", "furrr",
                              "Rcpp", "RcppArmadillo", "MASS",
                              "mvtnorm", "bain", "foreach", "doParallel")
        ) %dopar%{
          #t-loop: studies
          foreach(t=1:40,
                  .combine=map_rbind,
                  .packages = c("tidyverse","magrittr", "furrr",
                                "Rcpp", "RcppArmadillo", "MASS",
                                "mvtnorm", "bain")
          ) %dopar% {
            
            #print(paste("Sample size:", n, "; Iter:", i))
            
            sim<-single_sim(r2=0.13,  # R-squared of the regression model
                            pcor=0.3,  # correlation between the predictors
                            betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                            # Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                            propSD = p,
                            hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
                            n=350,  #sample size 
                            model="linear",  #linear, logistic or probit regression
                            #  save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
            )
            
            #save the results in a list
            res<-list(BF=sim$BF,
                      sampled_betas=sim$sampled_betas,
                      est_betas=sim$est_betas,
                      est_SE=sim$est_SE
            )
          
        }#end foreach t loop (studies) (rows)
        
      }# end foreach p loop (heterogenetity) (3rd dim)
        
    }# end i loop (iterations) (4th dim)
  
  }# end n loop (sample size) (list)
  
)#system.time

#Parallelized code is 4 times quicker!!
# user  system elapsed 
# 0.10    0.03   65.39 
269/65





# cl <- parallel::makeCluster(2)
# parallel::stopCluster(cl)
