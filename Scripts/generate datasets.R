
library(foreach)
library(doParallel)
library(abind)

#source("scripts/ThomVolker scripts/functions.R")
source("scripts/generate_datasets().R")

# #Conditions:
# n = c(25,50,100,150,200,250,300,350,500, 800)
# iterations = 1:40000 # 1000 iter x 40 studies#
# p = c(0.1, 0.3, 0.5, 0.70, 1)


#Generate TRUE_H1_medium---------------------------------------------------------------------------
#parallel x iter x n

#resulting data:
# list[[n]] $BF[t, BF, heterog, iter]
#           $sampled_betas[t, beta_index, heterog, iter] #here are the true betas (that were sampled in each iteration)
#           $est_betas[t, beta_index, heterog, iter] #betas estimated from lm()
#           $est_SE[t, beta_index, heterog, iter] #standard errors of the estimated betas from lm()



#registerDoParallel(7)  # use multicore, set to the number of cores


library(doSNOW)
library(doRNG)

cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed
n <-  c(25,100,200,250,350,500, 800)
iter<-40000
rng <- RNGseq(length(n)* iter, 123)

#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loo
## foreach loops
system.time(
  
  TRUE_H1 <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
       #     .combine=list,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% #{
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            #.combine = list, #bind along the 4th dimension
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      generate_dataset(r2=0.13,  # R-squared of the regression model
                            pcor=0.3,  # correlation between the predictors
                            betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                            propSD = 0,
                            n=n[s],  #sample size 
                            model="linear"  #linear, logistic or probit regression
      )$d
      
      # #save the results in a list
      # res<-list(d=sim$BF,
      #           sampled_betas=sim$sampled_betas
      # )
      
    } #end foreach t loop (studies) (rows)

  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

save(TRUE_H1,file="Outputs/generate datasets/TRUE_H1.RData")
TRUE_H1[[1]][[1]][1,] == TRUE_H1_repr[[1]][[1]][1,]
#Generate TRUE_Hc---------------------------------------------------------------------------

cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed
n <-  c(25,100,200,250,350,500, 800)
iter<-40000
rng <- RNGseq(length(n)* iter, 123)

#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loo
## foreach loops
system.time(
  
  TRUE_Hc <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            #     .combine=list,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% #{
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            #.combine = list, #bind along the 4th dimension
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      generate_dataset(r2=0.13,  # R-squared of the regression model
                       pcor=0.3,  # correlation between the predictors
                       betas=coefs(0.13, c(1,2,3), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                       propSD = 0,
                       n=n[s],  #sample size 
                       model="linear"  #linear, logistic or probit regression
      )$d
      
      # #save the results in a list
      # res<-list(d=sim$BF,
      #           sampled_betas=sim$sampled_betas
      # )
      
    } #end foreach t loop (studies) (rows)
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

save(TRUE_Hc,file="Outputs/generate datasets/TRUE_Hc.RData")

#Generate TRUE_H0---------------------------------------------------------------------------

cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed
n <-  c(25,100,200,250,350,500, 800)
iter<-40000
rng <- RNGseq(length(n)* iter, 123)

betas<-coefs(0.13, c(1,1,1), cormat(0.3, 3), "normal")

#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loo
## foreach loops
system.time(
  
  TRUE_H0 <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            #     .combine=list,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% #{
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            #.combine = list, #bind along the 4th dimension
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      generate_dataset(r2=0.13,  # R-squared of the regression model
                       pcor=0.3,  # correlation between the predictors
                       betas=betas,  # a numeric vector with the beta coefficients;  defines the truth in the population;
                       propSD = 0,
                       n=n[s],  #sample size 
                       model="linear"  #linear, logistic or probit regression
      )$d
      
      # #save the results in a list
      # res<-list(d=sim$BF,
      #           sampled_betas=sim$sampled_betas
      # )
      
    } #end foreach t loop (studies) (rows)
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) vr                             

save(TRUE_H0,file="Outputs/generate datasets/TRUE_H0.RData")


# test ---------------------------------------------------------
cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed
n <-  c(25,100)
studies<-4 #studies
iter<-studies*10 # 10 iterations for each of the 4 studies
rng <- RNGseq(length(n)* iter, 123)

#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loo
## foreach loops
system.time(
  
  test <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            #     .combine=list,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% #{
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            #.combine = list, #bind along the 4th dimension
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      generate_dataset(r2=0.13,  # R-squared of the regression model
                       pcor=0.3,  # correlation between the predictors
                       betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                       propSD = 0,
                       n=n[s],  #sample size 
                       model="linear"  #linear, logistic or probit regression
      )$d
      
      # #save the results in a list
      # res<-list(d=sim$BF,
      #           sampled_betas=sim$sampled_betas
      # )
      
    } #end foreach t loop (studies) (rows)
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 


test

#label the names of the list elements corresponding to different sample sizes n
names(test)<-paste0("n", n)

#label the names of the list elements corresponding to different study numbers t
test<-lapply(test, FUN=function(x) {
  names(x)<-paste0("set",rep(1:studies, times=10))
  return(x)
}
)

# #alternative
test<-lapply(test, FUN=function(x) {
  names(x)<-paste0("i",rep(1:10, times=studies))
  return(x)
}
)

test$n25 %>% names()

save(test,file="Outputs/generate datasets/test.RData")

