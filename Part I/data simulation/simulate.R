library(bain)
library(parallel)
library(foreach)
library(doParallel)
library(doRNG)
library(doSNOW)
library(abind)

source("Part I/data simulation/functions.R")

# custom functions to combine the results from the parallelized for-loops 
#add a 3rd dimension to a matrix 
abind_3<-function(...){
  abind(..., along = 3)
}
#add a 4th dimension to a matrix 
abind_4<-function(...){
  abind(..., along = 4)
}

# test ---------------------------------------------------------------------------------------
#run the same small scale simulation to confirm the structure and check the reproducibility

##test1--------------------------------------------
#Simulation conditions
n = c(50,100)
studies<-4 #studies
iterations=10
iter<-studies*iterations # 10 iterations for each of the 4 studies
hypothesis=c("V1>V2>V3")
s<-1



cl <- makeCluster(3)
registerDoSNOW(cl)

# #setting seed

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
  
  test1 <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            .combine=abind_3,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% 
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            .combine = rbind,
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      BF<-single_sim(r2=0.13,  # R-squared of the regression model
                     pcor=0.3,  # correlation between the predictors
                     ratio_beta=c(3,2,1),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                     p = 0,
                     n=n[s],  #sample size
                     model="linear"  #linear, logistic or probit regression
      )$m %>% 
        bain(hypothesis = hypothesis)%$%fit %>% 
        extract(c(1, nrow(.)),c("BF.u")) %>% 
        setNames(c("H1", "Hc"))
      
      
    } 
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 


## test 2 ----------------------------
cl <- makeCluster(3)
registerDoSNOW(cl)

# #setting seed

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
  
  test2 <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            .combine=abind_3,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% 
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            .combine = rbind,
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      BF<-single_sim(r2=0.13,  # R-squared of the regression model
                     pcor=0.3,  # correlation between the predictors
                     ratio_beta=c(3,2,1),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                     p = 0,
                     n=n[s],  #sample size
                     model="linear"  #linear, logistic or probit regression
      )$m %>% 
        bain(hypothesis = hypothesis)%$%fit %>% 
        extract(c(1, nrow(.)),c("BF.u")) %>% 
        setNames(c("H1", "Hc"))
      
      
    } 
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

#the results replicate!
test1==test2

# H1_r.13_pcor.3_b321_p0_linear ---------------------------------------------------------------------------------

#Simulation conditions
n = c(50,75,100,150,200,300)
studies<-30 #studies
iterations=1000
iter<-studies*iterations # 10 iterations for each of the 4 studies
hypothesis=c("V1>V2>V3")
s<-1

cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed

rng <- RNGseq(length(n)* iter, 123)
#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loops
## foreach loops
system.time(
  
  H1_r.13_pcor.3_b321_p0_linear <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            .combine=abind_3,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% 
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            .combine = rbind,
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      BF<-single_sim(r2=0.13,  # R-squared of the regression model
                    pcor=0.3,  # correlation between the predictors
                    ratio_beta=c(3,2,1),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                    p = 0,
                    n=n[s],  #sample size
                    model="linear"  #linear, logistic or probit regression
      )$m %>% 
            bain(hypothesis = hypothesis)%$%fit %>% 
            extract(c(1, nrow(.)),c("BF.u")) %>% 
            setNames(c("H1", "Hc")) %>% 
            c(.,"BFuu"=1)
      

    } 
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 
saveRDS("Part I/data simulation/simulation files/H1_r.13_pcor.3_b321_p0_linear.rds")

# H1_r.13_pcor.3_b321_p.50_linear ---------------------------------------------------------------------------------

#Simulation conditions
n = c(50,75,100,150,200,300)
studies<-30 #studies
iterations=1000
iter<-studies*iterations # 10 iterations for each of the 4 studies
hypothesis=c("V1>V2>V3")
s<-1

cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed

rng <- RNGseq(length(n)* iter, 456)
#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loops
## foreach loops
system.time(
  
  H1_r.13_pcor.3_b321_p.50_linear <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            .combine=abind_3,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% 
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            .combine = rbind,
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      BF<-single_sim(r2=0.13,  # R-squared of the regression model
                     pcor=0.3,  # correlation between the predictors
                     ratio_beta=c(3,2,1),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                     p = 0.68,
                     n=n[s],  #sample size
                     model="linear"  #linear, logistic or probit regression
      )$m %>% 
        bain(hypothesis = hypothesis)%$%fit %>% 
        extract(c(1, nrow(.)),c("BF.u")) %>% 
        setNames(c("H1", "Hc")) %>% 
        c(.,"BFuu"=1)
      
      
    } 
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

saveRDS("Part I/data simulation/simulation files/H1_r.13_pcor.3_b321_p.50_linear.rds")

# H1_r.13_pcor.3_b321_p.68_linear ---------------------------------------------------------------------------------

#Simulation conditions
n = c(50,75,100,150,200,300)
studies<-30 #studies
iterations=1000
iter<-studies*iterations # 10 iterations for each of the 4 studies
hypothesis=c("V1>V2>V3")
s<-1

cl <- makeCluster(7)
registerDoSNOW(cl)

# #setting seed

rng <- RNGseq(length(n)* iter, 456)
#add progress bar
iterations <- length(n)*iter # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
#end progress bar options

# run standard nested foreach loops
## foreach loops
system.time(
  
  H1_r.13_pcor.3_b321_p.68_linear <- 
    
    #n-loop: sample size
    foreach(s = 1:length(n), #,
            .combine=abind_3,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% 
    
    #i-loop: iterations
    foreach(i = 1:iter, #iterations
            r=rng[(s-1)*iter + 1:iter],
            .combine = rbind,
            .packages = c("tidyverse","magrittr", "furrr", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    )%dopar% {
      
      # set RNG seed
      rngtools::setRNG(r)
      
      BF<-single_sim(r2=0.13,  # R-squared of the regression model
                     pcor=0.3,  # correlation between the predictors
                     ratio_beta=c(3,2,1),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                     p = 0.68,
                     n=n[s],  #sample size
                     model="linear"  #linear, logistic or probit regression
      )$m %>% 
        bain(hypothesis = hypothesis)%$%fit %>% 
        extract(c(1, nrow(.)),c("BF.u")) %>% 
        setNames(c("H1", "Hc")) %>% 
        c(.,"BFuu"=1)
      
      
    } 
  
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

saveRDS("Part I/data simulation/simulation files/H1_r.13_pcor.3_b321_p.68_linear.rds")