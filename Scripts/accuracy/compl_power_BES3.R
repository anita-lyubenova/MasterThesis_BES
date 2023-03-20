
library(foreach)
library(doParallel)
library(abind)

source("scripts/ThomVolker scripts/functions.R")
source("scripts/single_sim().R")


#Simulate ---------------------------------------------------------------------------
#parallel t x heterog x iter x n

#resulting data:
# list[[n]] $BF[t, BF, heterog, iter]
#           $sampled_betas[t, beta_index, heterog, iter] #here are the true betas (that were sampled in each iteration)
#           $est_betas[t, beta_index, heterog, iter] #betas estimated from lm()
#           $est_SE[t, beta_index, heterog, iter] #standard errors of the estimated betas from lm()


# custom functions to combine the results from the parallelized for-loops 
#add a 3rd dimension to a matrix 
abind_3<-function(...){
  abind(..., along = 3)
}
#add a 4th dimension to a matrix 
abind_4<-function(...){
  abind(..., along = 4)
}

#for each element of the list add a 3rd dimension with the current loop values
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



#registerDoParallel(7)  # use multicore, set to the number of cores

#add progress bar
library(doSNOW)
cl <- makeCluster(7)
registerDoSNOW(cl)
iterations <- 5*1000*4*40 # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
#end progress bar options

## foreach loops ----------------------------
system.time(
  
  compl_power_BES3 <- 
    
    #n-loop: sample size
    foreach(n = c(50,100,150,200,300), #,
            .combine=list,
            .multicombine = TRUE,
            .options.snow = opts, #add progress bar
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% #{
    
    #i-loop: iterations
    foreach(i = 1:1000, #iterations
            .combine = map_abind_4, #bind along the 4th dimension
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %:% #{
    #p-loop: heterogeneity
    foreach(b = list(c(9,3,1),
                     c(2, 1.5, 1),
                     c(1, 1.5, 2),
                     c(1, 3, 9)
                     ), #heterogeneity levels from low to high
            .combine = map_abind_3 #bind along the 3rd dimension
    ) %:% #{
    #t-loop: studies
    foreach(t=1:40,
            .combine=map_rbind
    ) %dopar% {
      
      #print(paste("Sample size:", n, "; Iter:", i))
      
      sim<-single_sim(r2=0.13,  # R-squared of the regression model
                      pcor=0.3,  # correlation between the predictors
                      betas=coefs(0.13, b, cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                      # Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                      propSD = 0,
                      hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
                      n=n,  #sample size 
                      model="linear",  #linear, logistic or probit regression
                      #  save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
      )
      
      #save the results in a list
      res<-list(BF=sim$BF,
                sampled_betas=sim$sampled_betas,
                est_betas=sim$est_betas,
                est_SE=sim$est_SE
      )
      
    } #end foreach t loop (studies) (rows)
  
  #      }# end foreach p loop (heterogenetity) (3rd dim)
  
  #    }# end i loop (iterations) (4th dim)
  
  #  }# end n loop (sample size) (list)
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

length(compl_power_BES3)
compl_power_BES3[[1]]$BF %>% dim
save(compl_power_BES, file = "Outputs/accuracy/simulated files/compl_power_BES3.RData")