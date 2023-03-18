# create and assess parallelized code

library(foreach)
library(doParallel)
library(abind)

source("scripts/ThomVolker scripts/functions.R")
source("scripts/single_sim().R")

# parallel iter ----------------------------------------------------------

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
iterations <- 5*1000*3*40 # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
#end progress bar options

system.time(
  
  compl_power_BES <- 
    
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
        foreach(p = c(0.1, 0.3, 0.5), #heterogeneity levels from low to high
                .combine = map_abind_3 #bind along the 3rd dimension
        ) %:% #{
          #t-loop: studies
          foreach(t=1:40,
                  .combine=map_rbind
          ) %dopar% {
            
            #print(paste("Sample size:", n, "; Iter:", i))
            
            sim<-single_sim(r2=0.13,  # R-squared of the regression model
                            pcor=0.3,  # correlation between the predictors
                            betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
                            # Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                            propSD = p,
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
          
        }#end foreach t loop (studies) (rows)
        
#      }# end foreach p loop (heterogenetity) (3rd dim)
        
#    }# end i loop (iterations) (4th dim)
  
#  }# end n loop (sample size) (list)
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

#save(compl_power_BES, file = "Outputs/compl_power_BES.RData")

length(compl_power_BES)

compl_power_BES[[1]]$BF

## Post - processing ------------------------------------
load("Outputs/compl_power_BES.RData")
n = c(50,100,150,200,300)
p = c(0.1, 0.3, 0.5)

### add simulation conditions ----------------------------------
library(rlist)
library(purrr)
for(i in 1:length(n)){
    compl_power_BES[[i]]<-list.append(compl_power_BES[[i]],
                                      r2=0.13,
                                      pcor=0.3,
                                      betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),
                                      populations=list(Hu=c(3,2,1, "+heterogeneity of degree propSD")
                                                       ),
                                      hypothesis="V1>V2>V3",
                                      model="linear",
                                      propSD=c(0.1, 0.3, 0.5),
                                      iter=1000,
                                      studies=40,
                                      n=n[i]
                                      )
}


### add names/labels ------------------------------------
#add names to the lists for the sample sizes
names(compl_power_BES)[1:length(n)]<-paste0("n",n)

#add dimnames to the arrays

## change the dimnames of the array with BFs
compl_power_BES[1:5]<-lapply(compl_power_BES[1:5], function(x){
        dimnames(x$BF)<-list(1:40,
                             c("BFiu", "BFcu", "BFuu"),
                             paste0("HETEROG_H1p",substr(p,2,3)),
                             1:1000
                            )
        return(x)
    }
  )
#check
compl_power_BES$n100$BF

#change the dimnames of the arrays sampled_betas, est_betas, and est_SE
for(i in 1:length(n)){
  for(j in 2:4){ # arrays with sampled_betas, est_betas, and est_SE
    dimnames(compl_power_BES[[i]][[j]])<-list(1:40,
                                              c("b1", "b2", "b3"),
                                              paste0("HETEROG_H1p",substr(p,2,3)),
                                              1:1000
    )
  }
}

#check
compl_power_BES$n50$sampled_betas

save(compl_power_BES, file = "Outputs/compl_power_BES_r.RData")


