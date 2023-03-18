#the script creates the file compl_power_BES and merges it with with power_BES

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
iterations <- 5*1000*3*40 # total number of conditions - not sure if this actually matters
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
#end progress bar options

## foreach loops ----------------------------
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
      
    } #end foreach t loop (studies) (rows)
  
  #      }# end foreach p loop (heterogenetity) (3rd dim)
  
  #    }# end i loop (iterations) (4th dim)
  
  #  }# end n loop (sample size) (list)
  
)#system.time
#time: 7cl - about 1h 15 min
close(pb)
stopCluster(cl) 

#save(compl_power_BES, file = "Outputs/accuracy/simulated files/compl_power_BES.RData")

length(compl_power_BES)

compl_power_BES[[1]]$BF


# Post - processing --------------------------------------------------------
load("Outputs/accuracy/simulated files/compl_power_BES.RData")
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

#save(compl_power_BES, file = "Outputs/accuracy/simulated files/compl_power_BES_processed.RData")


# Merging ------------------------------------------------------

#load files
load("Outputs/accuracy/simulated files/power_BES.RData")
load("Outputs/accuracy/simulated files/compl_power_BES_processed.RData")
## Files structure ------------------------------------------------
#the data files power_BES and compl_power_BES have a similar structure

# *elemens present only in compl_power_BES
names(power_BES[[1]])
names(compl_power_BES[[1]])
#NOT code - just to provide the files stucture
# power_BES[[n]]list(BF [t, BF, population(H0,H1, Hc, Hu:H1+Hc), iter],
#                    ###unique to compl_ ###
#                    sampled_betas* [t, beta, heterogeneity (propSD), iter],
#                    est_betas*,
#                    est_SE*,
#                    betas*,
#                    propSD=c(0.1, 0.3, 0.5),
#                    
#                    ####common###
#                    r2=0.13,
#                    pcor=0.3,
#                    n,
#                    hypothesis="V1>V2>V3",
#                    populations
#                    model="linear",
#                    iter=1000,
#                    studies=40
#                    )

## Combine files ----------------------------------------------------------
#combine power_BES and compl_power_bes

#subset only n that is present in compl_
c1<-power_BES[1:5]
#remove BF0u
c1<-lapply(c1, function(x) x$BF[,-1,,])
c2<-lapply(compl_power_BES, function(x) x$BF)
names(c1)<-names(c2)


abind_3<-function(...){
  abind(..., along = 3)
}
#merge the BF arrays of the old and new file along the 3rd dimension
BFdat<-Map(abind_3,c1,c2)
#check
BFdat$n50 %>% dimnames()

for(i in 1:length(n)){
  BFdat[[i]]<-list.append(BFdat[[i]],
                          r2=0.13,
                          pcor=0.3,
                          betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),
                          populations=list(
                            H1=c(3,2,1), #population H1 = TRUE
                            Hc=c(1,2,3),
                            Hu=paste(paste("50%", c("H1", "Hc")), collapse = " & "),
                            Hu=c(3,2,1, "+heterogeneity of degree propSD")
                          ),
                          hypothesis="V1>V2>V3",
                          model="linear",
                          propSD=c(0.1, 0.3, 0.5),
                          iter=1000,
                          studies=40,
                          n=n[i]
  )
}

save(BFdat, file = "Outputs/accuracy/simulated files/merged_datBF.RData")
