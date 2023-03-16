#this script assesses what happens when true betas are not fixed across iterations but sampled from 
# a mvnorm with various spread;

#the degree of spread is determined by the proportion of the betas that comprises the SD(propSD): SD=propSD*betas,
#where 70% spread had beed stated to be wide distribution (REF)

#attempt to relate propSD to 
#  1) the proportion of times H1 is true across iterations
#  2) the the meta-analytical I-squared

# Question: for common tau, such as 0.30, what is corresponding prop of times H1 is true on population level?
# Implication of the results: it could be that a parsimonious informative hypothesis will not be true too often on population level

source("scripts/ThomVolker scripts/functions.R")
source("scripts/single_sim().R")

# 1) P(H1==TRUE | propSD) ---------------------
#the proportion of times H1 is true in the population across iterations
r2<-0.13
pcor<-0.3
ratio_beta<-c(3,2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

#How often is b1>b2>b3 for SD being the specified percentage of the coefficients
percentage=c(seq(0.01, 0.09, by=0.01),seq(0.1, 0.9, by=0.1), 0.95, 1)
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
# Insights:
# - The more parsimonious the hypothesis, the more prone to rejection it is in case there is heterogeneity
# - propSD = 0.5*betas results in H1 not being true 50% of the time (on population level)

# 2) propSD and tau ------------------------------------

## Simulate -------------------

library(foreach)
library(doParallel)
library(abind)
# custom functions to combine the results from the parallelized for-loops 


#add a 3rd dimension to a matrix 
abind_3<-function(...){
  abind(..., along = 3)
}

# apply abind_3() element-wise for the elements of lists x and y
map_abind_3<-function(x,y){
  Map(abind_3,x,y)
}
 
#apply rbind() element-wise for the elements of lists x and y
#eg. rbind() BFs from each iteration
map_rbind<-function(x,y){
  Map(rbind,x,y)
}

registerDoParallel(7)  # use multicore, set to the number of cores

system.time(
  
  results2 <- 
    foreach(p = c(0.1, 0.3, 0.7),
            .combine = map_abind_3, #bind along the 3rd dimension
            #.multicombine =TRUE,
            .packages = c("tidyverse","magrittr", "furrr",
                          "Rcpp", "RcppArmadillo", "MASS",
                          "mvtnorm", "bain", "foreach", "doParallel")
    ) %dopar%{
      
      foreach(i=1:200,
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
              
      }#end foreach i loop
      
    }# end foreach n loop
  
)#system.time

#save.image("Outputs/investigate heterogeneity/workspace_1.RData")

#checks
names(results2)
results2$BF %>% dim()
results2$sampled_betas %>% dimnames()
results2$est_betas %>% dim()
results2$est_SE %>% dim()

#change the dimnames of the arrays sampled_betas, est_betas, and est_SE
results2[-1]<-lapply(results2[-1], function(x) {
        dimnames(x)<-list(1:200,
                          c("V1", "V2", "V3"),
                          c(0.1, 0.3, 0.7) #propSD
              )
        return(x)
      }
  )
#change the dimnames of the array with the BFs
dimnames(results2[[1]])<-list(1:200,
                              c("BFiu", "BFcu", "BFuu"),
                              c(0.1, 0.3, 0.7) #propSD
)
b1.7=results2$est_betas[,,3][,1]
SE.7=results2$est_SE[,,3][,1]

b1.3=results2$est_betas[,,2][,1]
SE.3=results2$est_SE[,,2][,1]

b1.1=results2$est_betas[,,1][,1]
SE.1=results2$est_SE[,,1][,1]

# Meta-analysis --------------------------------------------
library(metafor)

0.3*betas[1] #expected tau
ma <- rma.uni(yi=b1.3[1:50],
        sei=SE.3[1:50],
        method="REML")
ma
#estimated tau = 0.0634
#I-squared = 0.582

0.1*betas[1] #expected tau
ma.1 <- rma.uni(yi=b1.1[1:50],
              sei=SE.1[1:50],
              method="REML")
ma.1

0.7*betas[1]
ma.7 <- rma.uni(yi=b1.7[1:50],
                sei=SE.7[1:50],
                method="REML")
ma.7
# tau=0.15
# I-squared=0.90