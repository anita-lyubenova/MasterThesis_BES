
# effect of sample sie on sample mean and sample standard deviation -------------------------------------------------
means.small<-c()
means.large<-c()

sd.small<-c()
sd.large<-c()

for(i in 1:1000){
  small<-rnorm(25, 0, 1)
  large<-rnorm(250,0,1)
  
  means.small[i]<-mean(small)
  means.large[i]<-mean(large)
  
  sd.small[i]<-sd(small)
  sd.large[i]<-sd(large)
  
}


sum(
sd.small>sd.large)

plot(sd.small)
plot(sd.large)

plot(means.small)
plot(means.large)


# effect of small sample size on BF --------------------------

library(bain)






#exact replications, different N, same effect size
library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
library(devtools)
#  devtools::build("DataCpp")
#  devtools::install("DataCpp")
# library(DataCpp)


 
 ## Number of simulations 
 nsim <- 1000
 
 ## Sample sizes
 n <- 25 * 2^{0:5}
 
 ## Models
 models <- c("normal", "logit", "probit")
 
 ## r2 of the regression model
 r2 <- c(.02, .09, .25)
 
 ## Specify relative importance of the regression coefficients
 ratio_beta <- c(0, 1, 1, 1, 2, 3)
 
 ## Specify the bivariate correlations between predictors
 pcor <- c(0.3)
 
 ################################################################################
 ## Test model specifications
 ################################################################################
 
 gen_dat(0.02, 
         coefs(0.02, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
         cormat(pcor, length(ratio_beta)),
         100000,
         "normal") %$%
   lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
   summary()
 
 
 
 gen_dat(0.09, 
         coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "logit"),
         cormat(pcor, length(ratio_beta)),
         100000,
         "logit") %$%
   glm(Y ~ V1 + V2 + V3 + V4 + V5 + V6, family = binomial(link = "logit")) %T>%
   {performance::r2_mckelvey(.) %>% print()} %>%
   summary()
 
 coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "logit")
 