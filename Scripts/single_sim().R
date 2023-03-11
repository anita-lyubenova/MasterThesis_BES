#a function to simulate a vector of BFs according to certain simulation conditions


# Simulation conditions -----------------------------------------------------
# R2
# pcor : correlation between the predictors
# p : number of predictors
# hypothesis : a string with the hypotheses of interest; must include all predictors
# n : sample size 
# model : "linear", #linear, logistic or probit regression

# pop_ratio_beta : defines the truth in the population; the ratio beta1:beta2:beta3 as a numeric vector, where larger values indicate larger parameter value
#




####################################################################
# heterogeneity of effect sizes--------------------
source("scripts/ThomVolker scripts/functions.R")
r2<-0.13
pcor<-0.3
ratio_beta<-c(3,2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


bcor<-0 #correlation between the coefficients
sigma_beta<-diag(x=0.09, nrow = length(betas),ncol = length(betas)) #covariance matrix of the coefficients

mvrnorm(n=1, mu=betas, Sigma = sigma_beta)


#######################################################################
0.3^2
