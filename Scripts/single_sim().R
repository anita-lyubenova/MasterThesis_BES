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
ratio_beta<-c(16,4,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


bcor<-0 #correlation between the coefficients
sigma_beta<-diag(x=0.10*betas, nrow = length(betas),ncol = length(betas)) #covariance matrix of the coefficients

#How often is b1>b2>b3 for SD being the specified percentage of the coefficients
percentage=c(seq(0.01, 0.1, by=0.01),seq(0.1, 0.5, by=0.1))
iter=10000
a<-data.frame(percentage=percentage,
              prop_correct=NA
              )
for(p in 1:length(percentage)){

    sigma_beta<-diag(x=percentage[p]*betas, nrow = length(betas),ncol = length(betas)) #covariance matrix of the coefficients
    b<-mvrnorm(n=iter, mu=betas, Sigma = sigma_beta)
    a$prop_correct[p]<- sum(b[,1]>b[,2] & b[,2]>b[,3])/iter
}
a
#Conclusion: 
#(1) If the true coefficients are sampled (instead of fixed betas), even with very small 
#    SDs of the MVnorm, the sampled coefficients will not be in line with the informative hypothesis 10-20-30-40% of the time
#    ==> It will be hard/not possible to have heterogeneity of the effects and have H1 to be true for all studies...

# (2) The more parsimonious the hypothesis, the more prone to rejection it is in case there is heterogeneity


#######################################################################
0.3^2
