#this script assesses what happens when true betas are not fixed across iterations but sampled from 
# a mvnorm with various spread;

#the degree of spread is determined by the proportion of the betas that comprises the SD(propSD): SD=propSD*betas,
#where 70% spread had beed stated to be wide distribution (REF)

#attempt to relate propSD to 
#  1) the proportion of times H1 is true across iterations
#  2) the the meta-analytical tau

# Question: for common tau, such as 0.30, what is corresponding prop of times H1 is true on population level?
# Implication of the results: it could be that a parsimonious informative hypothesis will not be true too often on population level

source("scripts/ThomVolker scripts/functions.R")
source("scripts/single_sim().R")

# heterogeneity of effect sizes--------------------
r2<-0.13
pcor<-0.3
ratio_beta<-c(3,2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


# bcor<-0 #correlation between the coefficients
# sigma_beta<-diag(x=0.10*betas, nrow = length(betas),ncol = length(betas)) #covariance matrix of the coefficients

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
