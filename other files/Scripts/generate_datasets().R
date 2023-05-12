#a function to generate a single dataset according to certain simulation conditions

source("scripts/ThomVolker scripts/functions.R")
generate_dataset<-function(r2,  # R-squared of the regression model
                           pcor,  # correlation between the predictors
                           betas,  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
                           propSD=0, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
                           n,  #sample size 
                           model  #linear, logistic or probit regression
                           
){
  
  input_betas<-betas
  Sigma_beta<-diag(x=(propSD*input_betas)^2, nrow = length(input_betas),ncol = length(input_betas))
  
  #sample true betas
  betas<-mvrnorm(n=1, mu=input_betas, Sigma = Sigma_beta)
  
  #generate data
  d<-gen_dat(r2=r2,
             betas=betas,
             rho=cormat(pcor, length(betas)),
             n=n,
             model="normal")
  
  
  list(d=d,
       sampled_betas=betas)
  # list(d=d,
  #      inputs=list(r2=r2,
  #                  pcor=pcor,
  #                  betas=input_betas,
  #                  propSD=propSD,
  #                  n=n,
  #                  model=model
  #      ),
  #      sampled_betas=betas,
  #      Sigma_beta=Sigma_beta
  # )
}

# a<-generate_dataset(r2=0.13,  # R-squared of the regression model
#                     pcor=0.3,  # correlation between the predictors
#                     betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
#                     # Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
#                     propSD = 0.2,
#                     #  hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
#                     n=100,  #sample size
#                     model="linear"  #linear, logistic or probit regression
#                     # save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
# )
# a


