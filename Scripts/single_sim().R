#a function to simulate a vector of BFs according to certain simulation conditions

source("scripts/ThomVolker scripts/functions.R")


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




# FUNCTION -----------------------------------------------------

single_sim<-function(r2,  # R-squared of the regression model
                     pcor,  # correlation between the predictors
                     betas,  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
                    # Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                     propSD=0, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
                     hypothesis,  # the hypothesis of interest; must be in the format of bain()
                     n,  #sample size 
                     model,  #linear, logistic or probit regression
                     save_mod_coefs=TRUE # should the estimated coefficients and their standard errors be saved?
){
  #n.hyp<-length(unlist(strsplit(hypothesis, ";")))
  n.hyp<-1 # for now I'd limit the user to only testing a single hypothesis
  
  input_betas<-betas
  Sigma_beta<-diag(x=(propSD*input_betas)^2, nrow = length(input_betas),ncol = length(input_betas))
  
  #sample true betas
  betas<-mvrnorm(n=1, mu=input_betas, Sigma = Sigma_beta)
  
  #obtain BFiu
  lm.mod<-gen_dat(r2=r2,
               betas=betas,
               rho=cormat(pcor, length(betas)),
               n=n,
               model="normal") %>% 
     lm(Y~., data=.)
  
  #save the lm estimated coefficients and standard errors
  est_betas<-coef(lm.mod)[-1]
  est_SE<-summary(lm.mod)$coefficients[-1,2]
  
  BF<-lm.mod %>% 
     bain(hypothesis = hypothesis)%$%fit %>%     #$BF.u[c(1,2,4)]
     extract(c(1:n.hyp, nrow(.)),"BF.u") %>%  #subset only BFiu for the specified hypothesis and the complement
     c(.,1) #add BFuu = 1
   
  list(BF=BF,
       inputs=list(r2=r2,
                   pcor=pcor,
                   betas=input_betas,
                   propSD=propSD,
                   hypothesis=hypothesis,
                   n=n,
                   model=model
                    ),
       sampled_betas=betas,
       Sigma_beta=Sigma_beta,
       est_betas=est_betas,
       est_SE=est_SE
       )
}

a<-single_sim(r2=0.13,  # R-squared of the regression model
              pcor=0.3,  # correlation between the predictors
              betas=coefs(0.13, c(3,2,1), cormat(0.3, 3), "normal"),  # a numeric vector with the beta coefficients;  defines the truth in the population;
             # Sigma_beta=NULL,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
              propSD = 0.2,
              hypothesis="V1>V2>V3",  # the hypothesis of interest; must be in the format of bain()
              n=100,  #sample size 
              model="linear",  #linear, logistic or probit regression
              save_mod_coefs=NULL # should the estimated coefficients and their standard errors be saved?
)
a

###############################################################
r2=.13 #effect size r-squared
pcor<-0.3 #correlation between the predictor variables
n<-100 #sample sizes
hypothesis<-"V1>V2>V3" #tested hypotheses
models <- c("normal") #linear, logistic or probit regression
ratio_beta<-c(9,3,1)
betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


####################################################################

