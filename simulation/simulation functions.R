library(tidyverse)
library(magrittr)
#library(furrr)
library(MASS)
#library(mvtnorm)
#library(bain)
library(parallel)
# library(ggplot2)
# library(ggstatsplot)
# library(plotly)
# library(readxl)
# library(writexl)
# library(lavaan)
# library(devtools)
# library(SSDbain)
# library(jtools)
# library(ggrepel)
library(BFpack)
# library(Rcpp)
# library(RcppArmadillo)

cormat <- function(partial_cor, diag_length) {
  r <- diag(diag_length)    # fill correlation matrix with partial correlation
  r[r != 1] <- partial_cor  # between variables
  r
}

coefs <- function(r2, ratio, rho, model = c("normal", "logit", "probit")) {
  
  # variance of predicted values (Var(yhat))
  if (model == "normal") {
    var_y <- r2
  }
  else if (model == "logit") {
    var_y <- (r2 * pi^2 / 3) / (1 - r2)
  }
  else if (model == "probit") {
    var_y <- r2 / (1 - r2)
  }
  # value of the regression coefficients
  sqrt(var_y / sum(ratio %*% t(ratio) * rho)) * ratio
}

gen_dat <- function(beta_mu,beta_tau, n
                    ){
  
  b<-rnorm(1, beta_mu, beta_tau)
  r2<-b^2
  
  print(b)
  # generate predictors X
  X<-rnorm(n,0,1)
  
  # generate outcome variable Y
  Y<-X*b + rnorm(n, 0, sd=sqrt(1-r2))
  # output data
  bind_cols(X = as.data.frame(X),
            Y = Y) 

}

gen_dat(0.3,0.3,300000)


#a function to simulate lm model according to certain simulation conditions
single_sim<-function(r2,  # R-squared of the regression model
                     pcor,  # correlation between the predictors
                     ratio_beta,  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
                     # Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                     p=0.6829787, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
                     hypothesis=c( "V1>V2>V3"),
                     n,  #sample size 
                     model  #linear, logistic or probit regression
){
  #n.hyp<-length(unlist(strsplit(hypothesis, ";")))
  n.hyp<-1 # for now I'd limit the user to only testing a single hypothesis
  
  #sample new ratios from a normal distribution
  new_ratio<-mvrnorm(1,mu=ratio_beta, Sigma=diag(p*ratio_beta))
  #resample every time any of the sampled values is negative (because for hte simulation conditions the ratios must be positive)
  while(any(new_ratio<0)) {
    new_ratio<-mvrnorm(1,mu=ratio_beta, Sigma=diag(p*ratio_beta))
  }
  
  betas<-coefs(r2, new_ratio, cormat(pcor, length(new_ratio)), "normal")
  
  #obtain lm object
  lm.mod<-gen_dat(r2=r2,
                  betas=betas,
                  rho=cormat(pcor, length(betas)),
                  n=n,
                  model="normal") %>% 
    lm(Y~., data=.)
  
  varnames<-paste0("H", 1:length(hypothesis))
  
  for(h in 1:length(varnames)){
    
    assign(varnames[h],  BF(lm.mod, hypothesis=hypothesis[h], complement=TRUE)$BFtable_confirmatory %>%
             as.data.frame() %>% 
             rownames_to_column() %>% 
             pull(var=BF, name = rowname)
           )
  }
  
  BFs<-unlist(mget(varnames))
  return(BFs)
}


#a function to simulate lm model according to certain simulation conditions
#sample ratios from log-normal distribution instead of normal with replacement for negative values
single_sim_ln<-function(r2,  # R-squared of the regression model
                     pcor,  # correlation between the predictors
                     ratio_beta=c(3,2,1),  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
                     # Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
                     p=0.86, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
                     hypothesis=c( "V1>V2>V3"),
                     n,  #sample size 
                     model  #linear, logistic or probit regression
){
  #n.hyp<-length(unlist(strsplit(hypothesis, ";")))
  n.hyp<-1 # for now I'd limit the user to only testing a single hypothesis
  
  #sample new ratios from a log-normal distribution  
  #with mean = ratio, and sd=p*ratio
  m<-ratio_beta
  s=p*ratio_beta
  # in order to draw from a log-normal dist with these mean and sd
  # the location and shape parameters must be reparametrized
  #https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  
  new_ratio<-c()
  for(r in 1:length(ratio_beta)){
    new_ratio[r]<-rlnorm(n=1, location[r], shape[r])
  }
  
  betas<-coefs(r2, new_ratio, cormat(pcor, length(new_ratio)), "normal")
  
  #obtain lm object
  lm.mod<-gen_dat(r2=r2,
                  betas=betas,
                  rho=cormat(pcor, length(betas)),
                  n=n,
                  model="normal") %>% 
    lm(Y~., data=.)
  
  varnames<-paste0("H", 1:length(hypothesis))
  
  for(h in 1:length(varnames)){
    
    assign(varnames[h],  BF(lm.mod, hypothesis=hypothesis[h], complement=TRUE)$BFtable_confirmatory %>%
             as.data.frame() %>% 
             rownames_to_column() %>% 
             pull(var=BF, name = rowname)
    )
  }
  
  BFs<-unlist(mget(varnames))
  return(BFs)
}

#parallel
run_sim_ln<-function(r2=0.13,
                  pcor=0.3,
                  hypothesis="V1>V2>V3",
                  ratio_beta=c(3,2,1),
                  p=0,
                  n = 25,
                  model="linear",
                  studies=30,
                  iterations=1000){
  
  BFs<- t(sapply(1:(iterations*studies), function(i){
        single_sim_ln(r2=r2,
                      pcor=pcor,
                      hypothesis=hypothesis,
                      ratio_beta=ratio_beta,
                      p=p,
                      n = n,
                      model=model)
        
      }))

} #end run_sim()

#parallel
run_sim<-function(r2=0.13,
                  pcor=0.3,
                  hypothesis="V1>V2>V3",
                  ratio_beta=c(3,2,1),
                  p=0,
                  n = c(50,75,100,150,200,300),
                  model="linear",
                  studies=30,
                  iterations=1000,
                  ncores=7,
                  seed){
  
  
  print( paste0("Prep cluster: ",Sys.time()))
  
  cl<-makeCluster(ncores)
  clusterSetRNGStream(cl, seed)
  
  clusterEvalQ(cl, {
    library(MASS)
    library(magrittr)
    library(tidyverse)
    library(BFpack)
  })
  clusterExport(cl=cl, c("single_sim", "q_glm", "gen_dat", "cormat", "coefs"))
  
  clusterExport(cl=cl, varlist=c("n", "r2", "pcor", "ratio_beta", "hypothesis",
                         "p","model"),envir = environment())
  
  print(paste0("Start sim: ",Sys.time()))
  BF_list<- 
    lapply(1:length(n), function(s){
      print(paste0("Sample size: ",s, ";", Sys.time()))
      t(parSapply(cl, 1:(iterations*studies), function(i){
        single_sim(r2=0.13,
                   pcor=pcor,
                   hypothesis=hypothesis,
                   ratio_beta=ratio_beta,
                   p=p,
                   n = n[s],
                   model=model)
        
      }))
    })
  print(paste0("End sim: ",Sys.time()))
  stopCluster(cl)
  
  BF_list<-lapply(BF_list, function(x) {
    cbind(x,1)
    return(x)
  } )
  
  attributes(BF_list)<-list(hypothesis=hypothesis,
                            complexity="check",
                            r2=r2,
                            pcor=pcor,
                            ratio_beta=ratio_beta,
                            p=0,
                            model="linear",
                            seed=seed,
                            iterations=iterations,
                            studies=studies,
                            n=n)
  
  return(BF_list)
} #end run_sim()
