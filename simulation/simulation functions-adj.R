library(tidyverse)
library(magrittr)
#library(furrr)
library(MASS)
#library(mvtnorm)
#library(bain)
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


gen_BF<-function(delta, N, hypothesis="X1>X0"){
  Y_C <- rnorm(n = N / 2, mean = 0, sd = 1)
  Y_T <- rnorm(n = N / 2, mean = delta, sd = 1)
  
  dat<-data.frame(Y=c(Y_T,Y_C),
                  X=as.factor(c(rep(1,times=length(Y_T)),
                                rep(0,times=length(Y_C))
                  ))
  )
  
  # lmod<-lm(Y~0+X,dat)
  # BFp_lm <- BF(lmod,hypothesis="X1>X0", complement=TRUE)
  # BFp_lm$BFtable_confirmatory
  
  lm(Y~0+X,dat) %>% 
    BF(hypothesis, complement=TRUE) %$% BFtable_confirmatory %>% 
    as.data.frame() %$%
    BF %>% 
    setNames(c("H1", "Hc"))
}


#a function to simulate a matrix with dim [studies*iter, 2]
sim_t_x_i<-function(delta,
                    tau,
                    N,  #sample size
                    hypothesis=c("X1>X0"),
                    studies=5,
                    iterations=10
){
  sapply(1:(iterations*studies), function(j){
    #sample study-level delta
    study_delta<-rnorm(1,delta,tau)
    #generate BFiu and BFcu
    gen_BF(delta=study_delta,N=N,hypothesis=hypothesis)
    
  }) %>%
    t() %>%
    return()
}

#studies*iter = 100 =>
# user  system elapsed 
# 2.56    0.07    2.68 

#studies*iter = 1000 =>
# user  system elapsed 
# 19.17    0.41   19.75
system.time({
  sim_t_x_i(delta=0.2,
          tau=0.2,
          N=300,
          hypothesis = "X1>X0",
          studies = 10,
          iterations = 100)
})


gen_BF(delta=0.2,N=300)
delta=0.2
N=300

Y_C <- rnorm(n = N / 2, mean = 0, sd = 1)
Y_T <- rnorm(n = N / 2, mean = delta, sd = 1)


mean(Y_T)
mean(Y_C)


ttest<-t_test(x=Y_T, y=Y_C,  alternative="greater") 

ttest$estimate[1]-ttest$estimate[2]


BFp_ttest<-BF(ttest,hypothesis="difference>0", complement=TRUE)
bain_ttest<-bain(ttest,hypothesis="x>y")


lmod_noint<-lm(Y~0+X,dat)
lmod<-lm(Y~X,dat)


BFp_lm_noint<-BF(lmod_noint,hypothesis="X1>X0", complement=TRUE)
BFp_lm_noint$BFtable_confirmatory

BFp_lm<-BF(lmod,hypothesis="X1>0", complement=TRUE)
BFp_lm$BFtable_confirmatory


##############################################

generate_est <- function(N, delta){
  
  # generate control group summary statistics
  Y_C <- rnorm(n = N / 2, mean = 0, sd = 1)
  ybar_C <- mean(Y_C)
  sd_C <- sd(Y_C)
  
  # generate treatment group summary statistics
  Y_T <- rnorm(n = N / 2, mean = delta, sd = 1)
  ybar_T <- mean(Y_T)
  sd_T <- sd(Y_T)
  
  means<-c(ybar_T, ybar_C)
  names(means)<-c("mean_treatment", "mean_control")
  
  # calculate Cohen's d
  sd_pool <- sqrt((sd_C^2 + sd_T^2) / 2)
  d <- (ybar_T - ybar_C) / sd_pool
  
  var<-4 / N + d^2 / (2 * (N - 2))
  
  return(list(means=means,
              d=d,
              var=var))
}

N=200
delta=0.2

obtain_BF<-function(N,delta,hypotheses="mean_treatment>mean_control"){
  params<-generate_est(N,delta)
  BF(x=params$means,hypothesis=hypotheses,Sigma = params$var,n = N) 
}
generate_est(200,0.2)
obtain_BF(N=200, delta=0.2, hypotheses="mean_treatment>mean_control")


###################################################
generate_d <- function(N, delta){
  
  # generate control group summary statistics
  Y_C <- rnorm(n = N / 2, mean = 0, sd = 1)
  ybar_C <- mean(Y_C)
  sd_C <- sd(Y_C)
  
  # generate treatment group summary statistics
  Y_T <- rnorm(n = N / 2, mean = delta, sd = 1)
  ybar_T <- mean(Y_T)
  sd_T <- sd(Y_T)
  
  # calculate Cohen's d
  sd_pool <- sqrt((sd_C^2 + sd_T^2) / 2)
  d <- (ybar_T - ybar_C) / sd_pool
  
  return(d)
}


# # calculate variance of Cohen's d 
# compute_var <- function(N, d){
#   4 / N + d^2 / (2 * (N - 2))
# }


# obtain a bain object, including Bayes factors and 
# posterior model probabilities (PMPs)              
get_bain <- function(est, var, N, hypotheses){
  names(est)<-c("Control", "Treatment")
  bain_list <- BF(x=est,hypothesis=hypotheses,Sigma = var,n = N) 
  return(bain_list)
}



obtain_BF<-function(N,delta,hypotheses="d>0"){
  d<-generate_d(N, delta) 
  var<-compute_var(N, d)
  get_bain(d, var, N, hypotheses) %>% # %$% BF.u %>%
       #  as.data.frame() %>%
       #  rownames_to_column() %>%
       #  pull(var=BF.u, name = rowname) %>%
       # # setNames(c("H1", "Hc")) %>%
        return()
}

obtain_BF(N=300,delta=0.2)
# delta=0.3
# tau=0.2
# study_delta<-rnorm(1,delta,tau)
# obtain_BF(100,0.3,hypotheses="d>0")

#a function to simulate a matrix with dim [studies*iter, 2]
sim_t_x_i<-function(delta,
                    tau,
                    N,  #sample size
                    hypothesis=c( "d>0"),
                    studies=5,
                    iterations=10
){


  sapply(1:(iterations*studies), function(j){
    study_delta<-rnorm(1,delta,tau)
    obtain_BF(N,study_delta,hypotheses="d>0")

  }) %>%
    t() %>%
    return()
}

# sim_t_x_i(delta=0.3,
#                     tau=0.13,
#                     N=40,  #sample size
#                     hypothesis=c( "d>0"),
#                     studies=5,
#                     iterations=10
# )
  
# gen_dat <- function(beta_mu,
#                     beta_tau,
#                     n
#                     ){
#   
#   b<-rnorm(1, beta_mu, beta_tau)
#   r2<-b^2
#   
#   # generate predictors X
#   X<-rnorm(n,0,1)
#   
#   # generate outcome variable Y
#   Y<-X*b + rnorm(n, 0, sd=sqrt(1-r2))
#   # output data
#   bind_cols(X = as.data.frame(X),
#             Y = Y) 
# 
# }
# 
# obtain_BF<-function(data){
#   lm(Y~X, data=data) %>% 
#     BF(hypothesis=hypothesis, complement=TRUE)%$%BFtable_confirmatory %>%
#     as.data.frame() %>% 
#     rownames_to_column() %>% 
#     pull(var=BF, name = rowname) %>% 
#     setNames(c("H1", "Hc")) %>% 
#     return()
# }
# 
# #a function to simulate a matrix with dim [studies*iter, 2] 
# sim_t_x_i<-function(beta_mu,
#                      beta_tau,
#                      n,  #sample size
#                      hypothesis=c( "X>0"),
#                     studies=5,
#                     iterations=10
# ){
# 
#   
#   sapply(1:(iterations*studies), function(i){
#     gen_dat(beta_mu,
#             beta_tau,
#             n) %>% 
#       obtain_BF()
#     
#   }) %>% 
#     t() %>% 
#     return()
# }

# 
# #a function to simulate lm model according to certain simulation conditions
# #sample ratios from log-normal distribution instead of normal with replacement for negative values
# single_sim_ln<-function(r2,  # R-squared of the regression model
#                      pcor,  # correlation between the predictors
#                      ratio_beta=c(3,2,1),  # a numeric vector with the true beta parameters (or the means of the mvnorm dist, if they are sampled);
#                      # Sigma_beta,  # variance covariance matrix of the (true) regression parameters - can be used to induce heterogeneity
#                      p=0.86, #the proportion of the effect size used to determine the SD of the parameters: SD=propSD*betas, 70% is a wide distribution (REF)
#                      hypothesis=c( "V1>V2>V3"),
#                      n,  #sample size 
#                      model  #linear, logistic or probit regression
# ){
#   #n.hyp<-length(unlist(strsplit(hypothesis, ";")))
#   n.hyp<-1 # for now I'd limit the user to only testing a single hypothesis
#   
#   #sample new ratios from a log-normal distribution  
#   #with mean = ratio, and sd=p*ratio
#   m<-ratio_beta
#   s=p*ratio_beta
#   # in order to draw from a log-normal dist with these mean and sd
#   # the location and shape parameters must be reparametrized
#   #https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
#   location <- log(m^2 / sqrt(s^2 + m^2))
#   shape <- sqrt(log(1 + (s^2 / m^2)))
#   
#   new_ratio<-c()
#   for(r in 1:length(ratio_beta)){
#     new_ratio[r]<-rlnorm(n=1, location[r], shape[r])
#   }
#   
#   betas<-coefs(r2, new_ratio, cormat(pcor, length(new_ratio)), "normal")
#   
#   #obtain lm object
#   BFs<-gen_dat(r2=r2,
#                   betas=betas,
#                   rho=cormat(pcor, length(betas)),
#                   n=n,
#                   model="normal") %>% 
#     lm(Y~., data=.) %>% 
#     BF(hypothesis=hypothesis, complement=TRUE)$BFtable_confirmatory %>%
#     as.data.frame() %>% 
#     rownames_to_column() %>% 
#     pull(var=BF, name = rowname) %>% 
#     setNames(c("H1", "Hc"))
#   
#   return(BFs)
# }
# 
# #parallel
# run_sim_ln<-function(r2=0.13,
#                   pcor=0.3,
#                   hypothesis="V1>V2>V3",
#                   ratio_beta=c(3,2,1),
#                   p=0,
#                   n = 25,
#                   model="linear",
#                   studies=30,
#                   iterations=1000){
#   
#   BFs<- t(sapply(1:(iterations*studies), function(i){
#         single_sim_ln(r2=r2,
#                       pcor=pcor,
#                       hypothesis=hypothesis,
#                       ratio_beta=ratio_beta,
#                       p=p,
#                       n = n,
#                       model=model)
#         
#       }))
# 
# } #end run_sim()
# 
# #parallel
# run_sim<-function(r2=0.13,
#                   pcor=0.3,
#                   hypothesis="V1>V2>V3",
#                   ratio_beta=c(3,2,1),
#                   p=0,
#                   n = c(50,75,100,150,200,300),
#                   model="linear",
#                   studies=30,
#                   iterations=1000,
#                   ncores=7,
#                   seed){
#   
#   
#   print( paste0("Prep cluster: ",Sys.time()))
#   
#   cl<-makeCluster(ncores)
#   clusterSetRNGStream(cl, seed)
#   
#   clusterEvalQ(cl, {
#     library(MASS)
#     library(magrittr)
#     library(tidyverse)
#     library(BFpack)
#   })
#   clusterExport(cl=cl, c("single_sim", "q_glm", "gen_dat", "cormat", "coefs"))
#   
#   clusterExport(cl=cl, varlist=c("n", "r2", "pcor", "ratio_beta", "hypothesis",
#                          "p","model"),envir = environment())
#   
#   print(paste0("Start sim: ",Sys.time()))
#   BF_list<- 
#     lapply(1:length(n), function(s){
#       print(paste0("Sample size: ",s, ";", Sys.time()))
#       t(parSapply(cl, 1:(iterations*studies), function(i){
#         single_sim(r2=0.13,
#                    pcor=pcor,
#                    hypothesis=hypothesis,
#                    ratio_beta=ratio_beta,
#                    p=p,
#                    n = n[s],
#                    model=model)
#         
#       }))
#     })
#   print(paste0("End sim: ",Sys.time()))
#   stopCluster(cl)
#   
#   BF_list<-lapply(BF_list, function(x) {
#     cbind(x,1)
#     return(x)
#   } )
#   
#   attributes(BF_list)<-list(hypothesis=hypothesis,
#                             complexity="check",
#                             r2=r2,
#                             pcor=pcor,
#                             ratio_beta=ratio_beta,
#                             p=0,
#                             model="linear",
#                             seed=seed,
#                             iterations=iterations,
#                             studies=studies,
#                             n=n)
#   
#   return(BF_list)
# } #end run_sim()
