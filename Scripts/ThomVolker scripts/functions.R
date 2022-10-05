library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
library(MASS)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(plotly)
library(readxl)
library(mvtnorm)
library(highcharter)
library(lavaan)
library(writexl)
library(devtools)
library(SSDbain)
#
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

gen_dat <- function(r2, betas, rho, n, model = c("normal", "logit", "probit"), 
                    mutate_args = NULL, select_args = quos(everything())) {
  # generate predictors X
  X <- mvrnorm(n, mu = rep(0, length(betas)), Sigma = rho)
  
  # generate outcome variable Y
  if (model == "normal") {
    Y <- X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))
  }
  if (model == "logit") {
    Y <- rbinom(n, 1, 1 / (1 + exp(-(X %*% betas))))
  }
  if (model == "probit") {
    Y <- rbinom(n, 1, pnorm(X %*% betas))
  }
  # output data
  bind_cols(X = as.data.frame(X), Y = Y) #%>%
    # mutate(!!!mutate_args) %>%
    # select(!!!select_args)
}

q_glm <- quietly(glm)

data_and_model <- function(r2, betas, rho, n, model, 
                           formula, 
                           hypothesis, complement = TRUE,
                           mutate_args = NULL, select_args = quos(everything())) {
  
  if (model == "normal") {
    
    # calculate bayes factors for normal data

    gen_dat(r2, betas, rho, n, model, mutate_args, select_args) %>%
      lm(formula = formula, data = .) %>%
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory
    
  } else {
      
    # for logit and probit data, first chck if there is no separation. If there
    # is complete separation of the outcome, then draw a new data set.
    warnings <- 0
      
    while(length(warnings) > 0) {
      mod <- gen_dat(r2, betas, rho, n, model, mutate_args, select_args) %>%
        q_glm(formula = formula, 
              family = binomial(link = model),
              data = .)
      warnings <- mod$warnings
    }
    
    # and calculate bayes factors
    mod$result %>% 
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory
    
  }
}

#function to create a set of colors for the plots based on teh cateogires
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#function to perform sample size determination for specified power levels
#when testing Hi agaitst its complement

power_to_N<-function(power.lvls,
                     r2,
                     pcor,
                     ratio_beta,
                     k=k,
                     H1, #e.g. "beta1>beta2>beta3",
                     T_sim=10000
              
){
  
  betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
  
  power<-data.frame(power=power.lvls,
                       k=k,
                       pcor=pcor,
                       r2=r2,
                       d=betas[length(betas)],
                       n=NA)
  
  
  for(p in 1:length(power.lvls)){
    
    power[p,"n"] <-SSDRegression(Hyp1 = hypothesis, Hyp2 = "Hc", k=k,
                                    rho = cormat(pcor, k),
                                    R_square1=r2,
                                    R_square2 = r2,
                                    T_sim = T_sim,
                                    BFthresh=1,
                                    eta=power.lvls[p],
                                    standardize = TRUE,
                                    ratio = ratio_beta
    )[[1]]
    
    
    
  }
  
}




