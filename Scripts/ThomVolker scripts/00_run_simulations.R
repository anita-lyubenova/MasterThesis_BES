
## Main file

################################################################################
## Load required packages and functions
################################################################################

library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
# devtools::build("DataCpp")
# devtools::install("DataCpp")
library(DataCpp)

source("scripts/functions.R")

################################################################################
## Specify parallel functionality and seed
################################################################################

## Set seed 
set.seed(123)

## Parallel seed
options <- furrr_options(seed = TRUE)

################################################################################
## Specify simulation conditions
################################################################################

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

coefs(0.02, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

gen_dat(0.09, 
        coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "logit"),
        cormat(pcor, length(ratio_beta)),
        100000,
        "logit") %$%
  glm(Y ~ V1 + V2 + V3 + V4 + V5 + V6, family = binomial(link = "logit")) %T>%
  {performance::r2_mckelvey(.) %>% print()} %>%
  summary()

coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "logit")

gen_dat(0.25, 
        coefs(0.25, ratio_beta, cormat(pcor, length(ratio_beta)), "probit"),
        cormat(pcor, length(ratio_beta)),
        100000,
        "probit") %$%
  glm(Y ~ V1 + V2 + V3 + V4 + V5 + V6, family = binomial(link = "probit")) %T>%
  {performance::r2_mckelvey(.) %>% print()} %>%
  summary()

coefs(0.25, ratio_beta, cormat(pcor, length(ratio_beta)), "probit")

################################################################################
## Run simulation scripts - simulation set 1
################################################################################

plan(multisession); source("scripts/01_sample_size_equal.R", echo = FALSE)
rm(list = c("output"))
plan(multisession); source("scripts/02_sample_size_underpowered.R", echo = FALSE)
rm(list = c("output", "n_initial", "n25"))
plan(multisession); source("scripts/03_dummied_variable_cont.R", echo = FALSE)
rm(list = c("output"))
plan(multisession); source("scripts/04_dummied_variable_cat.R", echo = FALSE)
rm(list = c("output"))
plan(multisession); source("scripts/05_combined_variable_mean.R", echo = FALSE)
rm(list = c("output"))
plan(multisession); source("scripts/06_separated_variables.R", echo = FALSE)
rm(list = c("output"))
plan(multisession); source("scripts/07_separated_variables_3wrong.R", echo = FALSE)
rm(list = c("output"))
plan(multisession); source("scripts/08_separated_variables_0included.R", echo = FALSE)
rm(list = c("output"))



################################################################################
## Run simulation scripts - simulation set 2
################################################################################

nstudy <- 150
n      <- c(25, 200)
r2     <- 0.09
models <- c("normal", "logit", "probit")

plan(multisession); source("scripts/09_Hu_Hc_1pred.R", echo = FALSE)
rm(list = c("output"))

models <- "normal"

plan(multisession); source("scripts/10_Hu_Hc_1pred_wrong.R", echo = FALSE)
rm(list = c("output"))

plan(multisession); source("scripts/11_Hu_Hc_3preds.R", echo = FALSE)
rm(list = c("output"))

plan(multisession); source("scripts/12_Hu_Hc_3preds_wrong.R", echo = FALSE)
rm(list = c("output"))

plan(multisession); source("scripts/13_Hu_Hc_3preds_separated.R", echo = FALSE)
rm(list = c("output"))

plan(multisession); source("scripts/14_Hu_Hc_3preds_separated_wrong.R", echo = FALSE)
rm(list = c("output"))

