#load packages
library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
library(MASS)
library(mvtnorm)
library(bain)
library(knitr)

#load the functions
source("scripts/functions.R")

iter<-1000
#simulate the data
source("scripts/Simulate BFs_MPCTH(H1).R")
source("scripts/Simulate BFs_MPCTH(Hc).R")
source("scripts/Simulate BFs_ES(H1)=ES(Hc).R")

#process the data and compute aggregate PMPs for 2 conditions
source("scripts/Aggregate PMPs_H1 vs. Hc.R")
source("scripts/Aggregate PMPs_H1 vs. Hc vs. Hu.R")

#create the plots 
source("scripts/Figures.R")




