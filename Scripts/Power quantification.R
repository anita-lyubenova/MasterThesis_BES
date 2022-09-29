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
#

#load the functions
load("Outputs/functions.RData")


#SSDreegression() --------------------------------
#from Fu (2021)

library(devtools)
#install_github("Qianrao-Fu/SSDbain",upgrade="never")
library(SSDbain)


#__________________________________________________________________________________________
# Power depending on hypothesis effect size d -------------------------------------------
#__________________________________________________________________________________________
#Check if the same absolute difference b1-b2 = 0.1497124 will lead to the same power estimate
# if there are three or 4 predictors in the model (with higher r2, so that d remains the same)


#find r2 such that the difference d between the coefficients is similar for 2, 3 and 4 predictors in the set
# "medium" effect size R2=.13 (cohen, 1988) was selected when K=3, because it has been stated that this is a slightly infated
#effect size because it is assumed that several predictors are included in the model.
#Thus, similarly "medium" effect size should be smaller if there are only two predictors. Respectively, if K=4 the "medium"
# effect size should be increased

# r2<-.04
# ratio_beta <- c(2,1)
# coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
#
# r2<-.13
# ratio_beta <- c(3,2,1)
# coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
# 
# r2<-.31
# ratio_beta <- c(4,3,2,1)
# coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")



#SDSRegression()----------------------------------------------------------------------
# Find sample sizes that correspond to the power of 
# power.lvls<-c(.50, .60, .70, .80, .90, .95, .99)
# power.lvls<-c(.55, .65, .75, .85)
power.lvls<-c(seq(from=0.5, to=0.95, by=0.05),0.99)

#These would be the sample sizes I will use for Sim1

#(I've run them separately but in the end they should be in the same file)

## for K=2 ------------------------------- 
pcor<-0.2
r2<-.04
#r2= b1^2 + b2^2 + 2*b1*b2*pcor, where b1 and b2 are the coefficients found in the section K=3 and r2=0.13
r2 = 0.16810970^2 + 0.08405485^2 + 2*0.08405485*0.16810970*0.2

ratio_beta <- c(2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

betas[length(betas)]
power.k2<-data.frame(k=2,
                     r2=0.04,
                     d=betas[length(betas)],
                     power=power.lvls,
                     n=NA)
for(p in 1:length(power.lvls)){
  
  power.k2[p,2] <-SSDRegression(Hyp1 = "beta1>beta2", Hyp2 = "Hc", k=2,
                   rho = cormat(pcor, length(ratio_beta)),
                   R_square1=r2,
                   R_square2 = r2,
                   T_sim = 10000,
                   BFthresh=1,
                   eta=power.lvls[p],
                   standardize = TRUE,
                   ratio = ratio_beta
  )[[1]]
  
  

}
beep("coin")
power.k2$k<-2
power.k2$r2<-0.04
power.k2$d<-0.08304548

write_xlsx(power.k2, "Outputs/power quantification/SDSpower.k(2).r2(.05),d(.08).xlsx")
plot(power.k2)




## for K=3 ------------------------------- 
power.lvls<-c(seq(from=0.5, to=0.95, by=0.05),0.99)
pcor<-0.2
r2<-.13
ratio_beta <- c(3,2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

power.k3<-data.frame(power=power.lvls,
                     k=3,
                     r2=0.13,
                     d=betas[length(betas)],
                     n=NA)

#This took about 14-15 hours to run
for(p in 1:length(power.lvls)){
  
  power.k3[p,"n"] <-SSDRegression(Hyp1 = "beta1>beta2>beta3", Hyp2 = "Hc", k=3,
                                rho = cormat(pcor, length(ratio_beta)),
                                R_square1=r2,
                                R_square2 = r2,
                                T_sim = 10000,
                                BFthresh=1,
                                eta=power.lvls[p],
                                standardize = TRUE,
                                ratio = ratio_beta
  )[[1]]
  
  
  
}


## for K=4 ------------------------------- 



# Summarize and export results -----------------
colnames(power.k2)
colorder <-c("k", "r2", "d", "power", "n")

power.k2<-power.k2[,colorder]
power.k3<-power.k3[,colorder]
rbind(power.k2, power.k3)

plot(power.k3$power, power.k3$n)
