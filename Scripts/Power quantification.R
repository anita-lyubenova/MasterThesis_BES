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


SSDreegression() --------------------------------
#from Fu (2021)

library(devtools)
#install_github("Qianrao-Fu/SSDbain",upgrade="never")
library(SSDbain)

## Specify relative importance of the regression coefficients
ratio_beta <- c(2,1)
## Specify the bivariate correlations between predictors
pcor <- c(0.2)
## r2 of the regression model
r2 <- .13

a<-SSDRegression(Hyp1 = "beta1>beta2", Hyp2 = "Hc", k=2,
              rho = cormat(pcor, length(ratio_beta)),
              R_square1=r2,
              R_square2 = r2,
              T_sim = 10000,
              BFthresh=3,
              eta=0.8,
              standardize = TRUE,
              ratio = ratio_beta
              )

# #keep d the same, wth 3 predicotrs and larger r2 --> is the power for the same hypothesis the same?
# #does the number of predictors in the model influence the power when the effect size of the hypothesis remains the same?
# 
# The algorithm cannot fin N max in this case => apparetnly the hypothesis defined should incude all predictors in the model
#r 2 <- .40
# ratio_beta <- c(3,2,1)
# coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
# 
# b<-SSDRegression(Hyp1 = "beta1>beta2>beta3", Hyp2 = "Hc", k=3,
#                  rho = cormat(pcor, length(ratio_beta)),
#                  R_square1=r2,
#                  R_square2 = r2,
#                  T_sim = 10,
#                  BFthresh=3,
#                  eta=0.8,
#                  standardize = TRUE,
#                  ratio = ratio_beta
# )
# 


#__________________________________________________________________________________________
# Reproduce SSD Fu(2021) -------------------------------------------------------
#__________________________________________________________________________________________
# In particular Table 4.4 (p.128) H2: b1>b2 vs. Hc: not H2

#Specify conditions

models <- c("normal")
## Specify the bivariate correlations between predictors
pcor <- c(0.2)
## r2 of the regression model
r2 <- 0.13
complement<-TRUE





##Hi == TRUE ---------------------------------------------------------
#1) draw samples from a population in which Hi is true
#2) compute BFic in each sample
#3) what is the proportion of BFs that are larger than BF.thres?

#reltive importance of the predictors
ratio_beta <- c(2,1)
Hi<-"V1 > V2"
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
d<-betas[1] - betas[2] #0.1497124

iter<-10000

BFic<-c()

#iter=10000 ann n=235 run for a minute
for(i in 1:iter){
  
  BF<-gen_dat(r2=r2, 
              betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
              rho=cormat(pcor, length(ratio_beta)),
              n=235,
              "normal")%$%
        lm(Y ~ V1 + V2) %>%
        BF(hypothesis = Hi, complement = complement) %$%
        BFtable_confirmatory %>% as.data.frame()%$% BF
  
  
  BFic[i]<-BF[1]/BF[2]

} #end iterations i loop

sum(BFic>3)/iter #0.8043




##Hc == TRUE-----------------------------------------------------------------
#1) draw samples from a population in which Hc is true
#2) compute BFci in each sample
#3) what is the proportion of BFs that are larger than BF.thres?

#The complement hypothesis h´comprises of two scenarios: b1<b2 (complement1) and b1=b2 (complement2)
# however b1=b2 is disregarded because the BF will be 1 independent of hte sample size
### complement1: b1<b2 --------------------------------------------------------

#let the ratio be b1:b2 = 1:2 to keep R2 = 0.13

ratio_beta <- c(1,2)
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
Hc1<-"V1<V2"

iter<-10000
BFc1i<-c()

for(i in 1:iter){
  
  BF<-gen_dat(r2=r2, 
              betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
              rho=cormat(pcor, length(ratio_beta)),
              n=235,
              "normal")%$%
    lm(Y ~ V1 + V2) %>%
    BF(hypothesis = Hc1, complement = complement) %$%
    BFtable_confirmatory %>% as.data.frame()%$% BF
  
  
  BFc1i[i]<-BF[1]/BF[2]
  
} #end iterations i loop

sum(BFc1i>3)/iter #0.8057



## Resulting power-------------------------------------------------------------

#Given BF.thres=3
#   if Hi:b1>b2 is true the probability of "accepting" Hi is P(BFiu > 3|Hi)=0.804
#   if Hc1: b1<b2 is true the prob of accepting Hc1 is P(BFc1i > 3|Hc1i)=0.806
#   if Hc2: b1=b2 is true the prob of accepting Hc2 is P(BFc2i > 3|Hc2i)=0.9447
#What is the prob of accepting Hc if Hc1 or Hc2 is true? P(BFci>3 | Hc)=?
#P(BFc1i > 3 or BFc2i > 3 | Hc1i or Hc2i)=?

#Within Hc Hc1 and Hc2 are equally likely (0.5 each) =>

#P(BFci>3 | Hc) = 0.806/2 + 0.9447/2 = 0.87535






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

pcor <- c(0.2)


#SDSRegression()----------------------------------------------------------------------
# Find sample sizes that correspond to the power of 
power.lvls<-c(.50, .60, .70, .80, .90, .95, .99)
#These would be the sample sizes I will use for Sim1



## for K=2 ------------------------------- 
r2<-.04
ratio_beta <- c(2,1)
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

power.k2<-data.frame(power=power.lvls,
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

power.k2$k<-2
power.k2$r2<-0.04
power.k2$d<-0.08304548

write_xlsx(power.k2, "Outputs/power quantification/SDSpower.k(2).r2(.05),d(.08).xlsx")
plot(power.k2)


## for K=2 ------------------------------- 





## for K=3 ------------------------------- 
