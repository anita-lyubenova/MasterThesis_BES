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
#install_github("Qianrao-Fu/SSDbain",upgrade="never")
library(SSDbain)
library(jtools)
library(ggplot2)
library(ggrepel)

#load the functions
load("Outputs/functions.RData")


#SSDreegression() --------------------------------
#from Fu (2021)




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
### previous version (mediumm R2) -------------------------
pcor<-0.2
#r2<-.04
#r2= b1^2 + b2^2 + 2*b1*b2*pcor, where b1 and b2 are the coefficients found in the section K=3 and r2=0.13
r2 = 0.16810970^2 + 0.08405485^2 + 2*0.08405485*0.16810970*0.2

ratio_beta <- c(2,1)
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

power.k2<-data.frame(k=2,
                     ratio="2:1",
                     r2=r2,
                     d=betas[length(betas)],
                     power=power.lvls,
                     n=NA)
for(p in 1:length(power.lvls)){
  
  power.k2[p,ncol(power.k2)] <-SSDRegression(Hyp1 = "beta1>beta2", Hyp2 = "Hc", k=2,
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

save(power.k2, file="Outputs/power quantification/power.k2.RData")
write_xlsx(power.k2, "Outputs/power quantification/power.k2.xlsx")
plot(power.k2)

### R2=.02------------------------
power.lvls<-c(seq(0.50, 0.95, by=0.05),99)
r2=.02
ratio_beta<-c(2,1)
pcor<-0.3
rho<-cormat(pcor, length(ratio_beta))
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

#obtain the semipartial correlations
m<-gen_dat(r2=r2, 
           betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
           rho=cormat(pcor, length(ratio_beta)),
           n=1000000,
           "normal")%$%
  lm(Y ~ V1 + V2) %>% 
  summ(part.corr=TRUE)

part.cor1<-m$coeftable[2,5]
part.cor2<-m$coeftable[3,5]

z1<-log((1+part.cor1)/(1-part.cor1))
z2<-log((1+part.cor2)/(1-part.cor2))

q=z1-z2
q 

#compute power
power.k2.p2<-power_to_N(power.lvls=power.lvls,
                        r2=r2,
                        pcor=pcor,
                        ratio_beta=ratio_beta,
                        k=length(ratio_beta),
                        H1="beta1>beta2",
                        T_sim=10000)
#minimum power: 0.5834 0.5822 

## for K=3 ------------------------------- 
### d=.08 -------------------------------
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

save(power.k3, file="Outputs/power quantification/power.k3.RData")

### d=.05 -------------------------------
power.lvls<-seq(0.05, 0.95, by=0.05)
d<-0.05435573
pcor<-0.2
r2<-(3*d)^2+ (2*d)^2  + d^2 + 2*pcor*(d*2*d + d*3*d + 2*d*3*d) # = 0.05436364
ratio_beta <- c(3,2,1)


power.k3.d1.r2<-power_to_N(power.lvls=power.lvls,
                         r2=r2,
                         pcor=0.2,
                         ratio_beta=ratio_beta,
                         k=3,
                         H1="beta1>beta2>beta3",
                         T_sim=10000)

## for K=4 ------------------------------- 



# Summarize and export results -----------------

power.k3$ratio<-"3:2:1"

colnames(power.k2)
colnames(power.k3)

power.k3$d<-0.08405485

power.k3<-power.k3[,colnames(power.k2)]
power.results<-rbind(power.k2, power.k3)

plot(power.k3$power, power.k3$n)

save(power.results, file="Outputs/power quantification/power.results.RData")
write_xlsx(power.results, "Outputs/power quantification/power.results.xlsx")




#Analyse results -----------------------------------------------------------

#Visualize how the power increases with sample size

power.results<-as.data.frame(power.results)
ggplot(data=power.results) %>% 
  geom_line(mapping = aes(x=n, y=power), group=ratio)
power.results %>% 
  ggplot(aes(x=n, y=power, group=ratio, color=ratio))+
  geom_line()+
  geom_point()+
  scale_y_continuous(breaks = seq(0, 1, by=0.05))+
  geom_text(aes(label=n),
          #  vjust=c(rep(0.5, times=11), rep(-0.5, times=11)),
            hjust=c(rep(-0.8, times=10),-0.1, rep(+1.8, times=10),-0.4))




#Hi effect size investigation --------------------------
#Omportance of R2 and ratio_beta for the power of a study to test Hi


## MAIN POWER simulation ------------------------
power.lvls<-seq(0.40, 0.95, by=0.05)
r2=.02
ratio_beta<-c(2,1)
pcor<-0.3
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
# 
#obtain the semipartial correlations
m<-gen_dat(r2=r2,
           betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
           rho=cormat(pcor, length(ratio_beta)),
           n=1000000,
           "normal")%$%
  lm(Y ~ V1 + V2) %>%
  summ(part.corr=TRUE)

part.cor1<-m$coeftable[2,5]
part.cor2<-m$coeftable[3,5]

z1<-log((1+part.cor1)/(1-part.cor1))
z2<-log((1+part.cor2)/(1-part.cor2))

q=z1-z2
q

#compute power
power.k2.p2<-power_to_N(power.lvls=power.lvls,
                        r2=r2,
                        pcor=pcor,
                        ratio_beta=ratio_beta,
                        k=length(ratio_beta),
                        H1="beta1>beta2",
                        T_sim=10000)


## Change q------------------------
#by increasing the ratio
#However, there must be a limit to q depending on R2
# the increase in q is very small after .20 even with large increase in the ratio,
# the limit of q for r2=.02 must be around .27
# 
# #a for loop to investigate how q increases with increasing ratio
# q<-c()
# rats<-seq(5,80, by=5)
# for(i in 1:length(rats)){
#   r2=.02
#   ratio_beta<-c(rats[i],1)
#   pcor<-0.3
#   
#   #obtain the semipartial correlations
#   m<-gen_dat(r2=r2, 
#              betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
#              rho=cormat(pcor, length(ratio_beta)),
#              n=1000000,
#              "normal")%$%
#     lm(Y ~ V1 + V2) %>% 
#     summ(part.corr=TRUE)
#   
#   part.cor1<-m$coeftable[2,5]
#   part.cor2<-m$coeftable[3,5]
#   
#   z1<-log((1+part.cor1)/(1-part.cor1))
#   z2<-log((1+part.cor2)/(1-part.cor2))
#   
#   q[i] =z1-z2
#   print(i)
#   
# }
# cbind(rats, q)
# plot(1:16,q)

power.lvls<-seq(0.60, 0.95, by=0.05)
r2=.02
ratio_beta<-c(55,1)
pcor<-0.3
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

# #obtain the semipartial correlations
# m<-gen_dat(r2=r2, 
#            betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
#            rho=cormat(pcor, length(ratio_beta)),
#            n=1000000,
#            "normal")%$%
#   lm(Y ~ V1 + V2) %>% 
#   summ(part.corr=TRUE)
# 
# part.cor1<-m$coeftable[2,5]
# part.cor2<-m$coeftable[3,5]
# 
# z1<-log((1+part.cor1)/(1-part.cor1))
# z2<-log((1+part.cor2)/(1-part.cor2))
# 
# q.single=z1-z2
# q.single

#compute power
power.k2.p2.increase.q<-power_to_N(power.lvls=power.lvls,
                        r2=r2,
                        pcor=pcor,
                        ratio_beta=ratio_beta,
                        k=length(ratio_beta),
                        H1="beta1>beta2",
                        T_sim=10000)




## increase R2 -----------------
#and decrease the ratio to keep q = .112 ( as in the main power simulation)
power.lvls<-seq(0.40, 0.95, by=0.05)
r2=.13
ratio_beta<-c(1.3,1)
pcor<-0.3
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

# #obtain the semipartial correlations
# m<-gen_dat(r2=r2, 
#            betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
#            rho=cormat(pcor, length(ratio_beta)),
#            n=1000000,
#            "normal")%$%
#   lm(Y ~ V1 + V2) %>% 
#   summ(part.corr=TRUE)
# 
# part.cor1<-m$coeftable[2,5]
# part.cor2<-m$coeftable[3,5]
# 
# z1<-log((1+part.cor1)/(1-part.cor1))
# z2<-log((1+part.cor2)/(1-part.cor2))
# 
# q.single=z1-z2
# q.single

#compute power
power.k2.p2.increase.R2<-power_to_N(power.lvls=power.lvls,
                        r2=r2,
                        pcor=pcor,
                        ratio_beta=ratio_beta,
                        k=length(ratio_beta),
                        H1="beta1>beta2",
                        T_sim=10000)

write_xlsx(power.k2.p2.increase.q, "Outputs/power quantification/power.k2.p2.increase.q.xlsx")

write_xlsx(power.k2.p2.increase.R2, "Outputs/power quantification/power.k2.p2.increase.R2.xlsx")


## increase ratio ------------------
r2=.02
ratio_beta<-c(4,1)
pcor<-0.3
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

#obtain the semipartial correlations
m<-gen_dat(r2=r2,
           betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
           rho=cormat(pcor, length(ratio_beta)),
           n=1000000,
           "normal")%$%
  lm(Y ~ V1 + V2) %>%
  summ(part.corr=TRUE)

part.cor1<-m$coeftable[2,5]
part.cor2<-m$coeftable[3,5]

z1<-log((1+part.cor1)/(1-part.cor1))
z2<-log((1+part.cor2)/(1-part.cor2))

q.single=z1-z2
q.single

## Visualization -------------------
power.importance.comparison<-read_xlsx("Outputs/power quantification/power.k2.p2.xlsx", sheet="comparison to .increase")

power.importance.comparison %>% 
  ggplot(aes(x=power, y=n, group=Condition, color=Condition))+
  geom_line()+
  geom_point()+
  geom_text_repel(aes(y=n, label=n, color=Condition),
                  size=4)+
  theme_minimal()+
  scale_x_continuous(breaks=seq(0.5,0.95, by = 0.05))+
  labs(title = "Importance of R2 and q for the power of a study to test H1: b1>b2 against Hc",
       caption= "Conditions. Main: R2=.02	ratio = c(2, 1), q = .112;
                             Increase q: R2= .02, ratio = c(55, 1), q = .27,
                             Increase R2: R2= .13, ratio = c(1.3, 1), q = .112;
                             pcor (all) = .3
       ")
