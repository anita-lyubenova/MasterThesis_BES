library(WebPower)

#___________________________________________________________________________________
## Specify simulation conditions-----------------------------------------
#__________________________________________________________________________________

## Number of simulations 
nsim <- 100

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


#___________________________________________________________________________________
## Generate data-----------------------------------------
#__________________________________________________________________________________


gen_dat(r2=0.02, 
        betas=coefs(0.02, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
        rho=cormat(pcor, length(ratio_beta)),
        n=3000,
        "normal") %$%
  lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
  summary()

coefs(0.02, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")


gen_dat(r2=0.09, 
        betas=coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
        rho=cormat(pcor, length(ratio_beta)),
        n=200,
        "normal") %$%
  lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
  summary()

coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

data_and_model(r2 = 0.09,          
               betas = coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
               rho = cormat(pcor, length(ratio_beta)),
               n = 200,
               model = 'normal',
               formula = Y ~ V1 + V2 + V3 + V4 + V5 + V6,
               hypothesis = "V4 < V5 < V6")


#__________________________________________________________________________________
#f2 - Calculating power for each coefficient --------------------------------------------
#__________________________________________________________________________________

dat<-gen_dat(r2=0.09, 
             betas=coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
             rho=cormat(pcor, length(ratio_beta)),
             n=100000,
             "normal")


fullm<-lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6, data = dat) %>% summary()
fullm$r.squared

redm<-lm(Y ~ V1 + V2 + V3 + V4 + V5 , data = dat) %>% summary()
redm$r.squared

#sample size needed to detect the effect of V6 - using f2
#I'm unsure how to interpret this effect size
f2.V6<-(fullm$r.squared - redm$r.squared)/(1-fullm$r.squared)

a<-wp.regression(n=NULL, p1 = 6, p2=5, f2=f2.V6, power = .80)
a$n
power.levels<-c(0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99)

N.power.V6<-data.frame(variable = "V6",
                       power=power.levels,
                       p1 = 6, p2=5, f2=f2.V6,
                       N=NA
)


for(i in 1:length(power.levels)){
  N.power$N[i]<- wp.regression(n=NULL, p1 = 6, p2=5, f2=f2.V6, power = power.levels[i])$n
  
}



N.power.V6<-data.frame(variable = "V6",
                       power=power.levels,
                       p1 = 6, p2=5, f2=f2.V6,
                       N=NA
)


for(i in 1:length(power.levels)){
  N.power$N[i]<- wp.regression(n=NULL, p1 = 6, p2=5, f2=f2.V6, power = power.levels[i])$n
  
}



















