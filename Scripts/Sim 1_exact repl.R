
library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
# devtools::build("DataCpp")
# devtools::install("DataCpp")
# library(DataCpp)

library(MASS)
library(dplyr)
library(WebPower) #needed for ws.regression(): calculating power for multiple regression
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


a<-gen_dat(r2=0.09, 
        betas=coefs(0.09, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
        rho=cormat(pcor, length(ratio_beta)),
        n=50,
        "normal") %$%
  lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
  summary()%$% coefficients[5:6,1:2] %>% as.data.frame()


a$CI.lb<- a$Estimate-1.96*a$`Std. Error`
a$CI.ub<- a$Estimate+1.96*a$`Std. Error`
a[,-2]


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
fullm$coefficients[5:7,1]
betas<-fullm$coefficients[5:7,1]
names(betas)<-c("b4","b5","b6")

betas["b4"]

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



## Power simulation ------

### per coef -------------
power<-data.frame(var = paste0("V", 2:6),
                       N=rep(n, each=5),
                        r2=rep(r2, each=30),
                       sig.count = 0,
                       power=NA
                       )



#for each sample size
for(s in n){
  #for each effect size
  for(f in r2){
    
    for (i in 1:10000){
      #simulate data
      p.vals<-gen_dat(r2=f, 
              betas=coefs(f, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
              rho=cormat(pcor, length(ratio_beta)),
              n=s,
              "normal") %$%
              #perform linear regression
              lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
              summary() %$% coefficients[3:7,4]
    
        print(i)
        power[power$N==s & power$r2==f,]$sig.count<-power[power$N==s & power$r2==f,]$sig.count + (p.vals < .05)

}
    
  }
  
}
power$power<- power$sig.count/10000

#save(power, file = "Outputs/sim1/power.sig.coef.RData")


### per hypothesis -----
#I.e. the probability of the posterior having locations that represent the correct ordering of the parameters
# which increases the probability of having a good fit when the hypothesis is true

#### Option 1: no SEs ----------------
#how often is the ordering of the parameters correctlooking only at point estimates of the
# parameters and disregarding SEs
hypotheses<-c(
  "b5>b4",
  "b6>b4",
  "b6>b5"
)

power.H<-data.frame(hypothesis = rep(hypotheses, each=18),
                  r2=rep(r2, each=length(n)),
                  N=n,
                  TRUE.count = 0,
                  power=NA
)
k<-0

for(h in hypotheses){
  
  #for each sample size
  for(s in n){
    
    #for each effect size
    for(f in r2){
      
      for (i in 1:10000){
        k=k+1
        
        #simulate data
        betas<-gen_dat(r2=f, 
                        betas=coefs(f, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                        rho=cormat(pcor, length(ratio_beta)),
                        n=s,
                        "normal") %$%
          #perform linear regression
          lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
          summary() %$% coefficients[5:7,1]
        b4<-betas[1]
        b5<-betas[2]
        b6<-betas[3]
        
        
        #print(power.H[power.H$N==s & power.H$r2==f & power.H$hypothesis==h,]$TRUE.count)
        print(k)
        
        power.H[power.H$N==s & power.H$r2==f & power.H$hypothesis==h,]$TRUE.count<-
          power.H[power.H$N==s & power.H$r2==f & power.H$hypothesis==h,]$TRUE.count + (eval(parse(text = h)))
        
        
        
      }
      
    }
    
  }

}

power.H$power<- power.H$TRUE.count/10000

save(power.H, file = "Outputs/sim1/power.hypotheses.RData")

power.H$hypothesis<-recode(power.H$hypothesis,
                "b6>b5" = "b5<b6",
                "b5>b4"="b4<b5",
                "b6>b4"="b4<b6"
                )
power.H.mid<-subset(power.H, r2==.09)

power.H.sub<-subset(power.H, hypothesis!="b4<b6")




#### Option 2: eval with SE -----
# Frequentist way of determiningn whether one parameter is significantly larger then another one
# by comparing the upper and lower bounds of the 95% CIs.
n <- 25 * 2^{0:6}

hypotheses.SE<-c(
  "b4+1.96*SE_b4 < b5-1.96*SE_b5",
  "b5+1.96*SE_b5 < b6-1.96*SE_b6"
  
)

powerH.SE<-data.frame(hypothesis = rep(hypotheses.SE, each=length(n)),
                    r2=0.25,
                    N=n,
                    TRUE.count = 0,
                    power=NA
)

k<-0

for(h in hypotheses.SE){
  
  #for each sample size
  for(s in n){
    
    #for each effect size
    for(f in 0.25){
      
      for (i in 1:10000){
        k=k+1
        
        #simulate data
        betas<-gen_dat(r2=f, 
                       betas=coefs(f, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                       rho=cormat(pcor, length(ratio_beta)),
                       n=s,
                       "normal") %$%
          #perform linear regression
          lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
          summary() %$% coefficients[5:7,1:2]
        b4<-betas[1,1]
        b5<-betas[2,1]
        b6<-betas[3,1]
        
        SE_b4<-betas[1,2]
        SE_b5<-betas[2,2]
        SE_b6<-betas[3,2]
        
        #print(power.H[power.H$N==s & power.H$r2==f & power.H$hypothesis==h,]$TRUE.count)
        print(k)
        
        powerH.SE[powerH.SE$N==s & powerH.SE$r2==f & powerH.SE$hypothesis==h,]$TRUE.count<-
          powerH.SE[powerH.SE$N==s & powerH.SE$r2==f & powerH.SE$hypothesis==h,]$TRUE.count + (eval(parse(text = h)))
        
        
        
      }
      
    }
    
  }
  
}


(eval(parse(text = h[1])))

powerH.SE$power<- powerH.SE$TRUE.count/10000



# Simulation 1 -----------------------------------------------------------
# r2=0.09
# size of the study set: T= 10, 20, 40
# manipulate sample sizes in each set


# ## Number of simulations 
# nsim <- 100

# ## Sample sizes
# n <- 25 * 2^{0:5}


## Specify relative importance of the regression coefficients
ratio_beta <- c(0, 1, 1, 1, 2, 3)

## Specify the bivariate correlations between predictors
pcor <- c(0.3)


## Models
models <- c("normal")

## r2 of the regression model
r2 <- .09

hypothesis<-"V4 < V5"

complement<-TRUE

#draw overall sample = 40studies x N=200 = 8000
set.seed(123)
pop<-gen_dat(r2=r2, 
        betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
        rho=cormat(pcor, length(ratio_beta)),
        n=8000,
        "normal")

a<-lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6, data = pop) %>%
  BF(hypothesis = hypothesis, complement = FALSE) %$%
  BFtable_confirmatory %>% as.data.frame() %$% BF
a

gen_dat(r2=r2, 
        betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
        rho=cormat(pcor, length(ratio_beta)),
        n=50,
        "normal")%$%
  lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
  BF(hypothesis = hypothesis, complement = FALSE) %$%
  BFtable_confirmatory %>% as.data.frame()



1.9998914587/(1.9998914587+0.0001085413)


a[1]/a[2]

(a$`fit>`[1]/a$`complex>`[1])/(a$`fit>`[2]/a$`complex>`[2])

### 1.1 equal n(adequate power) -----------------


n <- rep(200, times=10)
iter<-100

BFiu<-matrix(NA,nrow = iter, # a row per iteration
          ncol=length(n)) %>% as.data.frame() #columns represent each study in the set
BFic<-matrix(NA,nrow = iter, # a row per iteration
             ncol=length(n)) %>% as.data.frame() #columns represent each study in the set

all.rows<-1:nrow(pop)
used.rows<-c()
seed<-123

#for each iteration
for(i in 1:iter) {
  
  #clear the used rows because iterations are independent from each other
  used.rows<-c()
  sum.BF.iu.cu<-c()
  sum.BF.iu.uu<-c()
  
  BFcu<-c()
  
  #for each study
  for(s in 1:length(n)){
    
    seed=seed+1
    set.seed(seed)
    
    sampled.rows<-c()
    #sample data for each study in the set
    #note: sample from rows that have not been sampled for another study in the same set
    sampled.rows<-sample(all.rows[!all.rows %in% used.rows],  size = n[s], replace = FALSE)
    
    used.rows<-c(used.rows, sampled.rows)
    
     BF<-pop[sampled.rows,]%$%
          lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
          BF(hypothesis = hypothesis, complement = complement) %$%
          BFtable_confirmatory %>% as.data.frame()%$% BF
      
     # BFiu[i,s] <-log(BF[1])
     # BFic[i,s]<-log(BF[1]) - log(BF[2]) 
     
     # #save the sum of the BF for the two hypotheses
     # sum.BF.iu.uu<-c(sum.BF.iu.uu, log(1+BF[1]))
     # sum.BF.iu.cu<-c(sum.BF.iu.cu, log(BF[1]+BF[2]))
     
     BFiu[i,s] <-BF[1]
     BFic[i,s]<-BF[1]/BF[2] 
     
     BFcu<-c(BFcu, BF[2])
    
     

  }
  # #Attempt 1
  # BFiu$PMPiu_T[i] <- sum(BFiu[i,1:10]) / sum(BFiu[1,1:10])
  # BFic$PMPic_T[i] <- sum(BFic[i,1:10]) / prod(sum.BF)
  
  # #Attempt 2: s
  # BFiu$PMPiu_T[i] <- sum(BFiu[i,1:10]) / sum(sum.BF.iu.uu)
  # BFic$PMPic_T[i] <- sum(BFic[i,1:10]) / sum(sum.BF.iu.cu)
  
  # #Attempt 3: s
  # BFiu$PMPiu_T[i] <- exp(sum(BFiu[i,1:10]) - sum(sum.BF.iu.uu))
  # BFic$PMPic_T[i] <- exp(sum(BFic[i,1:10]) - sum(sum.BF.iu.cu))
  
  #Attempt 4:
  BFiu$PMPiu_T[i] <- prod(BFiu[i,1:10])/(prod(BFiu[i,1:10]) + 1)
  BFic$PMPic_T[i] <- prod(BFiu[i,1:10])/(prod(BFiu[i,1:10]) + prod(BFcu))
  
}



viol.df<-data.frame(iu = BFiu$PMPiu_T,
                    ic = BFic$PMPic_T) %>% pivot_longer(cols = c("iu", "ic"),
                                                        names_to = "alternative",
                                                        values_to = "aggr.PMP")

viol.df$alternative<-as.factor(viol.df$alternative)

library(ggstatsplot)


plt <- ggbetweenstats(
  data = viol.df,
  x = alternative,
  y = aggr.PMP
)


plt <- plt + 
  # Add labels and title
  labs(
    x = "Alternative hypothesis",
    y = "aggregate PMP",
    title = "Distribution of aggregate PMPs (from 10 studies) across iterations when testing against Hc or Hu"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 8, color = "black"),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      size = 5, 
      color="grey"
    )
  )




