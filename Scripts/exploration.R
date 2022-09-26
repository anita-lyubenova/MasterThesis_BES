# 
library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
# # devtools::build("DataCpp")
# # devtools::install("DataCpp")
# # library(DataCpp)
# 
library(MASS)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(plotly)
library(readxl)
library(mvtnorm)
library(highcharter)
library(lavaan)
#
#library(WebPower) #needed for ws.regression(): calculating power for multiple regression
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

# size of the study set: T= 10, 20, 40
# manipulate sample sizes in each set


## Specify relative importance of the regression coefficients
ratio_beta <- c(0, 1, 1, 1, 2, 3)

## Specify the bivariate correlations between predictors
pcor <- c(0.3)


## Models
models <- c("normal")

## r2 of the regression model
r2 <- 0.30 #.09 - too small if testing the difference between two parameters

hypothesis<-"V4 < V5"

complement<-TRUE



### overall N=2000 ------------


planned.n<-read_xlsx("Simulations planning.xlsx", sheet = "Sim1")

#draw overall sample = 10studies x N=200 = 2000
set.seed(123)

pop<-gen_dat(r2=r2, 
             betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
             rho=cormat(pcor, length(ratio_beta)),
             n=2000,
             "normal")


iter<-10

row.names<-paste0("Iter.", seq(1:iter))
column.names<-c(paste0("Study.", seq(1:10)), "aggr.PMP")
slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))

BFiu<-BFic<-array(NA, dim = c(iterations=iter, studies=11, conditions=nrow(planned.n)),
                  dimnames = list(row.names,column.names, slice.names))


scatterp.BFiu<-list()
scatterp.BFic<-list()
all.rows<-1:nrow(pop)
used.rows<-c()
seed<-123

#for each condition (manipulated sample size distribution in the set of studies) (slice m in the array)
for(m in 1:nrow(planned.n)){
  n<-planned.n[m, 3:12] %>% as.numeric()
  
  #for each iteration (row i in the array)
  for(i in 1:iter) {
    
    #clear the used rows because iterations are independent from each other
    used.rows<-c()
    BFcu<-c()
    
    #for each study; column s in the array
    for(s in 1:length(n)){
      
      seed=seed+1
      set.seed(seed)
      
      sampled.rows<-c()
      #sample data for each study in the set
      #note: sample from rows that have not been sampled for another study in the same set
      print(paste("Condition m:", m, ", Iteration i:", i, "Study s:", s))
      sampled.rows<-sample(all.rows[!all.rows %in% used.rows],  size = n[s], replace = FALSE)
      
      used.rows<-c(used.rows, sampled.rows)
      
      BF<-pop[sampled.rows,]%$%
        lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6) %>%
        BF(hypothesis = hypothesis, complement = complement) %$%
        BFtable_confirmatory %>% as.data.frame()%$% BF
      
      BFiu[i,s,m] <-BF[1]
      BFic[i,s,m]<-BF[1]/BF[2] 
      
      BFcu<-c(BFcu, BF[2])
      
    }# end iterations loop i
    
    #after all 10 studies in the set were simulated and evaluated in iteration i => calculate the PMPs for iteration i
    BFiu[i,11,m] <- prod(BFiu[i,1:10,m])/(prod(BFiu[i,1:10,m]) + 1)
    BFic[i,11,m] <- prod(BFic[i,1:10,m])/(prod(BFic[i,1:10,m]) + 1)
    
  }# end study loop s
  
  
  
  ## scatterplot of the BFs ------------
  scatterp.BFiu[[m]] <-BFiu[,1:10,m] %>% as.data.frame() %>%
    pivot_longer(cols = c(1:10),
                 names_to = "study",
                 values_to = "BFiu") %>%
    arrange(BFiu) %>% 
    mutate(iter = rep(seq(1:iter), each=length(n))) %>%
    hchart('scatter', hcaes(x = iter, y = BFiu, group = study)) %>%
    hc_title(text="Scatterplot of the BFiu across iterations grouped by study") %>%
    hc_subtitle(text = paste0(colnames(BFiu[,1:10,m]),": ", n))
  # 
  # scatterp.BFic[[m]] <-BFic[,1:10,m] %>% as.data.frame() %>%
  #   pivot_longer(cols = c(1:10),
  #                names_to = "study",
  #                values_to = "BFic") %>% 
  #   mutate(iter = rep(seq(1:iter), each=length(n))) %>% 
  #   hchart('scatter', hcaes(x = iter, y = BFic, group = study)) %>% 
  #   hc_title(text="Scatterplot of the BFic across iterations grouped by study") %>% 
  #   hc_subtitle(text = paste(c("Condition",m, ":" ,paste0(colnames(BFic[,1:10,m]),": ", n)), collapse = " "))
  # 
  # 
  
  
}#end conditions loop; THE END


#### Violin plots(aggr.PMPs) ----------------------------------------------------
#put the aggregate PMPs together in a long format categorized by condition
vioplot.iu<-data.frame(BFiu[,"aggr.PMP",1:9]) %>% 
  pivot_longer(cols = paste0("Condition.", seq(1:9)),
               names_to = "condition",
               values_to = "aggr.PMP") %>% 
  #boxplot with the PMPs per condition 
  ggbetweenstats(x = condition,
                 y = aggr.PMP,
                 pairwise.comparisons = FALSE,
                 results.subtitle=FALSE,
                 type = "nonparametric",
                 plot.type = "violin"
  ) +
  labs(
    x = "Condition",
    y = "aggregate PMP",
    title = "Distribution of aggregate PMPs from 10 studies (y-axis) in each condition (x-axis) when testing Hi against Hu",
    subtitle = "Each condition represents a different split of the total N=2000 across studies"
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=8, angle=20)
  )+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "grey", size=1)+
  scale_color_manual(values=gg_color_hue(9))+
  scale_x_discrete(labels=paste(paste0("Cond.", 1:9, ": \n"),
                                apply(planned.n[1:9,3:12],1,
                                      function(x) paste(x, collapse = "; ")
                                ) 
  )
  )


vioplot.ic<-data.frame(BFic[,"aggr.PMP",1:9]) %>% 
  pivot_longer(cols = paste0("Condition.", seq(1:9)),
               names_to = "condition",
               values_to = "aggr.PMP") %>% 
  #boxplot with the PMPs per condition 
  ggbetweenstats(x = condition,
                 y = aggr.PMP,
                 pairwise.comparisons = FALSE,
                 results.subtitle=FALSE,
                 type = "nonparametric",
                 plot.type = "violin"
  ) +
  labs(
    x = "Condition",
    y = "aggregate PMP",
    title = "Distribution of aggregate PMPs from 10 studies (y-axis) in each condition (x-axis) when testing Hi against Hc",
    subtitle = "Each condition represents a different split of the total N=2000 across studies"
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=8, angle=20)
  )+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "grey", size=1)+
  scale_color_manual(values=gg_color_hue(9))+
  scale_x_discrete(labels=paste(paste0("Cond.", 1:9, ": \n"),
                                apply(planned.n[1:9,3:12],1,
                                      function(x) paste(x, collapse = "; ")
                                ) 
  )
  )
vioplot.iu
vioplot.ic


scatterp.BFiu[[3]]
scatterp.BFic[[7]]


# Visualization: scatterplots with highcharter
hc <- BFiu[,1:10,1] %>% as.data.frame() %>%
  pivot_longer(cols = c(1:10),
               names_to = "study",
               values_to = "BFiu") %>%
  arrange(BFiu) %>% 
  mutate(iter = rep(seq(1:iter), each=length(n))) %>% 
  hchart('scatter', hcaes(x = iter, y = BFiu, group = study))






# # Sample size determinantion --------------------------------
# #from Fu (2022)
# 
# #library(devtools)
# #install_github("Qianrao-Fu/SSDbain",upgrade="never")
# library(SSDbain)
# 
# ## Specify relative importance of the regression coefficients
# ratio_beta <- c(0, 1, 1, 1, 2, 3)
# ## Specify the bivariate correlations between predictors
# pcor <- c(0.3)
# ## r2 of the regression model
# r2 <- .30
# 
# 
# #draw overall sample = 40studies x N=200 = 8000
# set.seed(123)
# pop<-gen_dat(r2=r2, 
#              betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
#              rho=cormat(pcor, length(ratio_beta)),
#              n=8000,
#              "normal")
# 
# 
# 
# lm(Y ~ V1 + V2 + V3 + V4 + V5 + V6)
# 
# ratio_beta<-c(1,2)
# a<-SSDRegression(Hyp1 = "beta2>beta1", Hyp2 = "Hc", k=2, 
#               rho = cormat(pcor, length(ratio_beta)),
#               R_square1=0.30,
#               R_square2 = 0.30,
#               T_sim = 100,
#               BFthresh=2,
#               eta=0.8,
#               standardize = TRUE,
#               ratio = ratio_beta
#               )
# 
# 
# a

#TPS_i --------------------
# calculate maximum possible true parameter space in line with Hi depending on effect size d and the correlation who



rho<-c(.1, .2, .3)
d<-seq(0,3.5, by=0.1)

mu1<-0

fit.pop<-matrix(NA, nrow = length(d), ncol = length(rho), dimnames = list( paste0("d.", d),
                                                                           paste0("rho.", 1:(length(rho)))
)
)

for(i in 1:length(d)){
  
  mu2<-d[i]+mu1
  
  for(j in 1:length(rho)){
    
    fit.pop[i,j]<-
      integrate(Vectorize(function(x,y)integrate( function(y) dmvnorm(cbind(x,y), mean = c(mu1,mu2), sigma = cormat(rho[j], 2)),lower = 0, upper = Inf)[[1]]),lower=-Inf, upper=0)[[1]]+
      integrate(Vectorize(function(x,y)integrate( function(y) dmvnorm(cbind(x,y), mean = c(mu1,mu2), sigma = cormat(rho[j], 2)),lower = x, upper = 0)[[1]]),lower=-Inf, upper=0)[[1]]+
      integrate(Vectorize(function(y,x)integrate( function(x) dmvnorm(cbind(x,y), mean = c(mu1,mu2), sigma = cormat(rho[j], 2)),lower = 0, upper = y)[[1]]),lower=0, upper=+Inf)[[1]]
    
    
  }
}

fit.pop<-as.data.frame(fit.pop)
fit.pop$d<-d
#calculate the "true BF_iu" for a hypothesis with a single inequality constraint b5>b4(i.e complexity=0.5)
#tested against the complement
compl.i<-0.5
BFpop<-cbind(fit.pop[,1:3]/compl.i, d)


ggplot(BFpop) +
  geom_point(aes(x=d, y=rho.3))

# TPSi -- add variation in mu1 -------------------------------------------------
# This is all wrong -- sigma should be diagonal matirx with very low values on its diagonal so that the density 
#resembles the density of the population (i.e population has n->inf => var->0)
#currently sigma has 1 on its diagonal
mu1<-seq(0, 1, by=0.2)
d<-seq(0,2, by=0.1)
rho=0.0001

fit.pop<-data.frame(mu1 = rep(mu1, each=length(d)),
                    mu2 = NA,
                    d = d,
                    rho=rho,
                    fit.pop=NA
)

for(i in 1:length(d)){
  for(m in 1:length(mu1)){
    
    #calculate mu2
    mu2<-d[i]+mu1[m]
    fit.pop[fit.pop$mu1==mu1[m] & fit.pop$d==d[i],]$mu2<-mu2
    
    for(j in 1:length(rho)){
      
      fit.pop[fit.pop$mu1==mu1[m] & fit.pop$d==d[i] & fit.pop$rho==rho[j],]$fit.pop<-
        integrate(Vectorize(function(x,y)integrate( function(y) dmvnorm(cbind(x,y), mean = c(mu1[m],mu2), sigma = cormat(rho[j], 2)),lower = 0, upper = Inf)[[1]]),lower=-Inf, upper=0)[[1]]+
        integrate(Vectorize(function(x,y)integrate( function(y) dmvnorm(cbind(x,y), mean = c(mu1[m],mu2), sigma = cormat(rho[j], 2)),lower = x, upper = 0)[[1]]),lower=-Inf, upper=0)[[1]]+
        integrate(Vectorize(function(y,x)integrate( function(x) dmvnorm(cbind(x,y), mean = c(mu1[m],mu2), sigma = cormat(rho[j], 2)),lower = 0, upper = y)[[1]]),lower=0, upper=+Inf)[[1]]
      
      
    }
  }
}

#Sim 0: Performance of BF ---------------------------------------------
#how much does the BF vary across iterations depending on sample size
#(when the true parameter space, i.e. the effect size, is kept constant)

#Model: Y ~ V1 + V2, where
#b2 = 2*b1, b2-b1 = 0.2199706
# cor(V1,V2) = 0.3

## Specify relative importance of the regression coefficients
ratio_beta <- c(1,2)
## Specify the bivariate correlations between predictors
pcor <- c(0.3)
## r2 of the regression model
r2 <- 0.09 #.09 - too small if testing the difference between two parameters

#coefficients in the population
coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

## Models
models <- c("normal")
complement<-TRUE
hypothesis<-"V1 < V2"

#n<-seq(20,200, by=10)
n<-c(50,100,150,200,250,300)

iter<-1000

S0_BFiu<-matrix(NA, nrow = iter, ncol = length(n), dimnames = list(1:iter,
                                                                   paste0("n.",n)
)) %>% as.data.frame()

S0_BFiu<-matrix(NA, nrow = iter, ncol = length(n), dimnames = list(1:iter,
                                                                   paste0("n.",n)
)) %>% as.data.frame()

seed<-123
for(s in 1:length(n)){
  
  for(i in 1:iter){
    
    #seed=seed+1
    BF<-gen_dat(r2=r2, 
                betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                rho=cormat(pcor, length(ratio_beta)),
                n=n[s],
                "normal")%$%
      lm(Y ~ V1 + V2) %>%
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory %>% as.data.frame()%$% BF
    
    S0_BFiu[i,s] <- BF[1]
    
  }
}



vioplot.BFiu<-S0_BFiu%>% 
  pivot_longer(cols = paste0("n.", n),
               names_to = "n",
               values_to = "BF_iu") %>%
  mutate(BF_iu_log=log(BF_iu)) %>%  
  #boxplot with the BFs per sample size 
  ggbetweenstats(x = n,
                 y = BF_iu_log,
                 pairwise.comparisons = FALSE,
                 results.subtitle=FALSE,
                 type = "nonparametric",
                 plot.type = "violin"
  ) +
  labs(
    x = "Sample size",
    y = "BF_iu",
    title = "Distribution of Bayes factors (y-axis) for different sample sizes (x-axis) when testing Hi against Hu across 100 iterations"
  )+ 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text( size = 10, color = "black"),
    axis.text.x = element_text(size=10)
  )+ 
  geom_hline(yintercept=c(0), linetype="dashed", 
             color = "grey", size=1)

vioplot.BFiu
log(2)

S0_BFiu%>% 
  pivot_longer(cols = paste0("n.", n),
               names_to = "n",
               values_to = "BF_iu") %>%
  mutate(BF_iu_log=log(BF_iu)) %>% 
  group_by(n) %>% 
  summarise(against.Hi=sum(BF_iu_log<0)/iter,
            suff.support.Hi=sum(BF_iu>1.33)/iter
            
  )




S0_BFiu%>% 
  mutate(iter=seq(1:nrow(S0_BFiu))) %>% 
  pivot_longer(cols = paste0("n.", n),
               names_to = "n",
               values_to = "BF_iu") %>%
  arrange(BF_iu)%>% 
  hchart('scatter', hcaes(x = iter, y = BF_iu, group = n)) %>%
  hc_title(text="Scatterplot of the BFiu across iterations grouped by sample size") 






#

# a function to caluclate the proportion of the true parameter space that is in line with Hi: b2>b1
popfit<-function(mu1,mu2,sigma){
  
  integrate(Vectorize(function(x,y)integrate( function(y) dmvnorm(cbind(x,y), mean = c(mu1,mu2), sigma = sigma),lower = 0, upper = Inf)[[1]]),lower=-Inf, upper=0)[[1]]+
    integrate(Vectorize(function(x,y)integrate( function(y) dmvnorm(cbind(x,y), mean = c(mu1,mu2), sigma = sigma),lower = x, upper = 0)[[1]]),lower=-Inf, upper=0)[[1]]+
    integrate(Vectorize(function(y,x)integrate( function(x) dmvnorm(cbind(x,y), mean = c(mu1,mu2), sigma = sigma),lower = 0, upper = y)[[1]]),lower=0, upper=+Inf)[[1]]
  
}


## Specify relative importance of the regression coefficients
ratio_beta <- c(1,2)
## Specify the bivariate correlations between predictors
pcor <- c(0.3)
## r2 of the regression model
r2 <- 0.30 #.09 - too small if testing the difference between two parameters

#the resulting coefficients
b<- coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")

dat<-gen_dat(r2=r2, 
             betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
             rho=cormat(pcor, length(ratio_beta)),
             n=10000,
             "normal")
cor(dat$V1, dat$V2)

mod<-'#the regression model
                    Y ~ V1 +V2 

                    #show that dependent variable has variance
                    Y ~~ Y

                    #we want to have an intercept
                    Y ~ 1'

regr<-lavaan(model=mod, data = dat)
regr %>% summary()

#the covariance matrix of the parameters
sigma<-vcov(regr)[1:2,1:2]



sigma
#fit of Hi: b2>b1 at the population level, i.e the proportion of true parameter space in line with Hi
popfit(b[1],b[2] , sigma)

#BF 
popfit(b[1],b[2] , rho)/0.5

