library(tidyverse)

 
# Simulated dist of the fit -------------------------------------------------
load("Outputs/exploration/q.general.perf.RData")
q<-q.general.perf
ratio.and.q<-data.frame(q=q,
                        ratio=names(q))
##sim1.f1: increase ES, fixed n , k=2 -----------------------

r2=.09
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 10, by=0.1)
iter<-10000

models <- c("normal")
complement<-TRUE

H1<-"V1>V2"


sim1.f1<-array(NA,
            dim=c(iter, length(q), 2),
            dimnames = list(c(1:iter),
                            paste0("q = ",round(q,3)),
                            c("f1", "fc")
            ))


for(s in 1:length(ratio1)){
  
  ratio_beta<-c(ratio1[s],1)
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFic 
    mod<-gen_dat(r2=r2,
                 betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                 rho=cormat(pcor, length(ratio_beta)),
                 n=n,
                 "normal")%$%
      lm(Y ~ V1 + V2)
    
    sim1.f1[i,s,]<-bain(mod, hypothesis = H1)%$%fit$Fit[c(1,3)]
  }
  
}
save(sim1.f1, file="Outputs/variation of fit/sim1.f1.RData")

## sim2.f1: fixed ES, increase n, k=2 -----------------------

r2=.09
pcor<-0.3
n<-seq(50,1000, by=50)
ratio1<-1.8 #corresponds to q=.21
ratio_beta<-c(ratio1,1)
iter<-10000

models <- c("normal")
complement<-TRUE

H1<-"V1>V2"


sim2.f1<-array(NA,
               dim=c(iter, length(n), 2),
               dimnames = list(c(1:iter),
                               paste0("n = ",n),
                               c("f1", "fc")
               ))


for(s in 1:length(n)){

  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain fit
    mod<-gen_dat(r2=r2,
                 betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                 rho=cormat(pcor, length(ratio_beta)),
                 n=n[s],
                 "normal")%$%
      lm(Y ~ V1 + V2)
    
    sim2.f1[i,s,]<-bain(mod, hypothesis = H1)%$%fit$Fit[c(1,3)]
  }
  
}
save(sim2.f1, file="Outputs/variation of fit/sim2.f1.RData")


##sim3.f1: increase ES, fixed n , k=3 -----------------------
q<-q.general.perf
q<-q[1:21]
r2=.16
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 3, by=0.1)+1
ratio3<-seq(0, 2, by=0.1) %>% rev()
iter<-10000

models <- c("normal")
complement<-TRUE

H1<-"V1>V2>V3"



sim3.f1<-array(NA,
               dim=c(iter, length(q), 2),
               dimnames = list(c(1:iter),
                               paste0("q = ",round(q,3)),
                               c("f1", "fc")
               ))


for(s in 1:length(ratio1)){
  
  ratio_beta<-c(ratio1[s],2,ratio3[s])
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s, ", Iteration i:", i))
    
    #obtain BFic 
    mod<-gen_dat(r2=r2,
                 betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                 rho=cormat(pcor, length(ratio_beta)),
                 n=n,
                 "normal")%$%
      lm(Y ~ V1 + V2 +V3)
    
    sim3.f1[i,s,]<-bain(mod, hypothesis = H1)%$%fit$Fit[c(1,3)]
  }
  
}

save(sim3.f1, file="Outputs/variation of fit/sim3.f1.RData")


# Analysis --------------------------
## sim1 ---------------------
q<-q.general.perf

## Hists base R ----------------
#fit Hi
par(mfrow = c(1, 1))
par(mfrow = c(4, 5))
for(s in c(1:20)){
  sim1.f1[,s,1] %>%
    hist(xlab="fit",
         main=paste0("Histogram of fit_i for q = ", round(q[s], 2), ", eta = ", q[s])
    )
}

#fit Hc
par(mfrow = c(1, 1))
par(mfrow = c(4, 5))
for(s in c(1:20)){
  sim1.f1[,s,2] %>%
    hist(xlab="fit",
         main=paste0("Histogram of fit_c for q = ", round(q[s], 2), ", eta = ", q[s])
    )
}

## hist + density ggplot ---------------------------

#ggplot histograms of the above
gg.df<-sim1.f1[,,1] %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(),
               names_to = "ES",
               values_to = "fit"
               ) %>%
  mutate(q=rep(q, times=10000),
         ES=as.factor(ES)) %>% 
  filter(q<0.37) 

gg.df<-sim1.f1[,,1] %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(),
               names_to = "ES",
               values_to = "fit"
  ) %>%
  mutate(q=rep(q, times=10000),
         ES=as.factor(ES)
         ) %>% 
  filter(q<.29)

gg.hist<-ggplot()+
        geom_histogram(data=gg.df,bins=50,colour="black",size=0.2, fill="gray",
                       mapping=aes(x=fit,y=..density..))+
        geom_density()+
        facet_wrap(~ES,ncol=5, nrow=3)+
        theme_minimal()
gg.hist


#Beta density exploration (alpha and beta) --------------------
#Goal: find alpha and beta coefficients that are a function of ES and n such that 
# the Beta density has the form of the fit histograms 
# (Later on probably complexity should be incorporated as well)

## sim1.f1: modelling q---------------
# alpha must increase with ES
# beta must decrease with ES
#   - should they increase and decrease at the same rate?
#   - or should alpha increase more than beta?


models <- c("normal")
complement<-TRUE

H1<-"V1>V2"
### exploration ----------------

dens.df<-matrix(NA, nrow=length(x), ncol = length(q), dimnames = list(x,q))
n<-632

alpha<-c()
beta<-c()
for(i in 1:ncol(dens.df)){
  alpha[i]=1+q[i]*n/100
  beta[i]=1/(1+100*q[i]^(2.2+q[i]*1.76))
  dens.df[,i]<-dbeta(x, alpha[i], beta[i])
}


gg.dens.df<-dens.df %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var="x") %>% 
  pivot_longer(cols = !contains("x"),
               names_to = "q",
               values_to = "likelihood"
  )  %>% 
  mutate(n=n,
         q=as.numeric(q),
         alpha=1+q*n/100,
         beta=1-q) %>% 
  filter(q<.29)

#overlay observed density with beta density
ggplot()+
  geom_density(data=gg.df,mapping=aes(x=fit))+
  facet_wrap(~ES)+
  theme_minimal()+
  geom_line(data=gg.dens.df,
            mapping=aes(x=as.numeric(x),
                        y=likelihood,
                        group=as.factor(q),
                        colour="red"))+
  geom_text(data=gg.dens.df,
            mapping=aes(label=paste("a=",round(alpha,2), "\nb=",round(beta,2)), x=0.3, y=13),
            size=3)+
  scale_y_continuous(limits = c(0,15))+
  facet_wrap(~as.factor(q),ncol=5, nrow=3)

### functional exploration ------------------
###function to plot the observed vs. beta density
compare.densities<-function(a,b, #alpha and beta parameters of the beta density
                            filter.q="q<.29", #a subset of q (ES) to show plots for
                            hyp=c("i", "c")
                            ){
  
  q<-q.general.perf
  r2=.09
  pcor<-0.3
  n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
  ratio1<-seq(1, 10, by=0.1)
  iter<-10000
  x<-seq(from=0.01, to=0.99, by=0.01)
  
  s<-ifelse(hyp=="i",1,2)
  
  gg.df<-sim1.f1[,,1] %>% 
    as.data.frame() %>% 
    pivot_longer(cols = everything(),
                 names_to = "ES",
                 values_to = "fit"
    ) %>%
    mutate(q=rep(q, times=10000),
           ES=as.factor(ES)
    ) %>% 
    filter(eval(parse(text=filter.q)))
  
  dens.df<-matrix(NA, nrow=length(x), ncol = length(q), dimnames = list(x,q))
  
  alpha<-c()
  beta<-c()
  for(i in 1:ncol(dens.df)){
    alpha[i]=eval(parse(text=str_replace_all(a, "q", "q[i]")))
    beta[i]=eval(parse(text=str_replace_all(b, "q", "q[i]")))
    dens.df[,i]<-dbeta(x, alpha[i], beta[i])
  }
  
 
  gg.dens.df<-dens.df %>% 
    as.data.frame() %>%
    tibble::rownames_to_column(var="x") %>% 
    pivot_longer(cols = !contains("x"),
                 names_to = "q",
                 values_to = "likelihood"
    )  %>% 
    mutate(n=n,
           q=as.numeric(q),
           alpha=eval(parse(text=a)),
           beta=eval(parse(text=b))) %>% 
    filter(eval(parse(text=filter.q)))
  
  #overlay observed density with beta density
  ggplot()+
    geom_histogram(data=gg.df,bins=50,colour="grey",size=0.2, fill="lightgray",
                   aes(x=fit,y=..density..))+
    geom_density(data=gg.df,mapping=aes(x=fit))+
    facet_wrap(~ES)+
    theme_minimal()+
    geom_line(data=gg.dens.df,
              mapping=aes(x=as.numeric(x),
                          y=likelihood,
                          group=as.factor(q),
                          colour="red"))+
    geom_text(data=gg.dens.df,
              mapping=aes(label=paste("a=",round(alpha,2), "\nb=",round(beta,2)), x=0.3, y=13),
              size=3)+
    scale_y_continuous(limits = c(0,15))+
    facet_wrap(~as.factor(q))
  
}

compare.densities(a="1+q*n/100", b="1/(1+n/100*q)", "q<.40")

compare.densities(a="1+q*n/100", b="1/(1+100*q^(2.2+q*1.76))", "0.01<q & q<.40", hyp="i")

compare.densities(b="1+q*n/100", a="1/(1+100*q^(2.2+q*1.76))", "0.01<q & q<.30", hyp="c")



## sim2.f1: modelling n ------------------------------------------

sim2.f1
n<-seq(50,1000, by=50)
ratio1<-1.8 #corresponds to q=.21
ratio_beta<-c(ratio1,1)
q<-q[names(q)=="1.8:1"]

compare.densities.n<-function(a,b, #alpha and beta parameters of the beta density
                            filter.n="n<=600", #a subset of q (ES) to show plots for
                            hyp=c("i", "c")
){
  x<-seq(from=0.01, to=0.99, by=0.01)
  
  s<-ifelse(hyp=="i",1,2)
  gg.df<-sim2.f1[,,1] %>% 
    as.data.frame() %>% 
    pivot_longer(cols = everything(),
                 names_to = "sample.size",
                 values_to = "fit"
    ) %>%
    mutate(n=rep(n, times=10000),
           sample.size=as.factor(sample.size),
           q=q
    ) %>% 
    filter(eval(parse(text=filter.n)))
  
  dens.df<-matrix(NA, nrow=length(x), ncol = length(n), dimnames = list(x,n))
  
  alpha<-c()
  beta<-c()
  #modify the a and b input strings such that they specify [i] (needed in the for-loop below)
  a.mod<-str_replace_all(a, "n", "n[i]")
  b.mod<-str_replace_all(b,"n", "n[i]")
  for(i in 1:ncol(dens.df)){
    alpha[i]=eval(parse(text=a.mod))
    beta[i]=eval(parse(text=b.mod))
    dens.df[,i]<-dbeta(x, alpha[i], beta[i])
  }

  
  gg.dens.df<-dens.df %>% 
    as.data.frame() %>%
    tibble::rownames_to_column(var="x") %>% 
    pivot_longer(cols = !contains("x"),
                 names_to = "n",
                 values_to = "likelihood"
    )  %>% 
    mutate(q=q,
           n=as.numeric(n),
           alpha=eval(parse(text=a)),
           beta=eval(parse(text=b))) %>% 
    filter(eval(parse(text=filter.n)))
  
  #overlay observed density with beta density
  ggplot()+
    geom_histogram(data=gg.df,bins=50,colour="grey",size=0.2, fill="lightgray",
                   aes(x=fit,y=..density..))+
    geom_density(data=gg.df,mapping=aes(x=fit))+
    facet_wrap(~sample.size)+
    theme_minimal()+
    geom_line(data=gg.dens.df,
              mapping=aes(x=as.numeric(x),
                          y=likelihood,
                          group=as.factor(n),
                          colour="red"))+
    geom_text(data=gg.dens.df,
              mapping=aes(label=paste("a=",round(alpha,2), "\nb=",round(beta,2)), x=0.3, y=13),
              size=3)+
    scale_y_continuous(limits = c(0,15))+
    facet_wrap(~as.factor(n))
  
}

compare.densities.n(a="1+q*n/100",
                    b="1/(1+n^(0.715)*q^(2.2+q*1.76))",
                    filter.n="n<=1100", #a subset of q (ES) to show plots for
                    hyp="i")

## Interim validation -----------------------------
#validate the specified Beta density with alpha and beta parameters as functions of ES and n on a 
#simulation in which both n and ES are varied
q<-q.general.perf
q<-q[1:71]
r2=.09
pcor<-0.3
n<-seq(50,1000, by=50)
ratio1<-seq(1, 10, by=0.1)[1:71]
iter<-1000

models <- c("normal")
complement<-TRUE

H1<-"V1>V2"



sim12.f1<-array(NA,
               dim=c(iter, length(q), length(n)),
               dimnames = list(c(1:iter),
                               paste0("q = ",round(q,3)),
                               paste0("n = ",n)
               ))
dim(sim12.f1)
for(s in 1:length(n)){
  
  for(j in 1:length(ratio1)){
    
  ratio_beta<-c(ratio1[j],1)
  
  for(i in 1:iter){
    
    print(paste("Condition s:", s,"Condition j:",j ,", Iteration i:", i))
    
    #obtain BFic 
    mod<-gen_dat(r2=r2,
                 betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                 rho=cormat(pcor, length(ratio_beta)),
                 n=n[s],
                 "normal")%$%
      lm(Y ~ V1 + V2)
    
    sim12.f1[i,j,s]<-bain(mod, hypothesis = H1)%$%fit$Fit[1]
    
  } # end i loop for iterations
 }# end j loop for ratio
}# end s loop for n 

#sim12.f1.i10000<-sim12.f1
#save(sim12.f1.i10000, file="Outputs/variation of fit/sim12.f1.i10000.RData")

#save(sim12.f1, file="Outputs/variation of fit/sim12.f1.RData")
sim12.f1[,,1]
















## sim3.f1 modelling complexity -------------------------
q<-q.general.perf
q<-q[1:21]
r2=.16
pcor<-0.3
n<-632 #(this n has 80% power to support Hi for R2=.02 and q=.11)
ratio1<-seq(1, 3, by=0.1)+1
ratio3<-seq(0, 2, by=0.1) %>% rev()
iter<-10000

H1<-"V1>V2>V3"

ratios<-paste0(ratio1,":", 2,":",ratio3)
ratio.and.q

### the R2 is different in sim3 so it is necessary to compute the corresponding q-s
ratio.and.q.3pred<-data.frame(ratio=ratios,
                              q.V1V2=NA,
                              q.V2V3=NA,
                              q.V1V3=NA
                              )
  
for(s in 1:length(ratio1)){
  
  print(paste0("Iteration:",s))
  ratio_beta<-c(ratio1[s],2,ratio3[s])
  
  m<-gen_dat(r2=r2,
             betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
             rho=cormat(pcor, length(ratio_beta)),
             n=1000000,
             "normal")%$%
    lm(Y ~ V1 + V2 + V3) %>%
    summ(part.corr=TRUE)
  
  part.cor1<-m$coeftable[2,5]
  part.cor2<-m$coeftable[3,5]
  part.cor3<-m$coeftable[4,5]
  
  z1<-log((1+part.cor1)/(1-part.cor1))
  z2<-log((1+part.cor2)/(1-part.cor2))
  z3<-log((1+part.cor3)/(1-part.cor3))
  
  ratio.and.q.3pred[s,2] =z1-z2
  ratio.and.q.3pred[s,3]=z2-z3
  ratio.and.q.3pred[s,4]=z1-z3
  
  
}
#there is no asymmetry between the effect sizes below and above 2
plot(ratio.and.q.3pred$q.V1V2, ratio.and.q.3pred$q.V2V3)


colnames(sim3.f1)<-paste0("q = ", round(ratio.and.q.3pred$q.V1V2,3))



