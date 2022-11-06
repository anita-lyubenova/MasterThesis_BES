library(tidyverse)

 
# Simulated dist of the fit -------------------------------------------------
load("Outputs/exploration/q.general.perf.RData")
q<-q.general.perf
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


##sim1.f1: increase ES, fixed n , k=3 -----------------------
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

### Base R histograms ----------------
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

# Ggplot hist with density ---------------------------

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



##overlayed histograms of the fit of Hi and Hc -- useless -----------------
# h<-c("i", "c")
# sim1.f1.long<-matrix(NA, nrow = 910000)
# l.sim1.f1<-data.frame(q=rep(q, times=10000))
# for(i in 1:2){
#   
#   l.sim1.f1<-cbind(l.sim1.f1, 
#     sim1.f1[,,i] %>% 
#       as.data.frame() %>% 
#       pivot_longer(cols = everything(),
#                    names_to = paste0("ES_", h[i]),
#                    values_to = paste0("fit_", h[i])
#       )
#   )
# }
# 
# colnames(l.sim1.f1)<-c("q"  ,   "ES"  ,  "fit_i", "ES"  ,  "fit_c")
# 
# 
# hist.df<-l.sim1.f1 %>% 
#   select(-c(ES_c)) %>% 
#   filter(q<0.29) %>% 
#   pivot_longer(cols=c("fit_i", "fit_c"),
#                names_to = "H",
#                values_to = "fit") %>% 
#   mutate(hypothesis=H1) 
#   
#   ggplot(hist.df,aes(x = fit, fill=H))+
#   geom_histogram(bins = 50, colour="black", alpha=0.5)+
#   facet_wrap(~ES_i)




  




## -----------------------

#Beta density exploration (alpha and beta) --------------------
#Goal: find alpha and beta coefficients that are a function of ES and n such that 
# the Beta density has the form of the fit histograms 
# (Later on probably complexity should be incorporated as well)

##Sim1.f1: increasing ES, fixed n=632---------------
# alpha must increase with ES
# beta must decrease with ES
#   - should they increase and decrease at the same rate?
#   - or should alpha increase more than beta?
gg.hist

x<-seq(from=0.01, to=0.99, by=0.01)
plot(dbeta(x, 1,0.01),type="l")


beta.par<-seq(1,0.77,by=-0.01)

par(mfrow = c(1, 1))
par(mfrow = c(5, 5))
for(s in 1:length(beta.par)){
  plot(dbeta(x, 1,beta.par[s]),type="l",
       main = paste0("alpha=1", "; beta=",beta.par[s]),
       ylim = c(0.6,1.4)
       )
  
}



alpha.par<-seq(1,1.23,by=0.01)

par(mfrow = c(1, 1))
par(mfrow = c(5, 5))
for(s in 1:length(beta.par)){
  plot(dbeta(x, alpha.par[s],beta.par[s]),
       type="l",
       main = paste0("alpha=",alpha.par[s], "; beta=",beta.par[s]),
       ylim = c(0.6,1.4)
       )
  
}

## Density df ------------------
x<-seq(from=0.01, to=0.99, by=0.01)
dens.df<-matrix(NA, nrow=length(x), ncol = length(q), dimnames = list(x,q))
n<-632

for(i in 1:ncol(dens.df)){
  dens.df[,i]<-dbeta(x, 1+q[i], 1-q[i])
}

for(i in 1:ncol(dens.df)){
  dens.df[,i]<-dbeta(x, 1+q[i]*n/100, exp(q[i]))
}


alpha<-c()
beta<-c()
for(i in 1:ncol(dens.df)){
  alpha[i]=1+q[i]*n/100
  beta[i]=q[i]/(1+exp(q[i]))
  dens.df[,i]<-dbeta(x, alpha[i], beta[i])
}

compare.densities(a="1+q*n/100", b="1/(1+6*q)")

#gg.dens.df<-dens.df %>% 
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
  facet_wrap(~ES,ncol=5, nrow=3)+
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

#function to plot the observed vs. beta density
compare.densities<-function(a,b, filter.q="q<.29"){
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

compare.densities(a="1+q*n/100", b="1/(1+100*q^(2.2+q*1.76))", "0.30<q & q<.40")


  


