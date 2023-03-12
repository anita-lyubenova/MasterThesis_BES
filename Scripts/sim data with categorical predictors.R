source("scripts/ThomVolker scripts/functions.R")

# simulating lm with continuouso predictors
r2<-0.13
ratio<-c(2,1)
rho<-cormat(0.3, length(ratio))

betas<-sqrt(r2 / sum(ratio %*% t(ratio) * rho)) * ratio

X <- mvrnorm(1000, mu = rep(0, length(betas)), Sigma = rho)

Y <- X %*% betas + rnorm(n = 1000, mean = 0, sd = sqrt(1 - r2))



# INPUT: ratio ------------------------------------------------------------


## 2 levels -----------------------------------
r2<-0.13
ratio<-c(2,1)
rho<-cormat(0, 2)
n<-200000

#group<-rbinom(n=n, 1, 0.5)
group<-c(rep(0, 100000), rep(1, 100000))
X<-data.frame(exp = ifelse(group==1, 1,0),
              contr = ifelse(group==0, 1, 0)) %>% as.matrix()

betas<-sqrt(length(ratio)*r2 / sum(ratio %*% t(ratio) * rho)) * ratio #3.597*
Y<-X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))

dat<-X %>% 
  as.data.frame() %>% 
  mutate(DV=Y)

lm(DV ~ 0 + exp + contr, data = dat) %>% summary()


dat %>% 
  group_by(exp) %>% 
  summarise(mean=mean(DV))

## 3 levels -------------------------------------------
r2<-0.09
n<-150000
ratio<-c(3,2,1)
rho<-cormat(0, length(ratio))

group<-c(rep(0, 50000), rep(1, 50000), rep(2, 50000))

X<-data.frame(G0 = ifelse(group==0, 1,0),
              G1 = ifelse(group==1, 1, 0),
              G2 = ifelse(group==2, 1, 0)) %>% as.matrix()


betas<-sqrt(length(ratio)*r2 / sum(ratio %*% t(ratio) * rho)) * ratio #3.597*
Y<-X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))

dat<-X %>% 
  as.data.frame() %>% 
  mutate(DV=Y)

lm(DV ~ 0 + G0 + G1+G2, data = dat) %>% summary()


## 4 levels --------------------------------------------
r2<-0.13
n<-200000
ratio<-c(4,3,2,1)
rho<-cormat(0, length(ratio))

group<-c(rep(0, 50000), rep(1, 50000), rep(2, 50000),rep(3, 50000))

X<-data.frame(G0 = ifelse(group==0, 1,0),
              G1 = ifelse(group==1, 1, 0),
              G2 = ifelse(group==2, 1, 0),
              G3 = ifelse(group==3, 1, 0)) %>% as.matrix()


betas<-sqrt(length(ratio)*r2 / sum(ratio %*% t(ratio) * rho)) * ratio #3.597*
Y<-X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))

dat<-X %>% 
  as.data.frame() %>% 
  mutate(DV=Y)

lm(DV ~ 0 + G0 + G1+G2+G3, data = dat) %>% summary()


## 5 leves --------------------------------------------
r2<-0.50
n<-250000
ratio<-c(5,4,3,2,1)
rho<-cormat(0, length(ratio))

group<-c(rep(0, 50000), rep(1, 50000), rep(2, 50000),rep(3, 50000),rep(4, 50000))

X<-data.frame(G0 = ifelse(group==0, 1,0),
              G1 = ifelse(group==1, 1, 0),
              G2 = ifelse(group==2, 1, 0),
              G3 = ifelse(group==3, 1, 0),
              G4 = ifelse(group==4, 1, 0)
              ) %>% as.matrix()


betas<-sqrt(length(ratio)*r2 / sum(ratio %*% t(ratio) * rho)) * ratio #3.597*
Y<-X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))

dat<-X %>% 
  as.data.frame() %>% 
  mutate(DV=Y)

lm(DV ~ 0 + G0 + G1+G2 +G3 +G4, data = dat) %>% summary()

#equal variances
dat %>% 
  mutate(group=group) %>% 
  group_by(group) %>% 
  summarise(var=var(DV))


# INPUT: Cohen's d------------------------------------------------------------------

## attempt 1 - failed -----------------
r2<-0.13
d<-0.3
r.XY<-0.489
n<-100000

X1<-rbinom(n,1,0.5)
X0<-1-X


Y<-rnorm(n, r.XY*X1, sd=sqrt(1-r.XY^2))
#Y <- X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))
Y <- r2*X1 + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))



lm(Y~0+X0+X1) %>% summary()


## attempt 2 ---------------------------------------------
#two means
d<-0.3
r2=0.13

M2<-0.1 # mean of the second group
M1=d+M2
ratio<-c(M1,M2)
# ratio2<-1
# ratio1=(d+M2)/M2
# ratio<-c(ratio1,ratio2)
# rho<-cormat(0, length(ratio))

n=200000

group<-rbinom(n,1,0.5)

X<-data.frame(G0 = ifelse(group==0, 1,0),
              G1 = ifelse(group==1, 1, 0)
) %>% as.matrix()

betas<-sqrt(length(ratio)*r2 / sum(ratio %*% t(ratio) * rho)) * ratio #3.597*

Y<-X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))

dat<-X %>% 
  as.data.frame() %>% 
  mutate(DV=Y)

lm(DV ~ 0 + G0 + G1, data = dat) %>% summary()
















##################################
k<-3
R_square<-0.13
Hyp="V1>V2>V3"
ratio<-c(3,2,1)
rho<-cormat(0.3, 3)
################################
cal_beta_general<-function(k,R_square,Hyp,ratio,rho){
  varnames<-c()
  for (i in 1:k){
    varnames[i]=paste('beta',as.character(i),sep='')
  }
  
  # beta<-c(1:k)*0
  # EI_matrix1<-matrix_trans(varnames,Hyp)
  # ERr1<-EI_matrix1[[1]]
  # IRr1<-EI_matrix1[[2]]
  matches <- regmatches(Hyp, gregexpr("[[:digit:]]+", Hyp))
  index<-as.numeric(unlist(matches))
  # if(length(which(index==0))==0){
  #   beta[index]=seq(from=k,to=1,by=-1)
  #   }else if(length(which(index==0))==1){
  # NE<-index[(which(index==0)+1):(k+1)]
  # beta[NE]=seq(from=-1,to=-length(NE),by=-1)
  # beta[index[1:which(index==0)-1]]=seq(from=which(index==0)-1,to=1,by=-1)}else{}
  #
  # if(nrow(IRr1)>=k&&ncol(IRr1)>=k){
  #   if(sum(IRr1[1:k,1:k]-diag(x=1,k,k))==0){
  #     beta=seq(from=5*k-4,to=1,by=-5)
  #   }}
  #
  beta<-ratio
  beta_r<-matrix(runif(k*k),nrow = k)
  for (i in 1:k){
    for (j in 1:k){
      beta_r[i,j]<-beta[i]*beta[j]*rho[i,j] # <=> ratio %*% t(ratio) * rho)
    }
  }
  
  
  d<-sqrt(R_square/sum(beta_r)) #the smallest coefficient
  beta[index]<-d*beta
  return(beta)
}



