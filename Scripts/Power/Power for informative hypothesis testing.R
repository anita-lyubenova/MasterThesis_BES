# I had this weird idea that when about quantifing power of an informativev hypotehsis test
# In a test of Ha: b1>b2>b3 vs. Hu, do not fix the true values for b1, b2 and b3 to sample
# from a mvnorm(Ha=TRUE).
# Instead sample values for b1, b2 and b3


library(tmvtnorm)
source("scripts/ThomVolker scripts/functions.R")


pcor<-0.3
hypothesis<-"V1=V2=V3; V1>V2>V3"
models <- c("normal")
complement<-TRUE

n<-c(350)
iter<-10000
#Simulations ----------------------------------------------

##Hu = TRUE ----------------
#  :: no restrictions on the sampled betas

Hu<-array(NA,
          dim=c(iter, 8, length(n)),
          dimnames = list(1:iter,
                          c("b1", "b2", "b3", "r2","BF0u", "BFiu", "BFcu", "BFuu"),
                          n
                          )
          )


for(i in 1:iter){

  for(s in 1:length(n)){
    betas<-rtmvnorm(1, mean = rep(0, 3), 
             sigma = diag(3),
             lower=rep(-1, length = 3), 
             upper=rep( 1, length = 3),
             D = diag(3)) %>% as.numeric() %>% abs()
    #this is the variance of the residual errors;
    var<-1/rgamma(1,shape=0.8,rate=5)
    r2<-1/(var+1)
    #hist(1/(var+1))
         
    BFs<-gen_dat(r2=r2,
              betas=betas,
              rho=cormat(pcor, length(betas)),
              n=n[s],
              "normal")%$%
      lm(Y ~ V1 + V2 +V3) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
    
    Hu[i, ,s]<-c(betas, r2, BFs,1)
  
 }
} #end iterations loop i



##Hi = TRUE ----------------
#  :: no restrictions on the sampled betas

Hi<-array(NA,
          dim=c(iter, 8, length(n)),
          dimnames = list(1:iter,
                          c("b1", "b2", "b3", "r2","BF0u", "BFiu", "BFcu", "BFuu"),
                          n
          )
)


for(i in 1:iter){
  
  for(s in 1:length(n)){
    
    betas<-c(0,0,0)
    repeat  {
      betas<-rtmvnorm(1, mean = rep(0, 3), 
                      sigma = diag(3),
                      lower=rep(-1, length = 3), 
                      upper=rep( 1, length = 3),
                      D = diag(3)) %>% as.numeric() %>% abs()
      
      if(betas[1]>betas[2] & betas[2]>betas[3]) break
      
      print("stuck")
    }
    
    #this is the variance of the residual errors;
    var<-1/rgamma(1,shape=0.8,rate=5)
    r2<-1/(var+1)
    #hist(1/(var+1))
    
    BFs<-gen_dat(r2=r2,
                 betas=betas,
                 rho=cormat(pcor, length(betas)),
                 n=n[s],
                 "normal")%$%
      lm(Y ~ V1 + V2 +V3) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
    
    Hi[i, ,s]<-c(betas, r2, BFs,1)
    
  }
} #end iterations loop i

##Hc = TRUE ----------------
#  :: no restrictions on the sampled betas

Hc<-array(NA,
          dim=c(iter, 8, length(n)),
          dimnames = list(1:iter,
                          c("b1", "b2", "b3", "r2","BF0u", "BFiu", "BFcu", "BFuu"),
                          n
          )
)


for(i in 1:iter){
  
  for(s in 1:length(n)){
    
    betas<-c(0,0,0)
    repeat  {
      betas<-rtmvnorm(1, mean = rep(0, 3), 
                      sigma = diag(3),
                      lower=rep(-1, length = 3), 
                      upper=rep( 1, length = 3),
                      D = diag(3)) %>% as.numeric() %>% abs()
      
      if(!(betas[1]>betas[2] & betas[2]>betas[3])) break
    
    }
    
    #this is the variance of the residual errors;
    var<-1/rgamma(1,shape=0.8,rate=5)
    r2<-1/(var+1)
    #hist(1/(var+1))
    
    BFs<-gen_dat(r2=r2,
                 betas=betas,
                 rho=cormat(pcor, length(betas)),
                 n=n[s],
                 "normal")%$%
      lm(Y ~ V1 + V2 +V3) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
    
    Hc[i, ,s]<-c(betas, r2, BFs,1)
    
  }
} #end iterations loop i

##H0 = TRUE ----------------
#  :: no restrictions on the sampled betas

H0<-array(NA,
          dim=c(iter, 8, length(n)),
          dimnames = list(1:iter,
                          c("b1", "b2", "b3", "r2","BF0u", "BFiu", "BFcu", "BFuu"),
                          n
          )
)


for(i in 1:iter){
  
  for(s in 1:length(n)){
    
    betas<-c(0,0,0)
    repeat  {
      betas<-rtmvnorm(1, mean = rep(0, 3), 
                      sigma = diag(3),
                      lower=rep(-1, length = 3), 
                      upper=rep( 1, length = 3),
                      D = diag(3)) %>% as.numeric() %>% abs()
      
      if(abs(betas[1]-betas[2])<0.05 & abs(betas[2]-betas[3])<0.05 & abs(betas[1]-betas[3])<0.05) break
    }
    
    #this is the variance of the residual errors;
    var<-1/rgamma(1,shape=0.8,rate=5)
    r2<-1/(var+1)
    #hist(1/(var+1))
    
    BFs<-gen_dat(r2=r2,
                 betas=betas,
                 rho=cormat(pcor, length(betas)),
                 n=n[s],
                 "normal")%$%
      lm(Y ~ V1 + V2 +V3) %>%
      bain(hypothesis = hypothesis)%$%fit$BF.u[c(1,2,4)]
    
    H0[i, ,s]<-c(betas, r2, BFs,1)
    
  }
} #end iterations loop i



save.image("Outputs/workspace_power for informative hypotheses.RData")


load("Outputs/workspace_power for informative hypotheses.RData")

dim(Hi)

hist(Hi[,4,1])
BFicu<-Hi[,6:8,, drop=FALSE]
PMPicu<-BFicu[,,1]
for(i in 1:nrow(BFicu)){
  for(h in 1:ncol(BFicu)){
    PMPicu[i,h]<-BFicu[i,h,]/sum(BFicu[i,,])
  }
}
colnames(PMPicu)<-c("PMP_i","PMP_c", "PMP_u")
x<-PMPicu[1,]
x[1]
sum(apply(PMPicu,1,function(x) x[1]>x[2] & x[1]>x[3]))/iter
1/32
1-0.1605618

0.16*2
1-0.16
0.32/(0.32+0.84)
