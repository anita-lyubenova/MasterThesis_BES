
gen_dat <- function(r2, betas, rho, n, model = c("normal", "logit", "probit"), 
                    mutate_args = NULL, select_args = quos(everything())) {
  # generate predictors X
  X <- mvrnorm(n, mu = rep(0, length(betas)), Sigma = rho)
  
  # generate outcome variable Y
  if (model == "normal") {
    Y <- X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))
  }
  if (model == "logit") {
    Y <- rbinom(n, 1, 1 / (1 + exp(-(X %*% betas))))
  }
  if (model == "probit") {
    Y <- rbinom(n, 1, pnorm(X %*% betas))
  }
  # output data
  bind_cols(X = as.data.frame(X), Y = Y) #%>%
  # mutate(!!!mutate_args) %>%
  # select(!!!select_args)
}


r2<-0.13
ratio_beta
betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")





set.seed(123)
# Simulation including all variables
n<- 10000
X <- t(rmultinom(n = n, size = 1, prob = rep(0.2,4)))
ones<-matrix(1,ncol = nrow(X), nrow = 1)

beta <- c(1,2,3,4)
e <- rnorm(n, 0, 0.25)
y <- X%*%beta + e
# Model fit
mod <- lm(y~X[,-1])
# Estimates
mod$coefficients

beta <- c(1,2,3,4)
e <- rnorm(n, 0, 0.25)
y <- X%*%beta + e
# Model fit
mod <- lm(y~X)
# Estimates
mod$coefficients

beta <- c(1,2,3,4)
e <- rnorm(n, 0, 0.25)
y <- X%*%beta + e
yX<-cbind(y,X)
# Model fit
mod <- lm(X1~., data=data.frame(yX))
# Estimates
mod$coefficients


set.seed(123)
# Simulation removing first category
n<- 1000
X <- t(rmultinom(n = n, size = 1, prob = rep(0.2,4)))
beta <- c(2,3,4)
e <- rnorm(n, 0, 0.25)
y <- X[,-1]%*%beta + e
# Model fit
mod <- lm(y~X[,-1])
# Estimates
mod$coefficients


library(alr4)
gala<-galapagos
mgala<-as.matrix(gala[,1:4])



#create an identity vector
ones<-matrix(1,ncol = nrow(gala), nrow = 1)

#combine the identity vector with the data matrix
mgala<-cbind(t(ones),mgala)

#calculate the mean of each variable
means<-(ones%*%mgala)/30

#calculate the betas
X<-mgala[,-2]
y<-mgala[,2]
betas<-solve(crossprod(X,X))%*%t(X)%*%y


#calculate the predicted values 
fitted<-X%*%betas

coefs <- function(r2, ratio, rho, model = c("normal", "logit", "probit")) {
  
  # variance of predicted values (Var(yhat))
  if (model == "normal") {
    var_y <- r2
  }
  else if (model == "logit") {
    var_y <- (r2 * pi^2 / 3) / (1 - r2)
  }
  else if (model == "probit") {
    var_y <- r2 / (1 - r2)
  }
  # value of the regression coefficients
  sqrt(var_y / sum(ratio %*% t(ratio) * rho)) * ratio
}




set.seed(123)
# Simulation including all variables
n<- 1000
X <- t(rmultinom(n = n, size = 1, prob = rep(0.2,4)))
ones<-matrix(1,ncol = nrow(X), nrow = 1)

beta <- c(1,2,3,4)
e <- rnorm(n, 0, 0.25)
y <- X%*%beta + e

lm(y~0+X)

#instead
pcor<-0.3
dat<-mvrnorm(n=n/4, mu=beta, Sigma = cormat(pcor, length(beta))) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "group", values_to = "y") %>% 
  mutate(group=as.factor(group))

dummy_dat<-dummy_cols(dat, select_columns = "group", remove_first_dummy = FALSE) 


X<-dummy_dat[,-c(1,2)] %>% as.matrix()#the matrix without the outcome variable
y<-dummy_dat[,2]%>% as.matrix() # the matrix only with the outcome variable
betas<-solve(crossprod(X,X))%*%t(X)%*%y #doing regression without intercept like this works well

#calculate the predicted values
fitted<-X%*%betas

#calculate residuals
res<-dat[,2]-fitted
var(res)


#calculate variance
var <- (1/(nrow(dat[,-1])-ncol(dat[,-1])))*t(res)%*%res
#calculate standard errors
ses<-sqrt(diag(as.numeric(var)*solve(t(X)%*%X)))


lm(y~group, data=dat)


# Model fit
mod <- lm(y~X[,-1])
# Estimates
mod$coefficients

y

