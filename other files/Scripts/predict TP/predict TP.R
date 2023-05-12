# Create dataset model TP as a function of 
# [population definition] R2, pcor, diff.beta, p (heterogeneity: SD_betas<-p*beta),
# [sample definition]     n, t
# [hypothesis definition] complexity, n.par


# test.RData ---------------------------------------------------------------
load("Outputs/generate datasets/test.RData")

iter<-10
studies=4
hypothesis="V1>V2>V3"



TP<-
  computeBFs(test,
             hypothesis = hypothesis,
             n.cores = 3,
             n=c(25,100),
             studies=4,
             iter=10
  ) %>% 
  aggregatePMP(c("H1","Hu"), studies = 3) %>% 
  TPRi(true_pop = "H1")



add_info(TP, 
         R2=0.13,
         pcor=0.3,
         complexity = get_complexity(data=test,hypothesis = "V1>V2>V3"),
         ratio_beta=c(3,2,1)
         )


#TRUE_H1.RData --------------------------------------------------------
#..failed
load("Outputs/generate datasets/TRUE_H1.RData")

n <-  c(25,100,200,250,350,500, 800)
studies<-40
iter<-1000
hypothesis="V1>V2>V3"


TP1_TRUE_H1<-
  computeBFs(TRUE_H1,
             hypothesis = hypothesis,
             n.cores = 7,
             n=n,
             studies=40,
             iter=1000
  ) %>% 
  aggregatePMP(c("H1","Hu"), studies = 3) %>% 
  TPRi(true_pop = "H1")

save(TP1_TRUE_H1, file = "Outputs/predict TP/data/TP1_TRUE_H1.RData")

#chuck-wise processing ----------------------------------
#running computeBFs on the whole TRUE_H1 data is too computationally intensive,
# so I'll try to see if there are optimal chuncks to process faster

library(tidyverse)
library(bain)
library(magrittr)
library(parallel)
library(foreach)
library(doParallel)
library(doRNG)
library(doSNOW)
load("Outputs/generate datasets/TRUE_H1.RData")

# Summary of results (with 7 cores):
# 1000 : 52.92 75.80 f
# 2000 : 100.05; 177.81(.inorder=TRUE); 124.19 (.inorder=FALSE)
# 2000 + dynamic load balancing 169.99 ..not efficient
# 3000 : 232.84 #not efficient anymore
# 3000 + dynamic load balancing : 218.44  # the benefit is not too much => load balancing is not the problem
# 10 000 iterations did not run

#It seems 2000 is the most efficient
# 158.08 (chunkSize=10)
# 120.84  (chunkSize=10 + dynamic load balancing)
# 107.70  (chunkSize=10 + dynamic load balancing + decreasing n)
# 148.60 (chunkSize=20 + dynamic load balancing + decreasing n)

# 4000 323.40 (chunkSize=10 + dynamic load balancing + decreasing n)
# 4000 249.53 (chunkSize=20 + dynamic load balancing + decreasing n)
# 8000 617.39  (chunkSize=50 + dynamic load balancing + decreasing n)
# 8000 604.40 (chunkSize=40 + dynamic load balancing + decreasing n)
# 8000 593.49 (chunkSize=30 + dynamic load balancing + decreasing n)

#subset 
data <-lapply(TRUE_H1, function(x) x<-x[1:16000] )
n <-  c(25,100,200,250,350,500, 800)
studies<-16
iter<-1000
hypothesis="V1>V2>V3"
n.cores=7

system.time({
cl <- makeCluster(n.cores)


# clusterEvalQ(cl, {
#   library(tidyverse)
#   library(abind)
# })

registerDoSNOW(cl)
#add progress bar
iterations <- length(n)*iter*studies # total number of conditions to iterate over
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)
opts.nws <- list(chunkSize=30)
mcoptions <- list(preschedule = FALSE) #dynamic load balancing - does not lead to much improvement
#1) Compute Bfs

  #BF_list is a list of dataframes (containing BFs), where each data frame has studies*iter rows
  #i.e., the data frames contain all BFs within this sample size s
  BF_list <-
    foreach(s = seq(from=length(n),to=1, by=-1), #1:length(n),
            .options.snow = opts,
            .options.nws=opts.nws,
            .options.multicore = mcoptions,
            .packages = c("bain", "magrittr", "dplyr")
    )%:%
      foreach(i = 1:(studies*iter),
              .combine = rbind
      ) %dopar% {
        lm(Y~., data=data[[s]][[i]])%>% 
          bain(hypothesis = hypothesis)%$%fit %>%     #$BF.u[c(1,2,4)]
          extract(c(1, nrow(.)),c("BF.u")) %>%  #subset only BFiu for the specified hypothesis and the complement
          c(.,"BFuu"=1)
    }
})
close(pb)
stopCluster(cl)


 
#TRoubleshooting ---------------------------
#why doe smy code run a lot slower with increasing number of iterations

## Threads? ---------------------------------
#Remember, you’ll need to make sure that every package you use on your child processes 
#is running in single-threaded mode. Packages usually tell you if they use multiple
# threads like data.table does.

tools::package_dependencies("bain")

