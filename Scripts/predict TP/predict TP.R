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
load("Outputs/generate datasets/TRUE_H1.RData")

#subset 
sub1000<-lapply(TRUE_H1, function(x) x<-x[1:1000] )

data = sub1000
n <-  c(25,100,200,250,350,500, 800)
studies<-10
iter<-100
hypothesis="V1>V2>V3"
n.cores=7

cl <- makeCluster(n.cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(tidyverse)
  library(abind)
})

#add progress bar
iterations <- length(n)*iter*studies # total number of conditions to iterate over
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)

#1) Compute Bfs
system.time(
  #BF_list is a list of dataframes (containing BFs), where each data frame has studies*iter rows
  #i.e., the data frames contain all BFs within this sample size s
  BF_list <-
    foreach(s = 1:length(n),
            .options.snow = opts,
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
)
close(pb)
stopCluster(cl)
 