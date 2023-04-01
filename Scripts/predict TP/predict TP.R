# Create dataset model TP as a function of 
# [population definition] R2, pcor, diff.beta, p (heterogeneity: SD_betas<-p*beta),
# [sample definition]     n, t
# [hypothesis definition] complexity, n.par

library(tidyverse)
library(bain)
library(magrittr)
load("Outputs/generate datasets/TRUE_H1.RData")
n<-c(25,100,200,250,350,500, 800)
iter<-1000
studies<-40

#label the names of the list elements corresponding to different sample sizes n
names(TRUE_H1)<-paste0("n", n)

#label the names of the list elements corresponding to different study numbers t
TRUE_H1<-lapply(TRUE_H1, FUN=function(x) {
  names(x)<-paste0("t",rep(1:studies, times=iter))
  return(x)
    }
  )

TRUE_H1$n25[names(TRUE_H1$n25)=="t1"]

#from each data.frame (at the lowest level) I obtain BFiu for hypothesis Hi
# V1 > V2 > V3 >0  :: ci=0.0118714 
# (V1,V2,V3)>0     :: ci=0.0778
# V1 > V2 > V3     :: ci=0.1354515
# V1> V2 & V3>0    :: ci=0.2754828
# V1 > (V2,V3)     :: ci=0.3104396
# V1+V2+V3>0       :: ci=0.5


lm(Y~., data=TRUE_H1$n25$t1)%>% 
  bain(hypothesis = "V1>V2+V3")%$%fit %>%     #$BF.u[c(1,2,4)]
  extract(c(1, nrow(.)),c("Com","BF.u")) %>%  #subset only BFiu for the specified hypothesis and the complement
  rbind(.,1) #add BFuu = 1


a<-lapply(TRUE_H1[c("n25", "n50")], function(n){
  lapply(n, function(t){
    lm(Y~., data=t)%>% 
      bain(hypothesis = "V1>V2>V3")%$%fit %>%     #$BF.u[c(1,2,4)]
      extract(c(1, nrow(.)),c("Com","BF.u")) %>%  #subset only BFiu for the specified hypothesis and the complement
      rbind(.,1) #add BFuu = 1
      }
    )
  }
)

data.frame(R2=0.13,
           pcor=0.3,
           diff.betas=0.07
           )















