#True positives --------------------------------------------
aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_H0")) %>% 
  acc_corrplot(object = "TP")+
  
  
  aggregatePMP(x=dat,
               h=c("H1" ,"Hu"),
               studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1large", Hu="TRUE_H0")) %>% 
  acc_corrplot(object = "TP")

aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_H0")) %>% 
  acc_corrplot(object = "TP")+
  
  aggregatePMP(x=dat,
               h=c("H1" ,"Hu"),
               studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.75")) %>% 
  acc_corrplot(object = "TP")

aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_H0")) %>% 
  acc_corrplot(object = "TP")+
  aggregatePMP(x=dat,
               h=c("Hc" ,"Hu"),
               studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(Hc="TRUE_Hc", Hu="TRUE_H0")) %>% 
  acc_corrplot(object = "TP")

dimnames(dat$BF)[[3]]
