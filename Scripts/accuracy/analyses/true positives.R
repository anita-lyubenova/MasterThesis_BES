
load("Outputs/accuracy/dat_merged.RData")
source("Scripts/accuracy/accuracy_functions.R")



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


# TPRi and the set of tested hypotheses---------------------------------------------
#Does TPRi depend the set of tested hypotheses?
# Most definitiely it will decrease with increasing number of hypotheses.
# How much it decresaes depend probably on hte differences in complexities.
# This is too much for me to investigate => focus on Hi vs Hu

# TPRi and the Hu-population --------------------------------------------------------
#Does the TPRi depend on the population true under Hu?
#E.g. H1 vs Hu (TRUE_H0)
# and H1 vs Hu (HETEROG_H1)
# and H1 vs Hu (TRUE_Hu)

#No!^^ 
# To compute TPRi we condition on a population, eg. Hi =>
# the error 1-TPRi still does not depend on the other population.
#H1 vs Hu (TRUE_H0)
# Implication: for the prediction of TPRi you don't need to specify what is true under the alternative
aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_H0")) %>% 
  acc_corrplot(object = "TP")+

# and H1 vs Hu (TRUE_Hu)
aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_Hu")) %>% 
  acc_corrplot(object = "TP")
