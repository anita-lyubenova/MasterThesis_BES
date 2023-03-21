#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [compl_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

load("Outputs/accuracy/dat_merged.RData")
source("Scripts/accuracy/accuracy_functions.R")
# H1 vs Hu
a<-aggregatePMP(dat,
                hyp=c("H1","Hu"),
                studies = 10)

a.acc<-accuracyPMP(listPMP = a,
            hyp_to_pop =c(H1="TRUE_H1", Hu="TRUE_Hc", Hu="TRUE_Hu")
            )

# VS.

#H1 vs. Hc vs. Hu
b<-aggregatePMP(dat,
                hyp=c("H1","Hc","Hu"),
                studies = 10)

b.acc<-accuracyPMP(listPMP = b,
            hyp_to_pop =c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="TRUE_Hu")
)
a.acc$acc
b.acc$acc



a.plot<-
  ggcorrplot(a.acc$acc,
             outline.col = "white",
             lab = TRUE)+
  scale_fill_gradient2(limit = c(0,1),
                       low = "blue", high =  "red",
                       mid = "white",
                       midpoint = 0.87)+
  scale_x_continuous(breaks = 1:a.acc$studies)+
  labs(title = a.acc$hypothesis_test,
       subtitle =paste("Populations:", paste(a.acc$hyp_to_pop, collapse = ", "), collapse = " ")
       )
a.plot
b.plot<-ggcorrplot(b.acc$acc,
                   outline.col = "white",
                   lab = TRUE)+
  scale_fill_gradient2(limit = c(0,1),
                       low = "blue", high =  "red",
                       mid = "white",
                       midpoint = 0.87)+
  scale_x_continuous(breaks = 1:b.acc$studies)+
  labs(title = b.acc$hypothesis_test,
       subtitle =paste("Populations:", paste(b.acc$hyp_to_pop, collapse = ", "), collapse = " ")
       )
b.plot

a.plot/b.plot



#
aggregatePMP(x=dat,
             h=c("H1" ,"Hu"),
             studies = 40) %>% 
  accuracyPMP(hyp_to_pop = c(H1="HETEROG_H1p.1", Hu="TRUE_H0")) %>% 
  acc_corrplot()/
  aggregatePMP(x=dat,
               h=c("H1" ,"Hu"),
               studies = 40) %>% 
  accuracyPMP(hyp_to_pop = c(H1="HETEROG_H1p.3", Hu="TRUE_H0")) %>% 
  acc_corrplot()



aggregatePMP(x=dat,
             h=c("H1","Hc"),
             studies = 40) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="HETEROG_H1p.5")) %>% 
  acc_corrplot()
