

dat<-readRDS("Part I/pre-processing/output/BF_data.rds")
source("Part I/analysis/analysis functions.R")

attributes(dat)
x<-dat

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=2) %>% 
  create_median_plot_data(pop="test1",n="50") %>% 
  median_plot()

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=2) %>%
  accuracyPMP(hyp_to_pop = c(H1="test1", Hu="test2")) %>% 
  acc_corrplot(object = "TP")


