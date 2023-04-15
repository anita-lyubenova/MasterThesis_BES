library(patchwork)
# Normal heterogeneity ----------------------------------------------------------------
dat<-readRDS("Part I/pre-processing/output/BF_data.rds")
source("Part I/analysis/analysis functions.R")

dimnames(dat)
attributes(dat)

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=20) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p0_linear",n="300") %>% 
  median_plot()

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=20) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.5_linear",n="300") %>% 
  median_plot()

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=20) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.75_linear",n="300") %>% 
  median_plot()

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=20) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.86_linear",n="300") %>% 
  median_plot()

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=20) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p0_linear",n="50") %>% 
  median_plot()


#Accuracies

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
                             )) %>% 
  acc_corrplot(object = "TP")

dat %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "acc")

dat %>% 
  aggregatePMP(hyp=c("H1", "Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "acc")

dat %>% 
  aggregatePMP(hyp=c("H1", "Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "TP")

dat %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hu="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "acc")


# Logormal heterogeneity ----------------------------------------------------------------
datln<-readRDS("Part I/pre-processing/output/BF_data_lognorm.rds")
source("Part I/analysis/analysis functions.R")
dimnames(datln)

datln %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "TP")

datln %>% 
  aggregatePMP(hyp=c("H1", "Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "acc")

datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "acc")

