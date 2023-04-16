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
                             Hu="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot2(object = "acc")

cp3.1504.2203_withscaling<-datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot2(object = "acc")

ggsave("Part I/analysis/output/cp3.1504.2203_withscaling.png", plot = cp3.1504.2203_withscaling, width = 7.5, height = 3.5, units = "in", dpi = 300, bg="white")


cp2.1504.2159_noscaling<- datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot3(object = "acc")

ggsave("Part I/analysis/output/cp2.1504.2159_noscaling.png", plot = cp2.1504.2159_noscaling,width = 7.5, height = 3.5, units = "in", dpi = 300, bg="white")


lp.1504.2203<-  
  datln %>% 
    aggregatePMP(hyp=c("H1","Hc","Hu"),
                               studies=30) %>% 
    accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                               Hc="Hc_r.13_pcor.3_b123_p0_linear",
                               Hu="H1_r.13_pcor.3_b321_p.86_linear"
    )) %>% 
    acc_lineplot()

ggsave("Part I/analysis/output/lp.1504.2203_noscaling.png", plot = lp.1504.2203,width = 7.5, height = 3.5, units = "in", dpi = 300, bg="white")

