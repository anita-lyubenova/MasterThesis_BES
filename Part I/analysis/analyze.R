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

# RQ1 Median plots ---------------------------------------------------------------------------------



datln %>% 
    aggregatePMP(hyp=c("H1","Hc"),
                 studies=30) %>% 
    create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.86_linear",
                            n="300") %>% 
    median_plot()
  
datln %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.86_linear",
                          n="300") %>% 
  median_plot()
datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.86_linear",
                          n="300") %>% 
  median_plot()





datln %>% 
  aggregatePMP(hyp=c("H1","Hc"),
               studies=15) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.5_linear",
                          n="300") %>% 
  median_plot()

datln %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=30) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.5_linear",
                          n="300") %>% 
  median_plot()

datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30) %>% 
  create_median_plot_data(pop="H1_r.13_pcor.3_b321_p.5_linear",
                          n="300") %>% 
  median_plot()





# RQ2 Tile plots -------------------------------------------------------------------------------

## Conjoint testing --------------------------------------------------
### Accuracy 

cp.1604.1422<-datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot(object = "acc")

ggsave("Part I/analysis/output/cp.1604.1422.png", plot = cp.1604.1422, width = 7, height = 2.6, units = "in", dpi = 300, bg="white")


### TPR  
TP.1604.1627<-
  datln %>% 
    aggregatePMP(hyp=c("H1","Hc","Hu"),
                 studies=30) %>% 
    accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                               Hc="Hc_r.13_pcor.3_b123_p0_linear",
                               Hu="H1_r.13_pcor.3_b321_p.86_linear"
    )) %>% 
    TP_corrplot()
ggsave("Part I/analysis/output/TP.1604.1627.png", plot = TP.1604.1627, width = 7, height = 6.6, units = "in", dpi = 300, bg="white")


