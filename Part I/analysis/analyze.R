library(patchwork)
source("Part I/analysis/analysis functions.R")
dat<-readRDS("Part I/pre-processing/output/BF_data_3par_hpc.rds")

#populations
dimnames(dat)[[3]]
attributes(dat)

###########################  MEDIAN PLOTS #########################################

# H1+Hc --------------------------------------------------------------
mp.H1Hc.ic<- dat %>% 
  aggregatePMP(hyp=c("H1","Hc"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321123_p0",
                          n="300") %>% 
  median_plot()+
  theme(legend.position = "none")+
  labs(title = NULL)

mp.H1Hc.iu<- dat %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321123_p0",
                          n="300") %>% 
  median_plot()+
  theme(legend.position = "none")+
  labs(title = NULL)

mp.H1Hc.icu<- dat %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321123_p0",
                          n="300") %>% 
  median_plot()+
  theme(legend.position = "none")+
  labs(title = NULL)

mp.H1Hc<-mp.H1Hc.ic/(mp.H1Hc.iu+mp.H1Hc.icu)+ 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

#fix legends
mp.H1Hc[[1]]<-mp.H1Hc[[1]]+ theme(legend.position = "none")
mp.H1Hc[[2]][[1]] <- mp.H1Hc[[2]][[1]]+ theme(legend.position = "none")
mp.H1Hc[[2]][[2]] <- mp.H1Hc[[2]][[2]] +labs(y=NULL)
mp.H1Hc

ggsave("Part I/analysis/output/mp.H1Hc.png", plot = mp.H1Hc, width = 7, height = 4, units = "in", dpi = 300, bg="white")


# p.86   --------------------------------------------------------------
mp.86.ic<- dat %>% 
  aggregatePMP(hyp=c("H1","Hc"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321_p0.86",
                          n="300") %>% 
  median_plot()+
  theme(legend.position = "none")+
  labs(title = NULL)

mp.86.iu<-dat %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321_p0.86",
                          n="300") %>% 
  median_plot()+
  labs(title = NULL)+
  theme(legend.position = "none")

mp.86.icu<-dat%>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321_p0.86",
                          n="300") %>% 
  median_plot()+
  labs(title = NULL)+
  theme(legend.position = "none")



mp.86<-mp.86.ic/(mp.86.iu+mp.86.icu)+ 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

#fix legends
mp.86[[1]]<-mp.86[[1]]+ theme(legend.position = "none")
mp.86[[2]][[1]] <- mp.86[[2]][[1]]+ theme(legend.position = "none")
mp.86[[2]][[2]] <- mp.86[[2]][[2]] +labs(y=NULL)
mp.86

ggsave("Part I/analysis/output/mp.86.png", plot = mp.86, width = 7, height = 4, units = "in", dpi = 300, bg="white")


#   p.5   ----------------------------------------
mp.50.ic<- dat %>% 
  aggregatePMP(hyp=c("H1","Hc"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321_p0.5",
                          n="300") %>% 
  median_plot()+
  theme(legend.position = "none")+
  labs(title = NULL)

mp.50.iu<-dat %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321_p0.5",
                          n="300") %>% 
  median_plot()+
  labs(title = NULL)
mp.50.iu<-mp.50.iu+ theme(legend.position = "none")

mp.50.icu<-dat %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=15) %>% 
  create_median_plot_data(pop="r0.13_pcor0.3_b321_p0.5",
                          n="300") %>% 
  median_plot()+
  labs(title = NULL)

mp.50<-mp.50.ic/(mp.50.iu+mp.50.icu)+ 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

#fix legends
mp.50[[1]]<-mp.50[[1]]+ theme(legend.position = "none")
mp.50[[2]][[1]] <- mp.50[[2]][[1]]+ theme(legend.position = "none")
mp.50[[2]][[2]] <- mp.50[[2]][[2]] +labs(y=NULL)


ggsave("Part I/analysis/output/mp.50.png", plot = mp.50, width = 7, height = 4, units = "in", dpi = 300, bg="white")
