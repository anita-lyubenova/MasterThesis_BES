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
               studies=10) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  )) %>% 
  acc_corrplot2(object = "acc")

#####################  Dev acc_corrplot2() ################################################
a<-datln %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="H1_r.13_pcor.3_b321_p0_linear",
                             Hc="Hc_r.13_pcor.3_b123_p0_linear",
                             Hu="H1_r.13_pcor.3_b321_p.86_linear"
  ))


attributes(datln)
attributes(a)
#a custom corrlot 
acc_corrplot2<-function(a, # a list created with accuracyPMP()
                        object="acc", # what should be plotted? acc or TP?
                        b=NULL #optional: another list created with accuracyPMP to plot differences in acc
){
  #create the data to plot
  if(is.null(b)){
    x<-
      a[[object]] %>% 
      reshape2::melt() %>% 
      rename(n=Var2,
             t=Var1) %>%
      mutate(cat=cut(value, breaks=c(0,0.4,0.5,0.7,0.8,0.9,1)),
             n=factor(n),
             t=factor(t)
             )
     
    
    # q<-a[[object]]
    # q[-which(q>.86 & q<.88)]<-NA
    # label<-reshape2::melt(q) %>% 
    #   pull(value) %>% 
    #   round(digits=2)
    
  }else{
    #compute differences in accuracies & reshape
  }
  # label<-round(x$value, digits = 2)
  all.colors <-c(`(0,0.4]`="#AD3511",
                 `(0.4,0.5]`="#f46d43",
                 `(0.5,0.7]`="#fdae61",
                 `(0.7,0.8]`="#fee08b",
                 `(0.8,0.9]`="#e6f598",
                 `(0.9,1]  `="#abdda4")
 counts<-table(x$cat)
  sub.colors<-all.colors[names(all.colors) %in% names(counts[counts!=0])]
  
  x$cat
  ggplot(data = x, mapping = aes(x=t, y=n, fill=cat))+
    geom_tile(color = "black")+
    scale_fill_manual(values=unname(sub.colors),labels=names(sub.colors),name = "Accuracy")+
    theme_minimal()
    # scale_fill_gradientn(colours = c("red", "white", "#7fc97f"),
    #                  #    limit = c(0, 1),
    #                      space = "Lab",
    #                      name = "Accuracy",
    #                      values = scales::rescale(c(0,0.87,1))
    # )+
    # geom_text(mapping = aes(x=t, y=n),
    #           label = label,
    #           size = 4)+
   
  
  
}

