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

###########################  TPR   #########################################
#
dimnames(dat)[[3]]
dimnames(dat[,,,,-c(1,3,11)])


TPR<-
  dat%>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30,
               subset = "dat[,,,,-c(1,3,11)]") %>% 
  accuracyPMP(hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                             Hc="r0.13_pcor0.3_b123_p0",
                             Hu="r0.13_pcor0.3_b321_p0.86"
  )) %>% 
  TP_corrplot()
TPRs<-wrap_plots(TPR, ncol=1)+ 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

ggsave("Part I/analysis/output/TPRs2.png", plot = TPRs, width = 8, height = 8, units = "in", dpi = 300, bg="white")

###########################  ACCURACY   #########################################
#
a<-dat %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30,
               subset = "dat[,,,,-c(1,3,11)]") %>% 
  accuracyPMP(hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                             Hc="r0.13_pcor0.3_b123_p0",
                             Hu="r0.13_pcor0.3_b321_p0.86"
  ))

accplot<-dat %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30,
               subset = "dat[,,,,-c(1,3,11)]") %>% 
  accuracyPMP(hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                             Hc="r0.13_pcor0.3_b123_p0",
                             Hu="r0.13_pcor0.3_b321_p0.86"
  )) %>% 
  acc_corrplot(object = "acc")
accplot
ggsave("Part I/analysis/output/accplot.png", plot = accplot, width = 7, height = 3, units = "in", dpi = 300, bg="white")


###########################   COSTS   #############################################
acc.iu<-dat %>% 
  aggregatePMP(hyp=c("H1","Hu"),
               studies=30,
               subset = "dat[,,,,-c(1,3,11)]") %>% 
  accuracyPMP(hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                             Hu="r0.13_pcor0.3_b123_p0",
                             Hu="r0.13_pcor0.3_b321_p0.86"
  ))
acc.icu<-dat %>% 
  aggregatePMP(hyp=c("H1","Hc","Hu"),
               studies=30,
               subset = "dat[,,,,-c(1,3,11)]") %>% 
  accuracyPMP(hyp_to_pop = c(H1="r0.13_pcor0.3_b321_p0",
                             Hc="r0.13_pcor0.3_b123_p0",
                             Hu="r0.13_pcor0.3_b321_p0.86"
  ))


#create text labels only for t=1 and t=30
q <-acc.iu$acc-acc.icu$acc
q[-c(1,nrow(q)),]<-NA
label.df<-
  reshape2::melt(q)%>%
  mutate(color=case_when(value<0.015 ~ "black",
                         value>0.015~ "white"
  )) %>%
  pull(value,color) %>%
  round(.,digits=2) %>%
  format(nsmall=2) %>%
  gsub("^0", "", .) %>%
  gsub("NA", NA, .)


#create data
acc.icu.l<-acc.icu$acc %>% 
  reshape2::melt() %>% 
  rename(n.icu=Var2,
         t.icu=Var1,
         acc.icu=value) %>%
  mutate(n.icu=factor(n.icu),
         t.icu=factor(t.icu)
  )

acc.iu.l<-acc.iu$acc %>% 
  reshape2::melt() %>% 
  rename(n.iu=Var2,
         t.iu=Var1,
         acc.iu=value) %>%
  mutate(n.iu=factor(n.iu),
         t.iu=factor(t.iu)
  )


diff<-cbind(acc.icu.l,acc.iu.l) %>% 
  mutate(diff=acc.iu-acc.icu,
         color= case_when(acc.icu<=0.865 & acc.iu<=0.865 ~"both <.87",
                          acc.icu<=0.865 & acc.iu>0.865 ~"only \"H1 vs Hu\">.87",
                          acc.icu>0.865 & acc.iu>0.865 ~"both >.87"
                          )
         )

cols<-c("#CCCCCC", "#EBEBEB",  "#fcfcfc")#"#f9f7f7"

costplot<-
  ggplot(data = diff, mapping = aes(x=t.icu,
                                            y=n.icu,
                                            fill=factor(color, levels = c("both <.87", "only \"H1 vs Hu\">.87", "both >.87" ))))+
  geom_tile()+#color = "white"
  scale_fill_manual(values=cols,
                    name="Test accuracies")+
  geom_point(mapping = aes(x=t.icu, y=n.icu, size=diff), color="#5C5C5C")+
  scale_size(range = c(-1, 5.5),
             name= "Difference in accuracies")+ # "acc(H1 vs Hu) - acc(conjoint)"
  # geom_text(mapping = aes(x=t.icu, y=n.icu),
  #           label = label.df,
  #           color= names(label.df),  #"white",
  #           size = 2.8,
  #           fontface='bold')+
  labs(x="t", y="n")+
  theme_minimal()+
  theme(legend.position = "top",legend.key=element_blank())+
  guides(fill = guide_legend(label.position = "bottom",
                             title.position = "top",
                             order = 1,
                             override.aes = list(shape = c(NA, NA, NA))),
         size = guide_legend(label.position = "bottom",
                             title.position = "top",
                             order=2)
         )
costplot  
ggsave("Part I/analysis/output/costplot.png", plot = costplot, width = 7, height = 3, units = "in", dpi = 300, bg="white")

