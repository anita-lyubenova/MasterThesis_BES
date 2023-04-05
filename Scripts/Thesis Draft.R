#Load packages ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(shades)
library(plotly)
library(patchwork)

#INTRO: FIGURE 1 ---------------------------------------------------
load("RRrepo/workspaces/Aggregate PMPs_H1 vs. Hc.RData")
load("RRrepo/workspaces/Aggregate PMPs_H1 vs. Hc vs. Hu.RData")

#function to create plot data
create_plot_data<-function(aggrPMP){ 
  medians<-apply(aggrPMP,c(1,2),median)%>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(aggrPMP)[[2]],
                 names_to = "Hypothesis",
                 values_to = "median_aggrPMP") %>%
    mutate(Hypothesis=as.factor(Hypothesis),
           t=factor(t, levels = unique(t))
    )
  
  #lower bounds of the aggregate values per hypothesis h per study number t
  lbs<-apply(aggrPMP,c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(aggrPMP)[[2]],
                 names_to = "Hypothesis",
                 values_to = "lb_aggrPMP") %>%
    mutate(Hypothesis=as.factor(Hypothesis),
           t=factor(t, levels = unique(t))
    )  
  
  ubs<-apply(aggrPMP,c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(aggrPMP)[[2]],
                 names_to = "Hypothesis",
                 values_to = "ub_aggrPMP") %>%
    mutate(Hypothesis=as.factor(Hypothesis),
           t=factor(t, levels = unique(t))
    )  
  
  df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
    left_join(., ubs, by=c("t", "Hypothesis"))
  
  return(df)
}
#common parameters for all plots
s<-2 #N=350
pd <- position_dodge(width = 0.4) # set desired dodge width
col.H1<-"#7fc97f"
  col.Hc<-"#fdc086"
    col.Hu<-"black"
      lightness(col.H1, scalefac(0.50))
      
## Median PMP plot: no Hu -------------------------
median.PMP1.noHu.df<-create_plot_data(aggrPMP1.noHu[1:20,,,s])

median.PMP1.noHu.plot<-median.PMP1.noHu.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
    subtitle = "H1 vs. Hc",
    #x="Number of aggregated studies",
    x=" ",
    y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 8, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)),
        legend.title = element_text(size = 7),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm"),
        legend.position="bottom"
        # legend.text = element_text(size = 10),
  )+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "Hc", "Hu"),name = "Hypothesis")

median.PMP1.noHu.plot

# ggplotly(median.PMP1.plot)
ggsave("Outputs/Thesis Draft/Fig1.png", plot = median.PMP1.noHu.plot, width = 6, height = 2.5, units = "in", dpi = 300, bg="white")


#_______________________________________________________________________________
#  RESULTS I  --------------------------------------------------------------------- 
#_______________________________________________________________________________
#How often do the highest PMPs belong to the most parsimonious common true hypothesis?
# -how many studies are necessary?
# - to what extend do individual study sample sizes matter?


#which data file to use..?

load("Outputs/accuracy/dat_merged.RData") # this one has many populations, including heterogeneous ones
dat$BF %>% dimnames()
#Populations: 
# [1] "TRUE_H0"         "TRUE_H1"         "TRUE_Hc"         "TRUE_Hu"         "HETEROG_H1p.1"   "HETEROG_H1p.3"  
# [7] "HETEROG_H1p.5"   "HETEROG_H1p.75"  "HETEROG_H1p1"    "HETEROG_H1p1.25" "HETEROG_H1p1.5"  "TRUE_H1large"   
# [13] "TRUE_H1small" 
source("Scripts/accuracy/accuracy_functions.R")

## Median plots ---------------------------------------------------------------------------

f1<-aggregatePMP(dat,
             hyp=c("H1","Hc" ),
             studies=15
) %>% 
  create_median_plot_data(pop = "TRUE_Hu",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "none")



f2<-aggregatePMP(dat,
             hyp=c("H1","Hc" ,"Hu"),
             studies=15
) %>% 
  create_median_plot_data(pop = "TRUE_Hu",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")

f8<-aggregatePMP(dat,
             hyp=c("H1","Hc" ,"Hu"),
             studies=15
            ) %>% 
  create_median_plot_data(pop = "HETEROG_H1p.75",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")+
  labs(title="Heterogeneous H1: tau=0.75betas")


f128<-f1/
        f2+theme(legend.position = "none")+
        f8
ggsave("Outputs/Thesis Draft/f12.png", plot = f12, width = 6, height = 6, units = "in", dpi = 300, bg="white")

ggsave("Outputs/Thesis Draft/f128.png", plot = f128,  width = 9, height = 9, units = "in", dpi = 300, bg="white")



f2u<-aggregatePMP(dat,
                 hyp=c("H1" ,"Hu"),
                 studies=15
) %>% 
  create_median_plot_data(pop = "TRUE_Hu",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")

f8u<-aggregatePMP(dat,
                 hyp=c("H1","Hu"),
                 studies=15
) %>% 
  create_median_plot_data(pop = "HETEROG_H1p.75",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")+
  labs(title="Heterogeneous H1: tau=0.75betas")



f128u<-f1/(f2+f2u)+(f8+f8u)

f128u<-f1/(f8u+theme(legend.position = "none")+labs(title="Heterogeneous H1: tau=0.75betas")+
           f8+theme(legend.position = "none")+labs(title="Heterogeneous H1: tau=0.75betas"))+
          (f2u+f2)
            

f128u<-f1/(f2u+theme(legend.position = "none")+
             f8u+theme(legend.position = "none"))+
  (f2+f8)

ggsave("Outputs/Thesis Draft/f128u.png", plot = f128u,  width = 9, height = 9, units = "in", dpi = 300, bg="white")

#add a less heterogeneous population which Hu cannot detect

f8up.5<-aggregatePMP(dat,
                     hyp=c("H1","Hu"),
                     studies=15
) %>% 
  create_median_plot_data(pop = "HETEROG_H1p.5",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")+
  labs(title="Heterogeneous H1: tau=0.50betas")

f8p.5<-aggregatePMP(dat,
                     hyp=c("H1","Hc","Hu"),
                     studies=15
) %>% 
  create_median_plot_data(pop = "HETEROG_H1p.5",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")+
  labs(title="Heterogeneous H1: tau=0.50betas")

f1p.75<-aggregatePMP(dat,
                     hyp=c("H1","Hc"),
                     studies=15
) %>% 
  create_median_plot_data(pop = "HETEROG_H1p.75",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")+
  labs(title="Heterogeneous H1: tau=0.75betas")+
  theme(legend.position = "none")

f1p.5<-aggregatePMP(dat,
                     hyp=c("H1","Hc"),
                     studies=15
) %>% 
  create_median_plot_data(pop = "HETEROG_H1p.5",n=c("n300")) %>% 
  median_plot()+
  theme(legend.position = "bottom")+
  labs(title="Heterogeneous H1: tau=0.50betas")+
  theme(legend.position = "none")

f128u<-f1/(f2u+theme(legend.position = "none")+
             f2+theme(legend.position = "none"))+
  f1p.75+
            (f8u+theme(legend.position = "none")+
             f8+theme(legend.position = "none"))+
  f1p.5+
            (f8up.5+f8p.5)


ggsave("Outputs/Thesis Draft/f128u.png", plot = f128u,  width = 9, height = 13, units = "in", dpi = 300, bg="white")



## Accuracy plots ---------------------------------------------------------------
n<-c(50,100,150,200,300)
### TRUE_Hu --------------------------
f02<-
  aggregatePMP(dat,
               hyp=c("H1","Hc","Hu"),
               studies=20
  )%>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="TRUE_Hu")) %>% 
  acc_corrplot(object = "acc")+
  scale_y_discrete(labels=paste0("n=", n))+
  theme(legend.position = "none")+
  labs(title = "Accuracy: H1 vs. Hc vs. Hu")

f03<-
  aggregatePMP(dat,
             hyp=c("H1","Hc","Hu"),
             studies=20
) %>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="TRUE_Hu")) %>% 
  acc_corrplot(object = "TP")+
  theme(legend.position = "bottom",
        legend.key.width=unit(3,"cm"))+
  labs(title = "True classifications: H1 vs. Hc vs. Hu")+
  scale_y_discrete(labels=c(paste0("True H1: n=", n),paste0("True Hc: n=", n),paste0("True Hu: n=", n)))


f23<-f02/f03
ggsave("Outputs/Thesis Draft/f23.png", plot = f23, width = 10, height = 13, units = "in", dpi = 300, bg="white")


### HETEROG_TRUE_H1 --------------------
f04<-
  aggregatePMP(dat,
               hyp=c("H1","Hc","Hu"),
               studies=20
  )%>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="HETEROG_H1p1")) %>% 
  acc_corrplot(object = "acc")+
  scale_y_discrete(labels=paste0("n=", n))+
  theme(legend.position = "none")+
  labs(title = "Accuracy: H1 vs. Hc vs. Hu")

f05<-
  aggregatePMP(dat,
               hyp=c("H1","Hc","Hu"),
               studies=20
  )%>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="HETEROG_H1p.75")) %>% 
  acc_corrplot(object = "acc")+
  scale_y_discrete(labels=paste0("n=", n))+
  theme(legend.position = "bottom",
        legend.key.width=unit(3.5,"cm"))+
  labs(title = "Accuracy: H1 vs. Hc vs. Hu")

f245<-f02/
        f04+labs(title = NULL)+
        f05+labs(title = NULL)

ggsave("Outputs/Thesis Draft/f245.png", plot = f245, width = 9, height = 12, units = "in", dpi = 300, bg="white")


# Acc + TP
f06<-
  aggregatePMP(dat,
               hyp=c("H1","Hc","Hu"),
               studies=20
  )%>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="HETEROG_H1p.75")) %>% 
  acc_corrplot(object = "acc")+
  scale_y_discrete(labels=paste0("n=", n))+
  theme(legend.position = "none")+
  labs(title = "Accuracy: H1 vs. Hc vs. Hu")

f07<-
  aggregatePMP(dat,
               hyp=c("H1","Hc","Hu"),
               studies=20
  ) %>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="HETEROG_H1p.75")) %>% 
  acc_corrplot(object = "TP")+
  theme(legend.position = "bottom",
        legend.key.width=unit(3,"cm"))+
  labs(title = "True classifications: H1 vs. Hc vs. Hu")+
  scale_y_discrete(labels=c(paste0("True H1: n=", n),paste0("True Hc: n=", n),paste0("True Hu: n=", n)))


f67<-f06/f07
ggsave("Outputs/Thesis Draft/f67.png", plot = f67, width = 13, height = 15, units = "in", dpi = 300, bg="white")


f09<-
  aggregatePMP(dat,
               hyp=c("H1","Hu"),
               studies=30
  )%>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.75")) %>% 
  acc_corrplot(object = "acc")+
  scale_y_discrete(labels=paste0("n=", n))+
  theme(legend.position = "none")+
  labs(title = "Accuracy: H1 vs. Hu")

f010<-
  aggregatePMP(dat,
               hyp=c("H1","Hu"),
               studies=30
  ) %>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.75")) %>% 
  acc_corrplot(object = "TP")+
  theme(legend.position = "bottom",
        legend.key.width=unit(3,"cm"))+
  labs(title = "True classifications: H1  vs. Hu")+
  scale_y_discrete(labels=c(paste0("True H1: n=", n),paste0("True Hu: n=", n)))



f910<-f09/f010
ggsave("Outputs/Thesis Draft/f910.png", plot = f910, width = 13, height = 10, units = "in", dpi = 300, bg="white")

