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
