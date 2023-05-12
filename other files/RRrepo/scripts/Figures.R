#Load packages ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(shades)
library(plotly)
library(patchwork)

load("workspaces/Aggregate PMPs_H1 vs. Hc.RData")
load("workspaces/Aggregate PMPs_H1 vs. Hc vs. Hu.RData")

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

#Hi:Hc = 1:1 (Fig. 1) ------------------------------------------------------------------

#Conditions:
#   -testing Hi vs Hc  AND  Hi vs Hc vs Hu 
#   -for every second study Hi is true while for the rest Hc is true (that is, Hu is the only common true hypothesis)
#   -the effect size for Hi and Hc is the same (ratio_beta for Hi 3:2:1 vs Hc: 1:2:3)
#   -N=350 (91% power)


#Question:
#   -how often is the aggregate PMP_u the highest?

## Median PMP plot: with Hu ----------------------
#aggrPMP1.withHu[studies, hypothesis, iter, n]
#dimnames(aggrPMP1.withHu)

dimnames(aggrPMP1.withHu[,,,s])
median.PMP1.df<-create_plot_data(aggrPMP1.withHu[,,,s])

dimnames(aggrPMP1.withHu[,,,s])
median.PMP1.plot<-median.PMP1.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP), position = pd)+
  theme_minimal()+
  labs(#title = "",
    subtitle = "H1 vs. Hc vs. Hu",
    x="Number of aggregated studies",
    y="Aggregate PMP")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 8, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)),
        legend.title = element_text(size = 7),
        legend.title.align=0.5,
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
        # legend.text = element_text(size = 10),
  )+
  # guides(colour = guide_legend(nrow = 1))+
  # annotate("text", x = 43, y = 0.5, label = "H1:Hc = 1:1",angle = 270)+
  coord_cartesian(xlim = c(0, 40), clip = "off")+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "Hc", "Hu"),name = "Hypothesis")


median.PMP1.plot

# ggplotly(median.PMP1.plot)
# 
# a<-median.PMP1.plot + theme(legend.position="none")
# ggsave("Outputs/Research Report/Presentation/Fig2.png", plot = a, width = 5, height = 2, units = "in", dpi = 300, bg="white")
# 

## Median PMP plot: no Hu -------------------------

median.PMP1.noHu.df<-create_plot_data(aggrPMP1.noHu[,,,s])

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
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
        # legend.text = element_text(size = 10),
  )+
  guides(color="none")+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "Hc", "Hu"),name = "Hypothesis")

median.PMP1.noHu.plot

# ggplotly(median.PMP1.plot)
# ggsave("Outputs/Research Report/Presentation/Fig1.png", plot = median.PMP1.noHu.plot, width = 5, height = 2, units = "in", dpi = 300, bg="white")

#Hi:Hc = 3:1 (Fig. 2) ------------------------------------------------------------------

#Conditions:
#   -testing  Hi vs Hc vs Hu 
#  ! -for every 4th study Hc is true while for the rest Hi is true(Hi:Hc = 3:1) (that is, Hu is still 
#      the only common true hypothesis but Hi is more prevalent)
#   -the effect size for Hi and Hc is the same (ratio_beta for Hi 3:2:1 vs Hc: 1:2:3 )

#Question:
#   -how often is the aggregate PMP_u the highest?

## Median PMP plot: with Hu ----------------------
#aggrPMP2.withHu[studies, hypothesis, iter, n]

median.PMP2.df<-create_plot_data(aggrPMP2.withHu[,,,s])

median.PMP2.plot<-median.PMP2.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c","PMP_u"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
  theme_minimal()+
  labs(#title = "",
    subtitle = "H1 vs. Hc vs. Hu",
    x="Number of aggregated studies",
    y="Aggregate PMP")+
  scale_color_discrete(labels=c("H1c", "H1", "Hu"))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 8, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm"),
        plot.subtitle = element_text(hjust = 0.5),
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  #annotate("text", x = 43, y = 0.5, label = "H1:Hc = 3:1",angle = 270)+
  coord_cartesian(xlim = c(0, 40), clip = "off")+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "Hc", "Hu"),name = "Hypothesis")

median.PMP2.plot

ggplotly(median.PMP1.plot)


## Median PMP plot: no Hu -------------------------

median.PMP2.noHu.df<-create_plot_data(aggrPMP2.noHu[,,,s])

median.PMP2.noHu.plot<-median.PMP2.noHu.df%>% 
  ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = c("PMP_i","PMP_c")), color=factor(Hypothesis, levels = c("PMP_i","PMP_c"))))+
  geom_point(position = pd)+
  geom_line(position = pd)+
  geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
  theme_minimal()+
  labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
    subtitle = "H1 vs. Hc",
    #  x="Number of aggregated studies",
    x=" ",
    y="Aggregate PMP")+
  scale_color_discrete(labels=c("H1c", "H1"))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 8, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm"),
        plot.subtitle = element_text(hjust = 0.5),
        # legend.text = element_text(size = 10),
  )+
  guides(color="none")+
  scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "H1c", "Hu"),name = "Hypothesis")

median.PMP2.noHu.plot

#ggplotly(median.PMP1.plot)

#Combine plots-----------------------------------------

F1<-median.PMP1.noHu.plot/median.PMP1.plot+
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')
F1

F2<-median.PMP2.noHu.plot/median.PMP2.plot+
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')&
  theme(legend.position='bottom')
F2


# Figure 3: Support for Hu ----------------------------------------------------------------
# Proportion of times Hu received most support

##create plot data ------------------------------------
### H1:Hc=1:1 ------------------------------------------
support.Hu1<-array(NA,
                  dim=c(iter,
                        studies,
                        length(n)
                  ),
                  dimnames = list(
                    1:iter,
                    1:studies,
                    paste0("n = ",n)
                  )
)

for(i in 1:iter){
  for(t in 1:studies){
    for(s in 1:length(n))
      #if PMP_u were the highest for iter i and study t => record 1, otherwise record 0
      support.Hu1[i,t,s]<-ifelse(max(aggrPMP1.withHu[t,,i,s])==aggrPMP1.withHu[t,,i,s][3], 1,0)
    
  }
}


### H1:Hc=3:1 ------------------------------------------
support.Hu2<-array(NA,
                   dim=c(iter,
                         studies,
                         length(n)
                   ),
                   dimnames = list(
                     1:iter,
                     1:studies,
                     paste0("n = ",n)
                   )
)


for(i in 1:iter){
  for(t in 1:studies){
    for(s in 1:length(n))
      #if PMP_u were the highest for iter i and study t => record 1, otherwise record 0
      support.Hu2[i,t,s]<-ifelse(max(aggrPMP2.withHu[t,,i,s])==aggrPMP2.withHu[t,,i,s][3], 1,0)
    
  }
}

#combine
support.df<-cbind(colSums(support.Hu1[,,s])/iter, 
                  colSums(support.Hu2[,,s])/iter)
colnames(support.df)<-c("1:1", "3:1")

## plot----------------------------
F3<-support.df %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = colnames(support.df),
               names_to = "ratio",
               values_to = "prop.highest.PMP_u"
  ) %>% 
  mutate(ratio=as.factor(ratio),
         t=factor(t, levels = unique(sort(as.numeric(t))))
  ) %>% 
  ggplot(aes(x=t, y=prop.highest.PMP_u, group=ratio, color=ratio))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  labs(x="Size of the study set ",
       y="Proportion",
       #  title="Proportion of times the PMP of Hu was the highest"
  )+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        legend.title = element_text(size = 7)
        # plot.subtitle = element_text(size = 12),
        # legend.text = element_text(size = 10),
  )+
  scale_colour_discrete(name = "Ratio H1:Hc")

save.image("workspaces/Figures.RData")
