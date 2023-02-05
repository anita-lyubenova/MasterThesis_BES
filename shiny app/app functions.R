#function to create median plot data
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
    left_join(., ubs, by=c("t", "Hypothesis")) %>% 
    as.data.frame() %>% 
    mutate(color=case_when(Hypothesis=="PMP_i" ~ "#7fc97f",
                           Hypothesis=="PMP_c" ~ "#fdc086",
                           Hypothesis=="PMP_u" ~ "black",
                           Hypothesis=="PMP_0" ~ "#6B2B74",
                           ),
           name=case_when(Hypothesis=="PMP_i" ~ "Hi",
                          Hypothesis=="PMP_c" ~ "Hc",
                          Hypothesis=="PMP_u" ~ "Hu",
                          Hypothesis=="PMP_0" ~ "H0",
           )
           )
  
  
  return(df)
}
# ###### temp ######
# load("RRrepo/workspaces/PMPs/PMP_H1TRUE.RData")
# PMP=PMP_H1TRUE
# 
# hyp_input<-c("i", "c", "u")
# N_input<-2
# 
# names(PMP)[1:4]
# 
# PMP[[paste0(hyp_input, collapse = "")]][,,,,N_input]
# 
# 
# #PMP[["ic"]][,,,,5]
# 
# data<-PMP[[paste0(hyp_input, collapse = "")]][,,,,N_input]
# 
# ###### temp #########

# median_plot<-function(data){
#   #common parameters for all plots
#   pd <- position_dodge(width = 0.4) # set desired dodge width
#   col.H1<-"#7fc97f"
#   col.Hc<-"#fdc086"
#   col.Hu<-"black"
#   col.H0<-"#6B2B74"
#  # lightness(col.H1, scalefac(0.50))
# 
#   create_plot_data(data)%>%
#     ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = dimnames(data)[[2]]), color=factor(Hypothesis, levels = dimnames(data)[[2]])))+
#     geom_point(position = pd)+
#     geom_line(position = pd)+
#     geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
#     theme_minimal()+
#     labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
#       subtitle = paste(paste0("H",hyp_input), collapse = " vs. "), #paste0("H",hyp_input[1], " vs H", hyp_input[2]),     #"H1 vs. Hc",
#       #x="Number of aggregated studies",
#       x=" ",
#       y="Aggregate PMP")+
#     theme(text = element_text(size = 9),
#          # axis.text.x = element_text(size = 8, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)),
#           legend.title = element_text(size = 7),
#           plot.subtitle = element_text(hjust = 0.5),
#           plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
#           # legend.text = element_text(size = 10),
#     )+
#     guides(color="none")+
#     scale_colour_manual(values = c(col.H1,col.Hc,col.Hu, col.H0 ),labels=c("H1", "Hc", "Hu", "H0"),name = "Hypothesis")
# 
# }

# hyp_input<-c("i", "c", "u", "0")
# data<-PMP[[paste0(hyp_input, collapse = "")]][,,,,N_input]
 #unique(plot_data$name)
median_plot<-function(data, hyp_input){
  
  # colors<-c("#7fc97f", "#fdc086", "black", "#6B2B74")
  # names(colors)<-c("i", "c", "u", "0")
  
  plot_data<-create_plot_data(data)
  
  plot_data%>%
    hchart("scatter",
           hcaes(x=t, y=median_aggrPMP,
                 group=factor(Hypothesis,levels = dimnames(data)[[2]])
                 ),
           color=plot_data$color[1:length(hyp_input)],
           name=plot_data$name[1:length(hyp_input)],
           id=letters[1:length(dimnames(data)[[2]])]
           )%>%
    hc_tooltip(enabled=FALSE)%>%
    hc_yAxis(labels=list(enabled=FALSE),
             #reversed=TRUE,
             title=list(text="Aggregated PMPs"),
             gridLineWidth=0)%>%
    hc_xAxis(labels=list(style=list(color="black", fontSize="12px")),
             title=list(text="Number of aggregated studies")
           # opposite=TRUE
             )%>%
    hc_legend(enabled=TRUE,
              verticalAlign = "bottom",
              align="left",
              title=list(text="Hypotheses"
                         
                         )) %>% 
    hc_title(text="Aggregate PMPs for each hypothesis with increasing number of studies",
             align="left") %>% 
    hc_subtitle(text=paste(paste0("H",hyp_input), collapse = " vs. "),
                align="left") %>% 
    hc_add_series(
      plot_data,
      "errorbar", 
      hcaes(y = median_aggrPMP, x = t, low = lb_aggrPMP, high = ub_aggrPMP,
            group = factor(Hypothesis, levels = dimnames(data)[[2]])
            ),
      color=plot_data$color[1:length(hyp_input)],
      linkedTo = letters[1:length(dimnames(data)[[2]])],
      enableMouseTracking = TRUE,
      showInLegend = FALSE,
      grouping=TRUE,
      groupPadding=0.3
    ) 
   

}

