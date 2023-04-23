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
                           Hypothesis=="PMP_0" ~ "#aa69b5",#6B2B74
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
# PMP=PMP_H1TRUE_eqES
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
 # PMP=PMP_H1TRUE
 # hyp_input<-c("i", "c", "u", "0")
 # N_input<-2
 # data<-PMP[[paste0(hyp_input, collapse = "")]][,,1,,N_input]


median_plot<-function(data, hyp_input){
  
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
    hc_tooltip(enabled=TRUE,
               valueDecimals=2)%>%
    hc_yAxis(labels=list(enabled=TRUE),
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
    hc_title(text="Variation of aggregate PMPs for each hypothesis across iterations",
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

