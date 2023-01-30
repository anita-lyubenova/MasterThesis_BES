library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(shades)
library(tidyverse)

setwd("C:/Anita's Documents/MSc Utrecht/Master Thesis/01_MasterThesis_BES")
load("RRrepo/workspaces/PMPs/PMP_H1TRUE.RData")
# load("RRrepo/workspaces/PMPs/PMP_HcTRUE.RData")
# load("RRrepo/workspaces/PMPs/PMP_HuTRUE_eqES.RData")
# load("RRrepo/workspaces/PMPs/PMP_HuTRUE_largerESi.RData")

source("shiny app/modules.R")


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
      
      
custom_minty<- bs_theme(
    version = 5,
    bootswatch = "minty"
  )

#bs_theme_preview(theme = custom_minty, with_themer = FALSE)

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme = custom_minty,
               selected = "PMP distributions",
               tags$head(
                 # Note the wrapping of the string in HTML()
                 tags$style(HTML("
                    .navbar.navbar-default {
                        background-color: #78C1A9 !important;
                        color: white !important;
                    }

                    .navbar-default .navbar-brand {
                         color: white !important;
                    }
                    .navbar-default .navbar-nav > li > a {color:white !important;}
                    .navbar-default .navbar-nav > li > a:hover {font-weight: bold !important;}
                    .navbar-default .navbar-nav > li > a:focus {font-weight: bold !important;}
                                "))),
               tabPanel("Home"),
               tabPanel("PMP distributions",
                        gen_plot_UI("plot_conditions")
                        ),
               tabPanel("BES-power plots")
               
               )

server<-function(input, output){
  
  gen_plot_server("plot_conditions", PMP=PMP_H1TRUE)
  
}

shinyApp(ui = ui, server = server)
#run_with_themer(shinyApp(ui = ui, server = server))


#seecolor::print_color(c("#8ed1be", "#6ECCAE", "#73BAA4"), type = "r")



