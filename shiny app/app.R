library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(shades)
library(tidyverse)
library(highcharter)

setwd("C:/Anita's Documents/MSc Utrecht/Master Thesis/01_MasterThesis_BES")
load("RRrepo/workspaces/PMPs/PMP_H1TRUE.RData")
# load("RRrepo/workspaces/PMPs/PMP_HcTRUE.RData")
# load("RRrepo/workspaces/PMPs/PMP_HuTRUE_eqES.RData")
# load("RRrepo/workspaces/PMPs/PMP_HuTRUE_largerESi.RData")

source("shiny app/modules.R")
source("shiny app/app functions.R")


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


