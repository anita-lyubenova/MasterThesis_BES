library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(shades)
library(tidyverse)
library(highcharter)
library(htmltools)
library(htmlwidgets)


dat3<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_3par.rds")
dat2<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_2par.rds")
dat1<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_1par.rds")
source("Part I/shiny app/modules.R")
source("Part I/shiny app/app functions.R")
par_to_hyp<-readxl::read_excel("Part I/shiny app/par_to_hyp2.xlsx")

# dat<-readRDS("BF_data.rds")
# source("modules.R")
# source("app functions.R")



custom_minty<- bs_theme(
    version = 5,
    bootswatch = "minty"
  )

#bs_theme_preview(theme = custom_minty, with_themer = FALSE)

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme = custom_minty,
               selected = "Accuracy plots",
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
                    
                  
                    }
                                "))),
               tags$div(HTML("<script type='text/x-mathjax-config' >
                              MathJax.Hub.Config({
                              tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                              });
                              </script >
                              ")),
               tabPanel("Home"),
               tabPanel("PMP distribution plots"),
               tabPanel("Accuracy plots",
                        
                        withMathJax(),
                        fluidRow(
                        column(width = 9,
                               fluidRow(column(width = 6,
                                               hyp_UI("hyp_UI1"),
                                               uiOutput("pops")#,
                                            #   verbatimTextOutput("test")
                               ),
                               column(width = 6,
                                      hyp_UI("hyp_UI2")))
                               ),
                        column(width = 3,
                               div(style="height:90vh; display:flex;font-size:14px;",
                                   wellPanel(verbatimTextOutput("test"))
                                  )
                              )
                            )
                        )
               
               )

server<-function(input, output,session){
 hyp_UI1_selection<- hyp_server("hyp_UI1")
 
 observeEvent(hyp_UI1_selection$hyp_input_r(), {
   output$pops<-renderUI({
     
     # if(is.null(hyp_UI1_selection$hyp_input)){
     #   return(NULL)
     #   
     # }else{
     do.call(what = "tagList", 
             args=lapply(hyp_UI1_selection$hyp_input_r(), function(i){
               pop_UI(paste0("pop_UI",i),
                      n_par = hyp_UI1_selection$n_par,
                      hyp_input=i)
             })
     )
     # }
     
    })
   
 })

   specs<-reactiveValues(A=pop_server(paste0("pop_UI",1)))
   
#    observe({
# 
#    for(x in hyp_UI1_selection$hyp_input_r()){
#      specs[[x]]<-pop_server(paste0("pop_UI",x))
#    }
#   # specs<-lapply(hyp_UI1_selection$hyp_input_r(), function(i){
#   #   pop_server(paste0("pop_UI",i))
#   # })
# })
 
 # lapply(hyp_UI1_selection$hyp_input_r,function(i){
 #   pop_server(paste0("pop_UI",i))
 # })
 #pop_server(paste0("pop_UI",1))
  
 output$test<-renderPrint(specs)
}
  


shinyApp(ui = ui, server = server)
#run_with_themer(shinyApp(ui = ui, server = server))

attributes(dat)

