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
library(shinybusy)
library(patchwork)


# dat3<-readRDS("Part I/pre-processing/output/BF_data_3par_hpc_final_mixed.rds")
# dat2<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_2par.rds")
# dat1<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_1par.rds")
 source("Part I/shiny app/modules.R")
# source("Part I/shiny app/app functions.R")
par_to_hyp<-readxl::read_excel("Part I/shiny app/par_to_hyp2.xlsx")

# dat<-readRDS("BF_data.rds")
# source("modules.R")
# source("app functions.R")

#bs<-attributes(dat3)$ratio_beta

custom_minty<- bs_theme(
    version = 5,
    bootswatch = "minty"
  )

#bs_theme_preview(theme = custom_minty, with_themer = FALSE)

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme = custom_minty,
               selected = "Home",
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
               tabPanel("Home",
                        
                        h3(style="margin-left:80px; margin-top:100px;color:#78C1A9;","Bayesian Evidence Synthesis: The Value of The Unconstrained Hypothesis"),
                        
                        div(style="width:50%;margin-top:30px; margin-left:80px;",
                            tags$p("THis Shiny App provides additional simulation conditions to those 
                               presented in Lyubenova ((2023)). Bayesian Evidence Synthesis: The 
                               Value of the Unconstrained Hypothesis. Two different types of plots
                               can be obtained in the tabs \"PMP distribution plots\" and \"Accuracy plots\".
                               In each two different simulation conditions can be compared.")
                            )
                        
                        ),
               tabPanel("PMP distribution plots",
                        withMathJax(),
                        fluidRow(
                          column(width = 12,
                                 fluidRow(
                                   column(width = 6,
                                                 hyp_UI("hyp_UI1_med"),
                                                 uiOutput("pop1_med"),
                                                 med_plot_UI("plot_UI1_med"),
                                                verbatimTextOutput("test_outer")
                                   ),
                                   column(width = 6,
                                          hyp_UI("hyp_UI2_med"),
                                          uiOutput("pop2_med"),
                                          med_plot_UI("plot_UI2_med")
                                          #   verbatimTextOutput("test")
                                          )
                                   )
                          )
                        )
                        ),
               tabPanel("Accuracy plots",
                        
                        withMathJax(),
                        fluidRow(
                        column(width = 12,
                               fluidRow(column(width = 6,
                                               hyp_UI("hyp_UI1_acc"),
                                               uiOutput("pops1_acc"),
                                               plots_UI("plots_UI1_acc")
                               ),
                               column(width = 6,
                                      hyp_UI("hyp_UI2_acc"),
                                      uiOutput("pops2_acc"),
                                      plots_UI("plots_UI2_acc")
                                      )
                               )
                               )
                        # ,
                        # column(width = 3,
                        #        div(style="height:90vh; display:flex;font-size:14px;",
                        #            wellPanel(verbatimTextOutput("test_outer"))
                        #           )
                        #       )
                            )
                        )
               
               )

server<-function(input, output,session){
# Tab PMPs: Panel 1 --------------------------------
  hyp_UI1_med_selection<- hyp_server("hyp_UI1_med")

  observeEvent(hyp_UI1_med_selection$hyp_input_r(), {
    output$pop1_med<-renderUI({
      pop_UI("pop_UI1_med",
             n_par = hyp_UI1_med_selection$n_par,
             hyp_input=NULL
             )
    })
  })
  
  #Create a placeholder for the population specifications
  #unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
  pop_def1_med<-reactive(pop_server("pop_UI1_med"))

  med_plot_server("plot_UI1_med",
               hyp=hyp_UI1_med_selection$hyp_input_r,
               pop_def = pop_def1_med,
               n_par = hyp_UI1_med_selection$n_par)

  
  # Tab PMPs: Panel 2 --------------------------------
  hyp_UI2_med_selection<- hyp_server("hyp_UI2_med")

  observeEvent(hyp_UI2_med_selection$hyp_input_r(), {
    output$pop2_med<-renderUI({
      pop_UI("pop_UI2_med",
             n_par = hyp_UI2_med_selection$n_par,
             hyp_input=NULL
      )
    })
  })

  #Create a placeholder for the population specifications
  #unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
  pop_def2_med<-reactive(pop_server("pop_UI2_med"))

  med_plot_server("plot_UI2_med",
                  hyp=hyp_UI2_med_selection$hyp_input_r,
                  pop_def = pop_def2_med,
                  n_par = hyp_UI2_med_selection$n_par)

  
  # output$test_outer<-renderPrint(hyp_UI1_med_selection$n_par())

# Tab Accuracy: Panel 1 --------------------------------
 hyp_UI1_acc_selection<- hyp_server("hyp_UI1_acc")
 
 observeEvent(hyp_UI1_acc_selection$hyp_input_r(), {
   
   output$pops1_acc<-renderUI({

     if(is.null(hyp_UI1_acc_selection$hyp_input_r())){
       return(NULL)

     }else{
     do.call(what = "tagList",
             args=lapply(hyp_UI1_acc_selection$hyp_input_r(), function(i){
               pop_UI(paste0("pop_UI1_acc",i),
                      n_par = hyp_UI1_acc_selection$n_par,
                      hyp_input=i)
             })
       )
     }
    })
 })
 
 #Create a placeholder for the population specifications 
 #unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
 pop_def1<-reactiveValues()
   #save the population specificatins
   observe({
     for(x in hyp_UI1_acc_selection$hyp_input_r()){
       #pop_def1[[x]] will be a list with 3 elements, r2, pcor and p
       pop_def1[[x]]<-pop_server(paste0("pop_UI1_acc",x))
     }
  })
  
 
plots_server("plots_UI1_acc",
             hyp=hyp_UI1_acc_selection$hyp_input_r,
             pop_def = pop_def1,
             n_par = hyp_UI1_acc_selection$n_par)
 

# Tab Accuracy: Panel 2 --------------------------------
hyp_UI2_acc_selection<- hyp_server("hyp_UI2_acc")
observeEvent(hyp_UI2_acc_selection$hyp_input_r(), {
  
  output$pops2_acc<-renderUI({
    
    if(is.null(hyp_UI2_acc_selection$hyp_input)){
      return(NULL)
      
    }else{
      do.call(what = "tagList",
              args=lapply(hyp_UI2_acc_selection$hyp_input_r(), function(i){
                pop_UI(paste0("pop_UI2_acc",i),
                       n_par = hyp_UI2_acc_selection$n_par,
                       hyp_input=i)
              })
      )
    }
  })
})

#Create a placeholder for the population specifications 
#unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
pop_def2<-reactiveValues()
#save the population specificatins
observe({
  for(x in hyp_UI2_acc_selection$hyp_input_r()){
    #pop_def2[[x]] will be a list with 3 elements, r2, pcor and p
    pop_def2[[x]]<-pop_server(paste0("pop_UI2_acc",x))
  }
})


plots_server("plots_UI2_acc",
             hyp=hyp_UI2_acc_selection$hyp_input_r,
             pop_def = pop_def2,
             n_par = hyp_UI2_acc_selection$n_par)



}
  


shinyApp(ui = ui, server = server)
#run_with_themer(shinyApp(ui = ui, server = server))

attributes(dat)

