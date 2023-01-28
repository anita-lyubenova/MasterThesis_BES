library(shiny)
library(bslib)
library(shinyWidgets)


custom_minty<- bs_theme(
    version = 5,
    bootswatch = "minty"
  )

#bs_theme_preview(theme = custom_minty, with_themer = FALSE)

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme = custom_minty,
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
                                "))
               ),
               tabPanel("Home"),
               tabPanel("PMPs distributions"),
               tabPanel("BES-power plots"),
               
               )

server<-function(input, output, session){}

shinyApp(ui = ui, server = server)
#run_with_themer(shinyApp(ui = ui, server = server))


#seecolor::print_color(c("#8ed1be", "#6ECCAE", "#73BAA4"), type = "r")







