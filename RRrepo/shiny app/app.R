library(shiny)
library(bslib)


#
ui<-navbarPage(title="Bayesian Evidence Sythesis",
               includeCSS("www/bootstrap.css")
               )

server<-function(input, output, session){}

shinyApp(ui = ui, server = server)

















