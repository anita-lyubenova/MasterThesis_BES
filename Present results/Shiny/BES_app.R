library(shiny)
library(shinythemes)
#öoad results for sim1, k=2
load("Outputs/sim1/Sim1(k=2, 10 conditions).RData")


ui <- navbarPage(
  id="navbar",
  title="Power in BES",
  fluid = TRUE,
  selected = "Introduction",
  theme = shinytheme("flatly"),
  tabPanel(title = "Introduction"),
  tabPanel(title = "Simulation 0: Power"),
  tabPanel(title =  "Simulation 1")
  )

server<-function(input, output){}

shinyApp(ui, server)
