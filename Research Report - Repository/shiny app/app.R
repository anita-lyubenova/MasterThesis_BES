library(shiny)
library(bslib)

custom_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Lato"),
  heading_font = font_google("Raleway")
)
bs_theme_preview(theme = custom_theme, with_themer = TRUE)



ui<-navbarPage(title="Bayesian Evidence Sythesis",
               theme=custom_theme)

server<-function(input, output, session){}

run_with_themer(shinyApp(ui = ui, server = server))


















