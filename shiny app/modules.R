library(shiny)


gen_plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(ns("hyp_input"),
                       label = "Hypotheses of interest",
                       choices = c("H0","H1", "Hc", "Hu"),
                       inline = TRUE),
    radioButtons(inputId = ns("N_input"),
                 label = "Sample size of individual studies",
                 choiceNames = c("N=100", "N=350"),
                 choiceValues = c(100,350),
                 inline = TRUE),
    actionButton(inputId = ns("go"), "Plot"),
    plotOutput(outputId = ns("median_plot")),
    verbatimTextOutput(ns("test"))
    
  )
}

gen_plot_server <- function(id,PMP) { #later on PMP shoudl be reactive
  
  moduleServer(id, function(input, output, session) {
  
      #check if there is selection for all inputs and show a message if not
      validate(
        need(!is.null(input$N_input), "Please, choose sample size N"),
        need(lenght(input$hyp_input)<2, "Please, choose at least 2 hypotheses to test against each other")
        )
    
      PMP<-PMP[[paste0(input$hyp_input, collapse = "")]]
      PMP<-PMP[,,,,input$N_input]
      
      median.PMP.df<-create_plot_data(PMP)
      
      output$test<-renderPrint({
        median.PMP.df
      })
      
      
      output$plot<-renderPlot({
        median.PMP.df %>%
          ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = dimnames(aggrPMP)[[2]]), color=factor(Hypothesis, levels = dimnames(aggrPMP)[[2]])))+
          geom_point(position = pd)+
          geom_line(position = pd)+
          geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP), position = pd)+
          theme_minimal()+
          labs(
            x="Number of aggregated studies",
            y="Aggregate PMP")+
          theme(text = element_text(size = 9),
                axis.text.x = element_text(size = 8), #, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)
                legend.title = element_text(size = 7),
                legend.title.align=0.5,
                plot.subtitle = element_text(hjust = 0.5),
                plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
                # legend.text = element_text(size = 10),
          )+
          # guides(colour = guide_legend(nrow = 1))+
          # annotate("text", x = 43, y = 0.5, label = "H1:Hc = 1:1",angle = 270)+
          coord_cartesian(xlim = c(0, 40), clip = "off") %>% 
          scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "Hc", "Hu"),name = "Hypothesis")
        
      })
      
  })

}


