library(shiny)


gen_plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(ns("hyp_input"),
                       label = "Hypotheses of interest",
                       choiceNames = c("H1", "Hc", "Hu","H0"),
                       choiceValues = c("i", "c", "u","0"),
                       inline = TRUE),
    radioButtons(inputId = ns("N_input"),
                 label = "Sample size of individual studies",
                 choiceNames = c("N=100", "N=350"),
                 choiceValues = c(1,2),
                 inline = TRUE),
    actionButton(inputId = ns("go"), "Plot"),
    highchartOutput(outputId = ns("median_plot")),
    verbatimTextOutput(ns("test")),
   verbatimTextOutput(ns("test2"))
    
  )
}


###############   TROUBLESHOOTING   #########################
# PMP=PMP_H1TRUE
# all_hyp<-c("0", "i", "c", "u")
# hyp_input<-c("i", "c")
# N_input<-1
# 
# names(PMP)[1:4]
# 
# PMP[[paste0(hyp_input, collapse = "")]][,,,,N_input]
# 
# grepl(pattern=hyp_input[1], x=names(PMP)[1:4]) & grepl(pattern=hyp_input[2], x=names(PMP)[1:4])

#PMP[["ic"]][,,,,5]
###############   TROUBLESHOOTING   #########################


gen_plot_server <- function(id,PMP) { #later on PMP shoudl be reactive
  
  moduleServer(id, function(input, output, session) {
  
      # #check if there is selection for all inputs and show a message if not
      # validate(
      #   need(!is.null(input$N_input), "Please, choose sample size N"),
      #   need(lenght(input$hyp_input)<2, "Please, choose at least 2 hypotheses to test against each other")
      #   )
    
    # output$test<-renderPrint({
    #   as.numeric(input$N_input)
    # })
    
    
    observeEvent(input$go, {
      PMP_react<-reactive(PMP[[paste0(input$hyp_input, collapse = "")]][,,,,as.numeric(input$N_input)])
      
      output$test2<-renderPrint({
        create_plot_data(PMP_react())
      })
      
      output$median_plot<-renderHighchart({
          median_plot(PMP_react())
      })
      
    })
      
      
      
      # output$median_plot<-renderPlot({
      #   median.PMP.df %>%
      #     ggplot(aes(x=t, y=median_aggrPMP, group=factor(Hypothesis, levels = dimnames(aggrPMP)[[2]]), color=factor(Hypothesis, levels = dimnames(aggrPMP)[[2]])))+
      #     geom_point(position = pd)+
      #     geom_line(position = pd)+
      #     geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP), position = pd)+
      #     theme_minimal()+
      #     labs(
      #       x="Number of aggregated studies",
      #       y="Aggregate PMP")+
      #     theme(text = element_text(size = 9),
      #           axis.text.x = element_text(size = 8), #, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)
      #           legend.title = element_text(size = 7),
      #           legend.title.align=0.5,
      #           plot.subtitle = element_text(hjust = 0.5),
      #           plot.margin = unit(c(0.5,1.1,0,0.2), "cm")
      #           # legend.text = element_text(size = 10),
      #     )+
      #     # guides(colour = guide_legend(nrow = 1))+
      #     # annotate("text", x = 43, y = 0.5, label = "H1:Hc = 1:1",angle = 270)+
      #     coord_cartesian(xlim = c(0, 40), clip = "off") %>% 
      #     scale_colour_manual(values = c(col.H1,col.Hc,col.Hu ),labels=c("H1", "Hc", "Hu"),name = "Hypothesis")
      #   
      # })
      
  })

}


