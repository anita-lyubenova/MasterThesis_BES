library(shiny)


gen_plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(ns("true_hyp"), 
                 label="True hypothesis in the population",
                 choiceNames = c("Hi", "Hc", "Hu"),
                 choiceValues = c("1", "c", "u"),
                 inline = TRUE
                 ),
    checkboxGroupInput(ns("hyp_input"),
                       label = "Hypotheses of interest",
                       choiceNames = c("Hi", "Hc", "Hu","H0"),
                       choiceValues = c("i", "c", "u","0"),
                       inline = TRUE),
    radioButtons(inputId = ns("N_input"),
                 label = "Sample size of individual studies",
                 choiceNames = c("N=100", "N=350"),
                 choiceValues = c(1,2),
                 inline = TRUE),
    actionButton(inputId = ns("go"), "Plot"),
    highchartOutput(outputId = ns("median_plot")),
    verbatimTextOutput(ns("test"))
    
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
true_hyp<-"1"
get(datafiles[names(datafiles)==true_hyp])
###############   TROUBLESHOOTING   #########################


gen_plot_server <- function(id) { #later on PMP shoudl be reactive
  
  moduleServer(id, function(input, output, session) {
  
      # #check if there is selection for all inputs and show a message if not
      # validate(
      #   need(!is.null(input$N_input), "Please, choose sample size N"),
      #   need(lenght(input$hyp_input)<2, "Please, choose at least 2 hypotheses to test against each other")
      #   )
    PMP<-reactive(get(datafiles[names(datafiles)==input$true_hyp]))
    
    observeEvent(input$go, {
      PMP_sub<-reactive(PMP()[[paste0(input$hyp_input, collapse = "")]][,,,,as.numeric(input$N_input)])
      
      output$median_plot<-renderHighchart({
          median_plot(PMP_sub(), hyp_input=input$hyp_input)
      })
      
    })
      
    # return(list(
    #   true_hyp = reactive({input$true_hyp})
    # ))
    
      
  })

}


