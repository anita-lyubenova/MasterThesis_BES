
#source("Part I/shiny app/app functions.R")



hyp_UI<-function(id) {
  ns <- NS(id)
  
  tagList(
    # radioButtons(ns("n_par"), 
    #              label="Number of parameters in the hypotheses ",
    #              choiceNames = c(#"1: $\\beta_{1}$",
    #                              "2: $\\beta_1, \\beta_2$",
    #                              "3: $\\beta_1, \\beta_2, \\beta_3$"),
    #              choiceValues = c( 2, 3),
    #              inline = FALSE,
    #              selected = 3
    # ),
   # uiOutput(ns("hyp_uiOutput"))
    checkboxGroupInput(inputId = ns("hyp_input"),
                       label = "Choose hypotheses to test",
                       choiceValues = par_to_hyp$label,
                       choiceNames =lapply(paste0(par_to_hyp$label, ": ",par_to_hyp$hyp_latex), withMathJax),
                       selected = c("H2","H2c", "Hu")
                       
    )
    
  )
}

hyp_server<-function(id) {

  moduleServer(id, function(input, output, session) {
    
  # hyps<-reactive({
  #     subset(par_to_hyp, select = c("label", "hyp_latex"))
  # 
  # })
  #    
  # output$hyp_uiOutput<-renderUI({
  #   ns <- session$ns
  #   
  # tags$div(
  #            h4(),
  #            checkboxGroupInput(inputId = ns("hyp_input"),
  #                               label = "Choose hypotheses to test",
  #                               choiceValues = hyps()$label,
  #                               choiceNames =lapply(paste0(hyps()$label, ": ",hyps()$hyp_latex), withMathJax),
  #                               selected = c("H2","H2c", "Hu")
  #                               
  #            ))
  #   
  #     })
      
   return(list(
               #n_par=reactive(input$n_par),
               hyp_input_r=reactive(input$hyp_input)
               ))
    
  })
  
}

pop_UI<-function(id, hyp_input=NULL) {
  ns <- NS(id)
  #select dataset based on n_par
  #dat<-reactive(eval(parse(text=paste0("dat",as.numeric(n_par())))))
 # dat<-reactive(dat3)
  #ratio beta choices
  bs<-reactive({
    b<-attributes(dat3)$ratio_beta
    b.names<-gsub('', ':', b) %>% 
      substr(., 2, nchar(.)-1)
    names(b)<-b.names
    c(b,"mixed")
    })
  
    
    tagList(
      wellPanel(style="padding-top:0px;margin-top:5px;margin-right:10%;",
        tags$strong(paste("Specify the population ", hyp_input)),
        fluidRow(
          column(width = 3,
                 selectInput(ns("b_input"),
                             "Ratio beta:",
                             choices = bs()
                 )
          ),
          column(width = 3,
                 selectInput(ns("p_input"),
                             "cv:",
                             choices = attributes(dat3)$p,
                             selected = 0
                 )
          ),
          column(width = 3, 
                 selectInput(ns("r2_input"),
                             "R-squared:",
                             choices = attributes(dat3)$r2,
                             selected = 0.13
                 )
          ) ,
          column(width = 3,
                 selectInput(ns("pcor_input"),
                             "Rho:",
                             choices = attributes(dat3)$pcor,
                             selected = 0.3
                 )
                 )
          
        )

  ))
}
pop_server<-function(id){

  moduleServer(id, function(input, output, session) {

    pop<-paste0("r", input$r2_input, "_pcor", input$pcor_input, "_b", input$b_input, "_p", input$p_input)
    return(pop)
    # return(list(r2=reactive(input$r2_input),
    #             pcor=reactive(input$pcor_input),
    #             p=reactive(input$p_input)))
   

  })

}

plots_UI<-function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("go_plot"), "Plot", width = "250px", style="margin-top:15px;"),
    h4("True positve rates (TPR)"),
    plotOutput(ns("TP_plot"), height = 700, width = 700),
    h4("Accuracy"),
    plotOutput(ns("acc_plot"), height = 250, width = 700),
    #verbatimTextOutput(ns("t1")),
   # verbatimTextOutput(ns("t2"))
  )
}

plots_server<-function(id, hyp_input,pop_def){
  
  moduleServer(id, function(input, output, session) {
    
    hyp<-reactive({
      subset(par_to_hyp,  label %in% hyp_input())$dimname
    })
    
    pops<-reactive({
     sapply(hyp_input(), function(x){ pop_def[[x]] })
    })
    
    hyp_to_pop<-reactive({
      hp<-sapply(hyp_input(), function(x){ paste0(x,"=","\"",pop_def[[x]],"\"")})
      hp<-paste0(hp, collapse = ",")
      hp<-paste0("c(",hp,")")
      eval(parse(text = hp))
      
    })
    
   # dat<-reactive(dat3)
    
       # output$t2<-renderPrint({
       #   TPR<-
       #     dat3%>%
       #     aggregatePMP(hyp=hyp(), #hyp(),
       #                  studies=30,
       #                  pops=pops() )%>%
       #              accuracyPMP(hyp_to_pop = hyp_to_pop(),
       #                          hyp=hyp())
       #    TPR
       #   })
    
    classif_data<-eventReactive(input$go_plot,{
      # show the modal window
      show_modal_spinner(spin = "circle",
                         color = "#78C1A9",
                         text = NULL)
      
      dat3%>%
        aggregatePMP(hyp=hyp(), #hyp(),
                     studies=30,
                     pops=pops() )%>%
        accuracyPMP(hyp_to_pop = hyp_to_pop(),
                    hyp=hyp())
    })
    # output$t1<-renderPrint({
    #   classif_data()
    # })
    # 
    TPRs<-eventReactive(classif_data(),{
      
      TPR<-classif_data()%>%
          TP_corrplot()
      
      
      TPRs<-wrap_plots(TPR, ncol=1)+
        plot_annotation(tag_levels = 'A') +
        plot_layout(guides = 'collect')&
        theme(legend.position='bottom'
              # legend.key.height = unit(3.3,"cm"),
              # legend.key.width = unit(0.5,"cm")
        )
      
      for(i in 1:length(pops())){
        TPRs[[i]]<-TPRs[[i]] + labs(subtitle = paste0(hyp()[i], ": ",pops()[i]))
      }

      remove_modal_spinner() # remove spinner when done
      
      TPRs
    })
    
    output$TP_plot<-renderPlot( TPRs() )
    
    accs<-eventReactive(input$go_plot,{
      classif_data() %>% 
        acc_corrplot(object = "acc")
    })
    output$acc_plot<-renderPlot( accs() )
    
  })
  
}


med_plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderTextInput(ns("N_input"),
                    "Choose sample size:",
                    choices =  c("15" , "25" , "35" , "50",  "75" , "100", "150", "200", "300", "500", "800"),
                    selected = "300",
                    width="60%"
    ),
    actionButton(inputId = ns("go_med_plot"), "Plot", width = "250px"),
    #plotOutput(outputId = ns("median_plot")),
    highchartOutput(outputId = ns("median_plot"))
   # verbatimTextOutput(ns("t1"))
    
  )
}


med_plot_server<-function(id, hyp_input,pop_def){
  
  moduleServer(id, function(input, output, session) {
    
    hyp<-reactive({
      subset(par_to_hyp, label %in% hyp_input())$dimname
    })
    
    
   # dat<-reactive(dat3)
    
    
    median_plot_data<-eventReactive(input$go_med_plot,{
      # # show the modal window
      # show_modal_spinner(spin = "circle",
      #                    color = "#78C1A9",
      #                    text = NULL)

      dat3%>%
        aggregatePMP(hyp=hyp(), #hyp(),
                     studies=30,
                     pops = pop_def()
                     ) %>%
        create_median_plot_data(pop=pop_def(),
                                n=input$N_input,
                                hyp=hyp())
    })
    # output$t1<-renderPrint({
    #   median_plot_data()
    # })

    
    med_plot<-eventReactive(median_plot_data(),{

      d<-median_plot_data()%>%
        median_plot(hyp_input=hyp())
       
      #remove_modal_spinner()
      d
    })

    output$median_plot<-renderHighchart( med_plot() )



   })
  
}