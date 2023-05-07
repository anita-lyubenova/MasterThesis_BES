library(shiny)
source("Part I/shiny app/app functions.R")

gen_plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(ns("true_hyp"), 
                 label="True hypothesis in the population",
                 choiceNames = c("Hi: b1:b2:b3 = 1:2:3",
                                 "Hc: b1:b2:b3 =  3:2:1",
                                 "Hu: for 50% of the studies Hi is true, for the remaining Hc is true",
                                 "Hu: for 75% of the studies Hi is true, for the remaining Hc is true",
                                 "H0"),
                 choiceValues = c("i", "c", "u_eq", "u_larger_i", "0"),
                 inline = FALSE
                 ),
    checkboxGroupInput(ns("hyp_input"),
                       label = "Tested hypotheses",
                       choiceNames = c("Hi: b1>b2>b3", "Hc: not Hi", "Hu: {b1, b2, b3}","H0: b1=b2=b3"),
                       choiceValues = c("i", "c", "u","0"),
                       inline = TRUE,
                       selected = c("i", "c", "u")),
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
 # PMP=PMP_HuTRUE_eqES
 # 
 # hyp_input<-c("i", "c")
 # N_input<-2
# 
# names(PMP)[1:4]
# 
# PMP[[paste0(hyp_input, collapse = "")]][,,,,N_input]
# 
# grepl(pattern=hyp_input[1], x=names(PMP)[1:4]) & grepl(pattern=hyp_input[2], x=names(PMP)[1:4])

#PMP[["ic"]][,,,,5]
# true_hyp<-"1"
# PMP<-get(datafiles[names(datafiles)==true_hyp])

###############   TROUBLESHOOTING   #########################


gen_plot_server <- function(id) { #later on PMP shoudl be reactive
  
  moduleServer(id, function(input, output, session) {
  
    observeEvent(input$go, {
      #get the list of PMPs belonging to the user-specified population
      PMP<-reactive(get(datafiles[names(datafiles)==input$true_hyp]))
      
      #from the PMPs list, select the array that tests the user-specified hypotheses withe the user-specified sample size N
      PMP_sub<-reactive(PMP()[[paste0(input$hyp_input, collapse = "")]][,,1,,as.numeric(input$N_input)])
      
      output$test<-renderPrint({
        create_plot_data(PMP_sub())
      })
      output$median_plot<-renderHighchart({
          median_plot(PMP_sub(), hyp_input=input$hyp_input)
      })
      
    })
      
  })

}


med_plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderTextInput(ns("pcor_input"),
                    "Choose sample size:",
                    choices =  c("15" , "25" , "35" , "50",  "75" , "100", "150", "200", "300", "500", "800"),
                    selected = "300"
                    ),
    actionButton(inputId = ns("go"), "Plot"),
    highchartOutput(outputId = ns("median_plot")),
    verbatimTextOutput(ns("test"))
    
  )
}

hyp_UI<-function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(ns("n_par"), 
                 label="Number of parameters in the hypotheses ",
                 choiceNames = c(#"1: $\\beta_{1}$",
                                 "2: $\\beta_1, \\beta_2$",
                                 "3: $\\beta_1, \\beta_2, \\beta_3$"),
                 choiceValues = c( 2, 3),
                 inline = FALSE,
                 selected = 3
    ),
    uiOutput(ns("hyp_uiOutput"))
    
  )
}

hyp_server<-function(id) {

  moduleServer(id, function(input, output, session) {
    
  hyps<-reactive({
      subset(par_to_hyp, n_par==input$n_par, select = c("label", "hyp_latex"))
  
  })
     
  output$hyp_uiOutput<-renderUI({
    ns <- session$ns
    
  tags$div(
             h4(),
             checkboxGroupInput(inputId = ns("hyp_input"),
                                label = "Choose hypotheses to test",
                                choiceValues = hyps()$label,
                                choiceNames =lapply(paste0(hyps()$label, ": ",hyps()$hyp_latex), withMathJax),
                                selected = c("H2","H2c", "Hu")
                                
             ))
    
      })
      
   return(list(
               n_par=reactive(input$n_par),
               hyp_input_r=reactive(input$hyp_input)
               ))
    
  })
  
}

pop_UI<-function(id,n_par, hyp_input) {
  ns <- NS(id)
  #select dataset based on n_par
  dat<-reactive(eval(parse(text=paste0("dat",as.numeric(n_par())))))
  #ratio beta choices
  bs<-reactive({
    b<-attributes(dat())$ratio_beta
    b.names<-gsub('', ':', b) %>% 
      substr(., 2, nchar(.)-1)
    names(b)<-b.names
    c(b,"mixed")
    })
  
    
    tagList(
      wellPanel(style="padding-top:0px;margin-top:5px;margin-right:10%;",
        tags$strong(paste("Specify the population of ", hyp_input)),
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
                             choices = attributes(dat())$p,
                             selected = 0
                 )
          ),
          column(width = 3, 
                 selectInput(ns("r2_input"),
                             "R-squared:",
                             choices = attributes(dat())$r2,
                             selected = 0.13
                 )
          ) ,
          column(width = 3,
                 selectInput(ns("pcor_input"),
                             "Rho:",
                             choices = attributes(dat())$pcor,
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

plots_server<-function(id, hyp_input,pop_def, n_par){
  
  moduleServer(id, function(input, output, session) {
    
    hyp<-reactive({
      subset(par_to_hyp, n_par==n_par() & label %in% hyp_input())$dimname
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
    
    dat<-reactive(eval(parse(text=paste0("dat",as.numeric(n_par())))))
    
       # output$t2<-renderPrint({
       #   TPR<-
       #     dat()%>%
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
      
      dat()%>%
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