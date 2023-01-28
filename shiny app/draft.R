
library(shiny)
library(shinythemes)
library(htmltools)

source("scripts/Aggregate PMPs.R")

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme=shinytheme("flatly"),
               tabPanel("Hu is TRUE",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       radioButtons(inputId="ratio_input",
                                                    label = "Ratio of studies originating from H1:Hc",
                                                    choices = c("1:1", "3:1")),
                                       
                                       radioButtons(inputId = "ratio_beta_input",
                                                    label= "Ratio of the regression coefficient in the populations of H1 and Hc",
                                                    choiceValues = c("Hc_equal", "Hc_smaller"),
                                                    choiceNames = c("H1: b1:b2:b3=3:2:1 &
                                                                     Hc: b1:b2:b3=1:2:3",
                                                                    "H1: b1:b2:b3=3:2:1 &
                                                                     Hc: b1:b2:b3=1:1.5:2"
                                                    )
                                       )
                                       
                          ),
                          mainPanel(column(width = 6,
                                           wellPanel(
                                             checkboxGroupInput("Hu_hyp_input1",
                                                                label = "Hypotheses of interest",
                                                                choices = c("H0","H1", "Hc", "Hu"),
                                                                inline = TRUE),
                                             radioButtons(inputId = "Hu_N_input1",
                                                          label = "Sample size of individual studies",
                                                          choiceNames = c("N=100", "N=350"),
                                                          choiceValues = c(100,350),
                                                          inline = TRUE),
                                             actionButton(inputId = "go1", "Plot")
                                           ),
                                           plotOutput(outputId = "Hu_plot1"),
                                           plotOutput(outputId = "Hu_plot3"),
                                           wellPanel(
                                             checkboxGroupInput("Hu_hyp_input3",
                                                                label = "Hypotheses of interest",
                                                                choices = c("H0","H1", "Hc", "Hu"),
                                                                inline = TRUE),
                                             radioButtons(inputId = "Hu_N_input3",
                                                          label = "Sample size of individual studies",
                                                          choiceNames = c("N=100", "N=350"),
                                                          choiceValues = c(100,350),
                                                          inline = TRUE),
                                             actionButton(inputId = "go3", "Plot")
                                           )
                          ),
                          column(width = 6,
                                 wellPanel(
                                   checkboxGroupInput("Hu_hyp_input2",
                                                      label = "Hypotheses of interest",
                                                      choices = c("H0","H1", "Hc", "Hu"),
                                                      inline = TRUE),
                                   radioButtons(inputId = "Hu_N_input2",
                                                label = "Sample size of individual studies",
                                                choiceNames = c("N=100", "N=350"),
                                                choiceValues = c(100,350),
                                                inline = TRUE),
                                   actionButton(inputId = "go2", "Plot")
                                 ),
                                 plotOutput(outputId = "Hu_plot2"),
                                 plotOutput(outputId = "Hu_plot4"),
                                 wellPanel(
                                   checkboxGroupInput("Hu_hyp_input4",
                                                      label = "Hypotheses of interest",
                                                      choices = c("H0","H1", "Hc", "Hu"),
                                                      inline = TRUE),
                                   radioButtons(inputId = "Hu_N_input4",
                                                label = "Sample size of individual studies",
                                                choiceNames = c("N=100", "N=350"),
                                                choiceValues = c(100,350),
                                                inline = TRUE),
                                   actionButton(inputId = "go4", "Plot")
                                 )
                          )
                          )
                        )
               ),
               tabPanel("H1 is TRUE"),
               tabPanel("Hc is TRUE")
)


server<-function(input,output){
  
 
  
  
}

shinyApp(ui,server)

###################################################################################

library(shiny)
library(shinythemes)
library(htmltools)
library(shinybusy)

#for the figures
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(shades)
library(plotly)
library(patchwork)

source("RRrepo/scripts/Aggregate PMPs.R")
create_plot_data<-function(aggrPMP){ 
  medians<-apply(aggrPMP,c(1,2),median)%>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(aggrPMP)[[2]],
                 names_to = "Hypothesis",
                 values_to = "median_aggrPMP") %>%
    mutate(Hypothesis=as.factor(Hypothesis),
           t=factor(t, levels = unique(t))
    )
  
  #lower bounds of the aggregate values per hypothesis h per study number t
  lbs<-apply(aggrPMP,c(1,2),function(x) quantile(x,probs=c(0.025))) %>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(aggrPMP)[[2]],
                 names_to = "Hypothesis",
                 values_to = "lb_aggrPMP") %>%
    mutate(Hypothesis=as.factor(Hypothesis),
           t=factor(t, levels = unique(t))
    )  
  
  ubs<-apply(aggrPMP,c(1,2),function(x) quantile(x,probs=c(0.975))) %>% 
    as.data.frame() %>% 
    rownames_to_column(var="t") %>% 
    pivot_longer(cols = dimnames(aggrPMP)[[2]],
                 names_to = "Hypothesis",
                 values_to = "ub_aggrPMP") %>%
    mutate(Hypothesis=as.factor(Hypothesis),
           t=factor(t, levels = unique(t))
    )  
  
  df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
    left_join(., ubs, by=c("t", "Hypothesis"))
  
  return(df)
}

pd <- position_dodge(width = 0.4) # set desired dodge width
col.H1<-"#7fc97f"
col.Hc<-"#fdc086"
col.Hu<-"black"
lightness(col.H1, scalefac(0.50))

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme=shinytheme("flatly"),
               tabPanel("Hu is TRUE",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       radioButtons(inputId="ratio_input",
                                                    label = "Ratio of studies originating from H1:Hc",
                                                    choiceNames = c("1:1", "3:1"),
                                                    choiceValues = c(1,2)),
                                       
                                       radioButtons(inputId = "ratio_beta_input",
                                                    label= "Ratio of the regression coefficient in the populations of H1 and Hc",
                                                    choiceValues = c("Hc_equal", "Hc_smaller"),
                                                    choiceNames = c("H1: b1:b2:b3=3:2:1 &
                                                                     Hc: b1:b2:b3=1:2:3",
                                                                    "H1: b1:b2:b3=3:2:1 &
                                                                     Hc: b1:b2:b3=1:1.5:2"
                                                    )
                                       )
                                       
                          ),
                          mainPanel(column(width = 6,
                                           wellPanel(
                                             checkboxGroupInput("Hu_hyp_input1",
                                                                label = "Hypotheses of interest",
                                                                choiceNames = c("H0","H1", "Hc", "Hu"),
                                                                choiceValues = c("0", "i", "c", "u"),
                                                                inline = TRUE),
                                             radioButtons(inputId = "Hu_N_input1",
                                                          label = "Sample size of individual studies",
                                                          choiceNames = c("N=100", "N=350"),
                                                          choiceValues = c(1,2),
                                                          inline = TRUE),
                                             actionButton(inputId = "go1", "Plot")
                                           ),
                                           plotOutput(outputId = "Hu_plot1"),
                                           verbatimTextOutput("test")
                          ),
                          column(width = 6,
                                 wellPanel(
                                   checkboxGroupInput("Hu_hyp_input2",
                                                      label = "Hypotheses of interest",
                                                      choiceNames = c("H0","H1", "Hc", "Hu"),
                                                      choiceValues = c("0", "i", "c", "u"),
                                                      inline = TRUE),
                                   radioButtons(inputId = "Hu_N_input2",
                                                label = "Sample size of individual studies",
                                                choiceNames = c("N=100", "N=350"),
                                                choiceValues = c(1,2),
                                                inline = TRUE),
                                   actionButton(inputId = "go2", "Plot")
                                 ),
                                 plotOutput(outputId = "Hu_plot2")
                          )
                          )
                        )
               ),
               tabPanel("H1 is TRUE"),
               tabPanel("Hc is TRUE")
)


server<-function(input,output){
  
  observeEvent(input$go1, {
    if(input$ratio_beta_input=="Hc_equal"){
      load("workspaces/Simulate BFs_ES(H1)=ES(Hc).RData")
      
    }else{
      load("workspaces/Simulate BFs_ES(H1) larger than ES(Hc).RData")
    }
    #check if there is selection for all inputs and show a message if not
    validate(need(!is.null(input$Hu_N_input1), "Please make your selection!"))
    
    # show the modal window
    show_modal_spinner(spin = "circle",
                       color = "#46ADE9",
                       text = "It can take a few moments, please wait...",)
    aggrPMP<-aggregatePMP(BF=BF, hyp = input$Hu_hyp_input1, iter=iter)
    
    remove_modal_spinner() # remove spinner when done
    
    aggrPMP<-aggrPMP[,,as.numeric(input$ratio_input),,as.numeric(input$Hu_N_input1)]
    median.PMP.df<-create_plot_data(aggrPMP)
    
    output$test<-renderPrint({
      median.PMP.df
    })
    
    output$Hu_plot1<-renderPlot({
      median.PMP.df%>%
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
  
  
  observeEvent(input$go2, {
    if(input$ratio_beta_input=="Hc_equal"){
      load("workspaces/Simulate BFs_ES(H1)=ES(Hc).RData")
      
    }else{
      load("workspaces/Simulate BFs_ES(H1) larger than ES(Hc).RData")
    }
    #check if there is selection for all inputs and show a message if not
    validate(need(!is.null(input$Hu_N_input3), "Please make your selection!"))
    
    # show the modal window
    show_modal_spinner(spin = "circle",
                       color = "#46ADE9",
                       text = "It can take a few moments, please wait...",)
    aggrPMP<-aggregatePMP(BF=BF, hyp = input$Hu_hyp_input3, iter=iter)
    
    remove_modal_spinner() # remove spinner when done
    
    aggrPMP<-aggrPMP[,,as.numeric(input$ratio_input),,as.numeric(input$Hu_N_input3)]
    median.PMP.df<-create_plot_data(aggrPMP)
    
    output$test<-renderPrint({
      median.PMP.df
    })
    
    output$Hu_plot3<-renderPlot({
      median.PMP.df%>%
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

shinyApp(ui,server)



