library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(shades)
library(tidyverse)
library(highcharter)
library(htmltools)
library(htmlwidgets)
library(shinybusy)
library(patchwork)


# dat2<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_2par.rds")
# dat1<-readRDS("Part I/pre-processing/output_ShinyApp/BF_data_1par.rds")

# dat3<-readRDS("Part I/pre-processing/output/BF_data_3par_hpc_final_mixed.rds")
# source("Part I/shiny app/modules.R")
# par_to_hyp<-readxl::read_excel("Part I/shiny app/par_to_hyp3.xlsx")

#upload paths
dat3<-readRDS("BF_data_3par_hpc_final_mixed.rds")
par_to_hyp<-readxl::read_excel("par_to_hyp3.xlsx")
#source("app functions.R")
#source("modules.R")

#App functions ----------------------------------------------

#library(ggpubr)
# 


#a function to compute aggregate PMPs for selected hypotheses from the BFs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10, #number of studies to aggregate over, max 40,
                       subset=NULL,
                       pops = c("r0.13_pcor0.3_b321_p0","r0.13_pcor0.3_bmixed_p0")
){
  #subset if specified, while retaining the attributes
  if(!is.null(subset)){
    att<-attributes(x)
    att<-att[names(att)[-grep("dim", names(att))]]
    x<-eval(parse(text = subset))
    attributes(x)<-c(attributes(x), att)
  }
  
  
  #subset the hypotheses and populations of interest
  ##hyp_index<-substr(hyp,2,2)
  #search_terms <- paste0(hyp, collapse = "|")
  BF<-x
  BF<-BF[1:studies,hyp,pops,,,drop=FALSE]
  
  nom<-aperm(BF, perm=c(1,3,4,5,2))
  denom<-rowSums(nom, dims = 4)
  PMP_perm<-nom/replicate(length(hyp),denom)
  
  #placeholder for tha aggregated PMPs
  PMP_t<-PMP_perm
  for(t in 2:studies){ #the PMPs of the first study remain the same, thus iterate from t=2
    nom_t<-PMP_t[t-1,,,,,drop=FALSE]*nom[t,,,,,drop=FALSE]#multiply the previous PMPs with the current BFs
    denom_t<-rowSums(nom_t, dims=length(dim(nom_t))-1) #sum the last dimension
    PMP_t[t,,,,]<-nom_t/replicate(length(hyp), denom_t)
  }
  
  PMP_t<-aperm(PMP_t, perm = c(1,5,2,3,4))
  dimnames(PMP_t)[[2]]<-paste0("PMP_", hyp)
  
  PMP_t<-  rlist::list.append(attributes(x), PMP=PMP_t, hypothesis_test = paste(hyp, collapse = " vs. ")) 
  return(PMP_t)
}



# hyp<-c("H1.V1>V2>V3>0", "H1.complement")
# hyp_to_pop<-c(H1="r0.09_pcor0_b321_p0", H1c="r0.09_pcor0_b321_p0" )
#a function to compute confusion matrix from aggregated PMPs
accuracyPMP<-function(listPMP, #list created with aggregatePMP()
                      hyp_to_pop, # a named character vector; elements are subset of dimnames(PMP)[[3]]: "TRUE_H0", "TRUE_H1", "TRUE_Hc", "TRUE_Hu", "HETEROG_H1p.1", "HETEROG_H1p.3", "HETEROG_H1p.5", names are the hypotheses for which the populations are true
                      hyp
){
  #subset the populations of interest
  # PMP<-listPMP$PMP[,,hyp_to_pop,,]
  PMP<-listPMP$PMP
  dim(PMP)
  #hyp_index<-substr(names(hyp_to_pop),2,2)
  
  tr<-data.frame(hyp=names(hyp_to_pop),
                 PMP=paste0("PMP_", hyp),
                 pop=hyp_to_pop
  )
  # a character vector whose elements are the names of the variables storing the correct classifications for each population
  correct_name<-paste0("c", tr$pop)
  
  #a loop that computes the true classifications for each population and stores it into the respective variables
  for(h in 1:nrow(tr)){
    
    #a character string that specifies which comparison should be made
    # the comparison yields TRUE if is the PMPs of the true Hypothesis of the current population [h] are larger than the remaining PMPs
    comp<- paste(paste0("PMP[,tr$PMP[h],tr$pop[h],,,drop=FALSE] > ",
                        paste0("PMP[,",which(!dimnames(PMP)[[2]] %in% tr$PMP[h]), ",tr$pop[h],,,drop=FALSE]")
    ),collapse = " & ")
    
    #perform the comparison specified in the string
    correct_logical<-eval(parse(text = comp))
    
    #compute the number of correct classifications by summing the TRUE values
    correct_count<-rowSums(aperm(correct_logical, c(1,2,3,5,4)), dims = 4)[,,,,drop=TRUE]
    
    #store the number of correct classifications in the respective variable
    assign(correct_name[h], correct_count)
  } 
  #the result of the loop are variables with names specified in correct_name
  #there is a variable for each population 
  #each variable contains the true classifications for each population
  
  
  true<-lapply( mget(correct_name), function(x) x/listPMP$dim[[4]])
  
  acc<-eval(parse(text = paste(correct_name, collapse = "+")))/(nrow(tr)*listPMP$dim[[4]])
  
  list(acc=round(acc, digits = 3),
       TP=true,
       r2=listPMP$r2,
       pcor=listPMP$pcor,
       hypothesis=listPMP$hypothesis,
       model=listPMP$model,
       iter=listPMP$iter,
       studies=listPMP$studies,
       populations=listPMP$populations[hyp_to_pop],
       hypothesis_test=listPMP$hypothesis_test,
       hyp_to_pop=hyp_to_pop,
       n=listPMP$n
  )
  
}


##a custom corrlot with a many colors 
acc_corrplot<-function(a, # a list created with accuracyPMP()
                       object="acc" # what should be plotted? acc or TP?
                       #    b=NULL #optional: another list created with accuracyPMP to plot differences in acc
){
  #create the data to plot
  
  x<-
    a[[object]] %>% 
    reshape2::melt() %>% 
    rename(n=Var2,
           t=Var1) %>%
    mutate(n=factor(n),
           t=factor(t)
    )
  #show accuracy text only for certain t
  q<-a[[object]]
  q[-c(1,nrow(q)),]<-NA
  label.df<-
    reshape2::melt(q)%>%
    mutate(color=case_when(value<.65 ~ "white",
                           value>=.65~ "black"
    )) %>%
    pull(value,color) %>%
    round(.,digits=2) %>%
    format(nsmall=2) %>%
    gsub("^0", "", .) %>%
    gsub("NA", NA, .)
  
  #create line data
  l<-a[[object]] 
  ld<-data.frame(x=NA,
                 y=1:ncol(l))
  #for each sample size (row) determine the first t that reaches accuracy =0.87
  for(i in 1:ncol(l)){
    ld[i,"x"]<- suppressWarnings(min(which(l[,i]>=0.865)))
  }
  ld[ld$x=="Inf",]<-matrix(c(rep(c(NA,NA), times=sum(ld$x=="Inf")-1),
                             nrow(l)-0.3,max(ld[ld$x=="Inf",]$y)+1
  ), ncol=2,byrow = TRUE)
  
  
  pal<-c(  "#1344CD"  ,"#481568FF","#A67DC4" ,"#D5984D", "#FDE725FF","#1F968BFF")
  
  ggplot(data = x, mapping = aes(x=t, y=n, fill=value))+
    geom_tile()+#color = "white"
    scale_fill_gradientn(colours =pal, #c("#481568FF","#ACAD94","#A77E82","#D2973F","#FDE725FF","#1F968BFF"), #  c("#481568FF","#AC82C9","#FDE725FF","#1F968BFF")
                         limit = c(0, 1),
                         breaks=c(0,0.10,0.20, 0.30, 0.4,0.50,0.60,0.70,0.80, 0.87, 0.95, 1),
                         space = "Lab",
                         name = "Accuracy",
                         values = scales::rescale(c(0,0.5,0.70,0.8,0.87,1))
    )+
    geom_point(data = ld[unique(ld$y),], mapping = aes(x=x, y=y), inherit.aes = FALSE)+#
    geom_step(data = ld[!duplicated(ld$x),], mapping = aes(x=x, y=y), inherit.aes = FALSE)+
    geom_text(mapping = aes(x=t, y=n),
              label = label.df,
              color= names(label.df),  #"white",
              size = 3)+
    labs(#title="Accuracy",
      x="Number of aggregated studies", y="Sample size")+
    theme_minimal()+
    theme(legend.position="bottom",
          legend.key.width=unit(2.6,"cm")
    )
}#end acc_corrplot()

TP_corrplot<-function(a# a list created with accuracyPMP() containgin TPRs
){
  i<-0
  #TPm<-a$TP$cH1_r.13_pcor.3_b321_p.86_linear
  TPplots<-lapply(1: length(a$TP), function(i){
    TPm<-a$TP[[i]]
    x<-
      TPm %>% 
      reshape2::melt() %>% 
      rename(n=Var2,
             t=Var1) %>%
      mutate(n=factor(n),
             t=factor(t)
      )
    #show accuracy text only for certain t
    q<-TPm
    q[-c(1,nrow(q)),]<-NA
    label.df<-
      reshape2::melt(q)%>%
      mutate(color=case_when(value<=.65 ~ "white",
                             value>=.56~ "black"
      )) %>%
      pull(value,color) %>%
      round(.,digits=2) %>%
      format(nsmall=2) %>%
      gsub("^0", "", .) %>%
      gsub("NA", NA, .)
    
    
    
    pal<-c(  "#1344CD"  ,"#481568FF","#A67DC4" ,"#D5984D", "#FDE725FF","#1F968BFF")
    
    ggplot(data = x, mapping = aes(x=t, y=n, fill=value))+
      geom_tile()+#color = "white"
      scale_fill_gradientn(colours =pal, #c("#481568FF","#ACAD94","#A77E82","#D2973F","#FDE725FF","#1F968BFF"), #  c("#481568FF","#AC82C9","#FDE725FF","#1F968BFF")
                           limit = c(0, 1),
                           breaks=c(0,0.10,0.20, 0.30, 0.4,0.50,0.60,0.70,0.80, 0.87, 0.95, 1),
                           space = "Lab",
                           name = "TPR",
                           values = scales::rescale(c(0,0.5,0.70,0.8,0.87,1))
      )+
      geom_text(mapping = aes(x=t, y=n),
                label = label.df,
                color= names(label.df),  #"white",
                size = 3)+
      labs(
        subtitle = paste0("MPCTH: ", names(a$hyp_to_pop)[i]),
        x="Number of aggregated studies",
        y="Sample size"
      )+
      theme_minimal()+
      theme(legend.position="bottom",
            legend.key.width=unit(3,"cm")
      )
    
  } )
  return(TPplots)
  
  # print(TPplots)#
  # wrap_plots(TPplots, ncol=1, guides = "collect")
  # ggarrange(plotlist=TPplots, ncol=1, common.legend = TRUE, legend = "bottom",labels="AUTO")
  
  # annotate_figure(printTPs, top = text_grob("True positive rates for different MPCTH", 
  #                                       color = "black", face = "bold", hjust = 1.1))
  # 
}#end TP_corrplot



###########
create_median_plot_data<-function(listPMP,
                                  pop,
                                  n,
                                  hyp){ 
  aggrPMP<-listPMP$PMP[,,pop,,n]
  
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
  all.colors <-colorBlindness::availableColors()
  all.colors<-all.colors[c(1,2,4,3,5:8)]
  
  df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
    left_join(., ubs, by=c("t", "Hypothesis")) %>% 
    as.data.frame()
  df$color<-all.colors[1:length(hyp)]
  df$name<-hyp
  
  
  list(plot_data=df,
       pop=pop,
       n=n,
       r2=listPMP$r2,
       pcor=listPMP$pcor,
       hypothesis=listPMP$hypothesis,
       hypothesis_test=listPMP$hypothesis_test,
       iter=listPMP$iter,
       studies=listPMP$studies
  )
  
  #return(df)
}
#colors were selected from a color blind palette
#colorBlindness::availableColors() %>%  colorBlindness::displayColors()

#################################### TEMP
# hyp_input<-c("H2.V1>V2>V3" ,  "H2.complement", "Hu"  )
# pops = c("r0.13_pcor0.3_b321_p0") #,"r0.13_pcor0.3_bmixed_p0"
# studies=10
# data<-dat3 %>% 
#   aggregatePMP(hyp=hyp_input,
#                studies=15) %>% 
#   create_median_plot_data(pop="r0.13_pcor0.3_bmixed_p0",
#                           n="300")
# #################################### TEMP
# plot_data$color<-c(  "#009E73" , "#E69F00","#000000"  )
# plot_data$name<-hyp_input
####################
median_plot<-function(data, hyp_input){
  
  
  plot_data<-data$plot_data
  
  plot_data%>%
    hchart("scatter",
           hcaes(x=t, y=median_aggrPMP,
                 group=factor(Hypothesis,levels = unique(Hypothesis))
           ),
           color=plot_data$color[1:length(hyp_input)],
           name=plot_data$name[1:length(hyp_input)],
           #pointPlacement=c(-0.15,0.15),
           id=letters[1:length(unique(plot_data$Hypothesis))] #letters[1:length(unique(plot_data$Hypothesis))]
    )%>%
    hc_tooltip(enabled=TRUE,
               valueDecimals=2)%>%
    hc_yAxis(labels=list(enabled=TRUE),
             #reversed=TRUE,
             title=list(text="Aggregated PMPs"),
             gridLineWidth=0)%>%
    hc_xAxis(labels=list(style=list(color="black", fontSize="12px")),
             title=list(text="Number of aggregated studies")
             # opposite=TRUE
    )%>%
    hc_legend(enabled=TRUE,
              verticalAlign = "bottom",
              align="left",
              title=list(text="Hypotheses"
              )) %>% 
    hc_title(text="Variation of aggregate PMPs for each hypothesis across iterations",
             align="left") %>% 
    hc_subtitle(text=data$hypothesis_test,#paste(paste0("H",hyp_input), collapse = " vs. ")
                align="left") %>% 
    hc_add_series(
      plot_data,
      "errorbar", 
      hcaes(y = median_aggrPMP, x = t, low = lb_aggrPMP, high = ub_aggrPMP,
            group = factor(Hypothesis,levels = unique(Hypothesis))
      ),
      color=plot_data$color[1:length(hyp_input)],
      linkedTo =letters[1:length(unique(plot_data$Hypothesis))],
      enableMouseTracking = TRUE,
      showInLegend = FALSE,
      grouping=TRUE,
      groupPadding=0.3
    ) 
  
  
}

# END App functions ---------------------------------------------

# Modules --------------------------------------------------


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

# END Modules -----------------------------------------------


#bs<-attributes(dat3)$ratio_beta

custom_minty<- bs_theme(
    version = 5,
    bootswatch = "minty"
  )

#bs_theme_preview(theme = custom_minty, with_themer = FALSE)

ui<-navbarPage(title = "Bayesian Evidence Synthesis",
               theme = custom_minty,
               selected = "Home",
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
                    
                  
                    }
                                "))),
               tags$div(HTML("<script type='text/x-mathjax-config' >
                              MathJax.Hub.Config({
                              tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                              });
                              </script >
                              ")),
               tabPanel("Home",
                        
                        h3(style="margin-left:80px; margin-top:100px;color:#78C1A9;width:50%;",
                           "Bayesian Evidence Synthesis:",tags$br(),"The Value of The Unconstrained Hypothesis"),
                        
                        div(style="width:40%;margin-top:30px; margin-left:80px;",
                            tags$p("This Shiny App provides additional simulation conditions to those 
                               presented in Lyubenova ((2023)). Bayesian Evidence Synthesis: The 
                               Value of the Unconstrained Hypothesis. Two different types of plots
                               can be obtained in the tabs \"PMP distribution plots\" and \"Accuracy plots\".
                               In each two different simulation conditions can be compared.")
                            )
                        
                        ),
               tabPanel("PMP distribution plots",
                        withMathJax(),
                        fluidRow(
                          column(width = 12,
                                 fluidRow(
                                   column(width = 6,
                                                 hyp_UI("hyp_UI1_med"),
                                                 uiOutput("pop1_med"),
                                                 med_plot_UI("plot_UI1_med"),
                                                verbatimTextOutput("test_outer")
                                   ),
                                   column(width = 6,
                                          hyp_UI("hyp_UI2_med"),
                                          uiOutput("pop2_med"),
                                          med_plot_UI("plot_UI2_med")
                                          #   verbatimTextOutput("test")
                                          )
                                   )
                          )
                        )
                        ),
               tabPanel("Accuracy plots",
                        
                        withMathJax(),
                        fluidRow(
                        column(width = 12,
                               fluidRow(column(width = 6,
                                               hyp_UI("hyp_UI1_acc"),
                                               uiOutput("pops1_acc"),
                                               plots_UI("plots_UI1_acc")
                               ),
                               column(width = 6,
                                      hyp_UI("hyp_UI2_acc"),
                                      uiOutput("pops2_acc"),
                                      plots_UI("plots_UI2_acc")
                                      )
                               )
                               )
                        # ,
                        # column(width = 3,
                        #        div(style="height:90vh; display:flex;font-size:14px;",
                        #            wellPanel(verbatimTextOutput("test_outer"))
                        #           )
                        #       )
                            )
                        )
               
               )

server<-function(input, output,session){
# Tab PMPs: Panel 1 --------------------------------
  hyp_UI1_med_selection<- hyp_server("hyp_UI1_med")

  observeEvent(hyp_UI1_med_selection$hyp_input_r(), {
    output$pop1_med<-renderUI({
      pop_UI("pop_UI1_med",
             #n_par = hyp_UI1_med_selection$n_par,
             hyp_input=NULL
             )
    })
  })
  
  #Create a placeholder for the population specifications
  #unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
  pop_def1_med<-reactive(pop_server("pop_UI1_med"))

  med_plot_server("plot_UI1_med",
               hyp=hyp_UI1_med_selection$hyp_input_r,
               pop_def = pop_def1_med
             #  n_par = hyp_UI1_med_selection$n_par
             )

  
  # Tab PMPs: Panel 2 --------------------------------
  hyp_UI2_med_selection<- hyp_server("hyp_UI2_med")

  observeEvent(hyp_UI2_med_selection$hyp_input_r(), {
    output$pop2_med<-renderUI({
      pop_UI("pop_UI2_med",
           #  n_par = hyp_UI2_med_selection$n_par,
             hyp_input=NULL
      )
    })
  })

  #Create a placeholder for the population specifications
  #unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
  pop_def2_med<-reactive(pop_server("pop_UI2_med"))

  med_plot_server("plot_UI2_med",
                  hyp=hyp_UI2_med_selection$hyp_input_r,
                  pop_def = pop_def2_med
                 # n_par = hyp_UI2_med_selection$n_par
                  )

  
  # output$test_outer<-renderPrint(hyp_UI1_med_selection$n_par())

# Tab Accuracy: Panel 1 --------------------------------
 hyp_UI1_acc_selection<- hyp_server("hyp_UI1_acc")
 
 observeEvent(hyp_UI1_acc_selection$hyp_input_r(), {
   
   output$pops1_acc<-renderUI({

     if(is.null(hyp_UI1_acc_selection$hyp_input_r())){
       return(NULL)

     }else{
     do.call(what = "tagList",
             args=lapply(hyp_UI1_acc_selection$hyp_input_r(), function(i){
               pop_UI(paste0("pop_UI1_acc",i),
                     # n_par = hyp_UI1_acc_selection$n_par,
                      hyp_input=i)
             })
       )
     }
    })
 })
 
 #Create a placeholder for the population specifications 
 #unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
 pop_def1<-reactiveValues()
   #save the population specificatins
   observe({
     for(x in hyp_UI1_acc_selection$hyp_input_r()){
       #pop_def1[[x]] will be a list with 3 elements, r2, pcor and p
       pop_def1[[x]]<-pop_server(paste0("pop_UI1_acc",x))
     }
  })
  
 
plots_server("plots_UI1_acc",
             hyp=hyp_UI1_acc_selection$hyp_input_r,
             pop_def = pop_def1
           #  n_par = hyp_UI1_acc_selection$n_par
           )
 

# Tab Accuracy: Panel 2 --------------------------------
hyp_UI2_acc_selection<- hyp_server("hyp_UI2_acc")
observeEvent(hyp_UI2_acc_selection$hyp_input_r(), {
  
  output$pops2_acc<-renderUI({
    
    if(is.null(hyp_UI2_acc_selection$hyp_input)){
      return(NULL)
      
    }else{
      do.call(what = "tagList",
              args=lapply(hyp_UI2_acc_selection$hyp_input_r(), function(i){
                pop_UI(paste0("pop_UI2_acc",i),
                      # n_par = hyp_UI2_acc_selection$n_par,
                       hyp_input=i)
              })
      )
    }
  })
})

#Create a placeholder for the population specifications 
#unfortunately, every hyp checked even once creates a named element in the pop_def1 reactive values
pop_def2<-reactiveValues()
#save the population specificatins
observe({
  for(x in hyp_UI2_acc_selection$hyp_input_r()){
    #pop_def2[[x]] will be a list with 3 elements, r2, pcor and p
    pop_def2[[x]]<-pop_server(paste0("pop_UI2_acc",x))
  }
})


plots_server("plots_UI2_acc",
             hyp=hyp_UI2_acc_selection$hyp_input_r,
             pop_def = pop_def2
            # n_par = hyp_UI2_acc_selection$n_par
             )



}
  


shinyApp(ui = ui, server = server)
#run_with_themer(shinyApp(ui = ui, server = server))

