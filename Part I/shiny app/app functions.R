library(tidyverse)
# library(gridExtra)
library(ggpubr)
# 
# ############################################# TEMP
# BF<-dat3
# hyp<-c("H2.V1>V2>V3" ,  "H2.complement", "Hu"  )
# pops = c("r0.13_pcor0.3_b321_p0") #,"r0.13_pcor0.3_bmixed_p0"
# studies=10
# BF %>% dimnames()
# BF %>% dim
# nom_t %>% dimnames()
# nom_t %>% dim() %>% length()
# PMP_t %>% dim()
# ############################################# TEMP

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


#function to create median plot data
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
    left_join(., ubs, by=c("t", "Hypothesis")) %>% 
    as.data.frame() %>% 
    mutate(color=case_when(Hypothesis=="PMP_i" ~ "#7fc97f",
                           Hypothesis=="PMP_c" ~ "#fdc086",
                           Hypothesis=="PMP_u" ~ "black",
                           Hypothesis=="PMP_0" ~ "#aa69b5",#6B2B74
                           ),
           name=case_when(Hypothesis=="PMP_i" ~ "Hi",
                          Hypothesis=="PMP_c" ~ "Hc",
                          Hypothesis=="PMP_u" ~ "Hu",
                          Hypothesis=="PMP_0" ~ "H0",
           )
           )
  
  
  return(df)
}

median_plot<-function(data, hyp_input){
  
  plot_data<-create_plot_data(data)
  
  plot_data%>%
    hchart("scatter",
           hcaes(x=t, y=median_aggrPMP,
                 group=factor(Hypothesis,levels = dimnames(data)[[2]])
                 ),
           color=plot_data$color[1:length(hyp_input)],
           name=plot_data$name[1:length(hyp_input)],
           id=letters[1:length(dimnames(data)[[2]])]
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
    hc_subtitle(text=paste(paste0("H",hyp_input), collapse = " vs. "),
                align="left") %>% 
    hc_add_series(
      plot_data,
      "errorbar", 
      hcaes(y = median_aggrPMP, x = t, low = lb_aggrPMP, high = ub_aggrPMP,
            group = factor(Hypothesis, levels = dimnames(data)[[2]])
            ),
      color=plot_data$color[1:length(hyp_input)],
      linkedTo = letters[1:length(dimnames(data)[[2]])],
      enableMouseTracking = TRUE,
      showInLegend = FALSE,
      grouping=TRUE,
      groupPadding=0.3
    ) 
   

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
           pointPlacement=c(-0.15,0,0.15),
           id=c("a", "b", "c") #letters[1:length(unique(plot_data$Hypothesis))]
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
      linkedTo =c("a", "b", "c"), letters[1:length(unique(plot_data$Hypothesis))],
      enableMouseTracking = TRUE,
      showInLegend = FALSE,
      grouping=TRUE,
      groupPadding=0.3
    ) 
  
  
}
