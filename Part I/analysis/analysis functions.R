library(ggcorrplot)
#library(patchwork)
library(tidyverse)

#a function to compute aggregate PMPs for selected hypotheses from the BFs
aggregatePMP<-function(x, # a 5 dim array with structure [t, BF, pop, iter, n]
                       hyp=c("H1", "Hu"),
                       studies=10 #number of studies to aggregate over, max 40
){
  hyp_index<-substr(hyp,2,2)
  search_terms <- paste0(hyp, collapse = "|")
  BF<-x
  BF<-BF[1:studies,str_subset(dimnames(BF)[[2]], search_terms),,,,drop=FALSE]
  
  nom<-aperm(BF, perm=c(1,3,4,5,2))
  denom<-rowSums(nom, dims = 4)
  PMP_perm<-nom/replicate(length(hyp),denom)
  
  #placeholder for tha aggregated PMPs
  PMP_t<-PMP_perm
  for(t in 2:studies){ #the PMPs of the first study remain the same, thus iterate from t=2
    nom_t<-PMP_t[t-1,,,,]*nom[t,,,,]#multiply the previous PMPs with the current BFs
    denom_t<-rowSums(nom_t, dims=3)
    PMP_t[t,,,,]<-nom_t/replicate(length(hyp), denom_t)
  }
  
  PMP_t<-aperm(PMP_t, perm = c(1,5,2,3,4))
  dimnames(PMP_t)[[2]]<-paste0("PMP", c(hyp_index))
  
  PMP_t<-  rlist::list.append(attributes(x), PMP=PMP_t, hypothesis_test = paste(hyp, collapse = " vs. ")) 
  return(PMP_t)
}
  
create_median_plot_data<-function(listPMP,
                                  pop,
                                  n){ 
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
  
  df<-left_join(medians, lbs, by=c("t", "Hypothesis")) %>%
    left_join(., ubs, by=c("t", "Hypothesis")) %>% 
    as.data.frame() %>% 
    mutate(color=case_when(Hypothesis=="PMP1" ~ "#7fc97f",
                           Hypothesis=="PMPc" ~ "#fdc086",
                           Hypothesis=="PMPu" ~ "black",
                           Hypothesis=="PMP0" ~ "#aa69b5",#6B2B74
    ),
    name=case_when(Hypothesis=="PMP1" ~ "H1",
                   Hypothesis=="PMPc" ~ "Hc",
                   Hypothesis=="PMPu" ~ "Hu",
                   Hypothesis=="PMP0" ~ "H0",
    )
    )
  
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



median_plot<-function(x # a list created with create_median_plot_data()
                      
){
  
  pd <- position_dodge(width = 0.4) # set desired dodge width
  col.H1<-"#7fc97f"
  col.Hc<-"#fdc086"
  col.Hu<-"black"
  shades::lightness(col.H1, shades::scalefac(0.50))
  all.colors <-c(H1="#7fc97f", Hc="#fdc086",Hu="black")
  #subset the names of the tested hypoteheses
  hyp<-unlist(str_extract_all(x$hypothesis_test, "H(1|c|u)"))
  sub.colors<-all.colors[names(all.colors) %in% hyp]
  
  p<-x$plot_data
  
  p$Hypothesis<-factor(p$Hypothesis, levels = unique(p$Hypothesis))
  
  pplot<-p %>% 
    ggplot(aes(x=t, y=median_aggrPMP, group=Hypothesis, color=Hypothesis))+
    geom_point(position = pd)+
    geom_line(position = pd)+
    geom_errorbar(aes(ymin = lb_aggrPMP, ymax = ub_aggrPMP),position = pd)+
    theme_minimal()+
    labs(#title = "Aggregate PMPs for each hypothesis with increasing number of studies",
      title=x$pop,
      # title=paste0("MPCTH: ",str_extract(x$pop, "H."),str_split(x$pop, ':', simplify = TRUE)[,2]),
      # subtitle = paste0(x$hypothesis_test, " (MPCTH: ",str_extract(a$pop, "H."),str_split(a$pop, ':', simplify = TRUE)[,2], ")"),
      subtitle = paste0(x$hypothesis_test, " (",x$n, ")"),
      x="Number of aggregated studies",
      #x=" ",
      y="Aggregate PMP")+
    theme(text = element_text(size = 9),
          plot.title = element_text(face="bold", size=11),
          #  axis.text.x = element_text(size = 8, colour = rep(c(lightness(col.H1, scalefac(0.70)),lightness(col.Hc, scalefac(0.70))), times=20)),
          legend.title = element_text(size = 8),
          #plot.subtitle = element_text(hjust = 0.5), #center aligned subtitle
          #  plot.margin = unit(c(0.5,1.1,0,0.2), "cm"),
          legend.position="bottom"
          # legend.text = element_text(size = 10),
    )+
    scale_colour_manual(values = unname(sub.colors),labels=names(sub.colors),name = "Hypothesis")
  
  return(pplot)
}



#a function to compute confusion matrix from aggregated PMPs
accuracyPMP<-function(listPMP, #list created with aggregatePMP()
                      hyp_to_pop # a named character vector; elements are subset of dimnames(PMP)[[3]]: "TRUE_H0", "TRUE_H1", "TRUE_Hc", "TRUE_Hu", "HETEROG_H1p.1", "HETEROG_H1p.3", "HETEROG_H1p.5", names are the hypotheses for which the populations are true
                      
){
  #subset the populations of interest
  PMP<-listPMP$PMP[,,hyp_to_pop,,]
  dim(PMP)
  hyp_index<-substr(names(hyp_to_pop),2,2)
  
  tr<-data.frame(hyp=names(hyp_to_pop),
                 PMP=paste0("PMP", hyp_index),
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
  
  
  true<-lapply( mget(correct_name), function(x) x/listPMP$iter)
  
  acc<-eval(parse(text = paste(correct_name, collapse = "+")))/(nrow(tr)*listPMP$iter)
  
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


acc_corrplot<-function(a, # a list created with accuracyPMP()
                       object="acc" # what should be plotted? acc or TP?
){
  
  ggcorrplot(as.data.frame(a[object]),   #a$acc,
             outline.col = "black",
             lab = TRUE)+
    scale_fill_gradient2(limit = c(0,1),
                         breaks=seq(0,1,0.1),
                         low = "blue", high =  "red",
                         mid = "white",
                         midpoint = 0.87)+
    scale_x_continuous(breaks = 1:a$studies)+
    #  scale_y_discrete(labels= substr(colnames(a$acc), 2, nchar(colnames(a$acc))))+
    labs(title = a$hypothesis_test,
         subtitle =paste("Populations:", paste(a$hyp_to_pop, collapse = ", "), collapse = " "),
         fill="Accuracy"
    )
}


##a custom corrlot with a lot of scaling 
acc_corrplot<-function(a, # a list created with accuracyPMP()
                       object="acc", # what should be plotted? acc or TP?
                       b=NULL #optional: another list created with accuracyPMP to plot differences in acc
){
  #create the data to plot
  if(is.null(b)){
    
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
      mutate(color=case_when(value<=.65 ~ "white",
                             value>=.56~ "black"
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
    for(i in 1:ncol(l)){
      ld[i,"x"]<- suppressWarnings(min(which(l[,i]>=0.865)))
    }
    ld[ld$x=="Inf",]<-c(nrow(l)-0.3,
                        ld[ld$x=="Inf",]$y+1
                                    )
    
  }else{
    #compute differences in accuracies & reshape
  }

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
  geom_point(data = ld[unique(ld$y),], mapping = aes(x=x, y=y), inherit.aes = FALSE)+
  geom_step(data = ld, mapping = aes(x=x, y=y), inherit.aes = FALSE)+
  geom_text(mapping = aes(x=t, y=n),
            label = label.df,
            color= names(label.df),  #"white",
            size = 3)+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.key.width=unit(3,"cm"))
}


# ##a custom corrlot without scaling
# acc_corrplot3<-function(a, # a list created with accuracyPMP()
#                         object="acc", # what should be plotted? acc or TP?
#                         b=NULL #optional: another list created with accuracyPMP to plot differences in acc
# ){
#   #create the data to plot
#   if(is.null(b)){
#     
#     x<-
#       a[[object]] %>% 
#       reshape2::melt() %>% 
#       rename(n=Var2,
#              t=Var1) %>%
#       mutate(cat=cut(value, breaks=c(0,0.4,0.5,0.7,0.8,0.9,1)),
#              n=factor(n),
#              t=factor(t)
#       )
#     
#     q<-a[[object]]
#     q[-c(1, nrow(q)),]<-NA
#     label<-reshape2::melt(q)%>% 
#       pull(value) %>% 
#       round(.,digits=2) %>% 
#       # as.character() %>% 
#       format(nsmall=2) %>% 
#       gsub("^0", "", .) %>% 
#       gsub("NA", NA, .)
#     
#   }else{
#     #compute differences in accuracies & reshape
#   }
#   
#   ggplot(data = x, mapping = aes(x=t, y=n, fill=value))+
#     geom_tile()+#color = "white"
#     scale_fill_gradientn(colours = c("#481568FF","#FDE725FF","#1F968BFF"), #  c("#481568FF","#AC82C9","#FDE725FF","#1F968BFF")
#                          limit = c(0, 1),
#                          breaks=c(0,0.10,0.20, 0.30, 0.4,0.50,0.60,0.70,0.80, 0.87, 0.95, 1),
#                          space = "Lab",
#                          name = "Accuracy",
#                          values = scales::rescale(c(0,0.87,1))
#     )+
#     geom_text(mapping = aes(x=t, y=n),
#               label = label,
#               color= "black",  #"white",
#               size = 3)+
#     # geom_point(data = linedat[-nrow(linedat),], mapping = aes(x=x, y=y), inherit.aes = FALSE)+
#     # geom_step(data = linedat, mapping = aes(x=x, y=y), inherit.aes = FALSE)+
#     theme_minimal()+
#     theme(legend.position="bottom",
#           legend.key.width=unit(3,"cm"))
# }
# 
# 
# acc_lineplot<-function(x){
#   accdat<-x$acc %>%
#     as.data.frame() %>% 
#     rownames_to_column("t") %>% 
#     reshape2::melt() %>% 
#     rename(n=variable,
#            acc=value)
#   accdat %>% 
#     ggplot(aes(x=factor(t, levels=unique(t)), y=acc, group=n, color=n)) +
#     geom_point()+
#     geom_line()+
#     theme_minimal()+
#     labs(title = x$hypothesis_test,
#          subtitle = paste0("Populations:",paste(x$hyp_to_pop, collapse = ",")),
#          x="studies")+
#     geom_hline(yintercept = 0.87, color="red", linetype="dashed")+
#     scale_y_continuous(breaks = seq(0,1,0.1))+
#     scale_color_manual(values=rev(viridis::viridis(6)))
#   
# }
