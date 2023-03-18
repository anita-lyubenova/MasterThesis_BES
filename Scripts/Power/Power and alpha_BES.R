# Load packages -------------------------------------------------------------------
source("scripts/load_packages.R")
library(plotly)
#source("scripts/ThomVolker scripts/functions.R")

# r2=0.13
# pcor=0.3
# hypothesis="V1=V2=V3; V1>V2>V3"
# ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
#                 H1=c(3,2,1), #population H1 = TRUE
#                 Hc=c(1,2,3),
#                 Hu=c(1,1,1)
# )
# model="linear"
# iter=3
# studies=40



# # a function that 
# # (1) simulates data based on specified populations in line with hypohteses of interest
# # (2) Computes BFs for each hypohteses of interest in each population
# sim_BES<-function(
#     r2=0.13,#effect size r-squared
#     pcor=0.3,#correlation between the predictor variables
#     n, #sample size
#     hypothesis, #tested hypotheses;
#     ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
#                    H1=c(3,2,1), #population H1 = TRUE
#                    Hc=c(1,2,3),
#                    Hu= c(1,1,1)
#                    ), # definition of the populations, as determined by the ratio between the regression coefficients b1:b2:b3; should be a named list of numeric vectors, where each vector corresponds to the ratio of betas in hte respective hypothesis (note the order of hypotheses!); the names should only contain the subscript of the hypothesis (e.g "1" or "i" or"0")
#     model="linear", #linear, logistic or probit regression
#     iter=3,
#     #BES arguments
#     studies=40,
#     ratio_HiHc=4 # every 2nd study comes from a different population
# ){
#   
# 
#   # a list to store the population level coefficients for each hypothesis-population
#   betas<-list()
#   #a placeholder for the BFs
#   BF.u<-array(NA,
#               dim = c(studies,
#                       length(ratio_beta),
#                       length(ratio_beta),
#                       iter),
#               dimnames = list(1:studies,
#                               paste0("BF", substr(names(ratio_beta),2,2), "u"),
#                               paste0("TRUE_", names(ratio_beta)),
#                               1:iter
#                               ))
#   
#   for(t in 1:studies){
#     
#     #for each population deifned by ratio_beta
#     for(b in 1:length(ratio_beta)){
#       betas[[b]]<-coefs(r2, ratio_beta[[b]], cormat(pcor, length(ratio_beta[[b]])), "normal")
#     
#       
#       if(b==4){
#         #have the betas conform with Hc or with Hi
#         #e.g. Hi:Hc study ratio could be 1:1 (where ratio_HiHc = 2; i.e. every 2nd study comes from Hc)
#         #or 1:3, where ratio_HiHc = 4, i.e every fourth study comes from Hc)
#         if(t %% ratio_HiHc == 0){ #2nd or 4th study => Hc
#           betas[[4]]<-coefs(r2, ratio_beta[["Hc"]], cormat(pcor, length(ratio_beta[[4]])), "normal")
#         }else{                       #else Hi
#           betas[[4]]<-coefs(r2, ratio_beta[["H1"]], cormat(pcor, length(ratio_beta[[4]])), "normal")
#         }
#     }
#       
#       for(i in 1:iter){
#         print(paste0("Sample size: ", n ,"; Study: ",t ,"; Population: ", b ,"; Iteration: ", i))
#         #generate BFs
#         BF.u[t,c(1,2,3),b,i]<-gen_dat(r2=r2,
#                             betas=unlist(betas[[b]]),
#                             rho=cormat(pcor, length(ratio_beta[[b]])),
#                             n=n,
#                             "normal")%$%
#           lm(Y ~ V1 + V2 +V3) %>%
#           bain(hypothesis = hypothesis)%$%
#           fit %>%
#           extract(c(1:length(unlist(strsplit(hypothesis, ";"))),nrow(.)),"BF.u")#subset only BFiu for the specified hypothesis and the complement
#         
#     }
#    }
#   }
#   
#   BF.u[,"BFuu",,]<-1
#   return(list(BF=BF.u,
#               r2=r2,
#               pcor=pcor,
#               n=n,
#               hypothesis=hypothesis,
#               populations=ratio_beta,
#               model=model,
#               iter=iter,
#               studies=studies
#   ))
# }


#function to aggregate the PMPs for a set of specified hypotheses
#reteruns the aggregate PMPs and sim conditions in a list
aggregatePMP2<-function(x, #a list created with sim_BES()
                       hyp=c("H1", "Hc", "Hu"), #which hypothesis are to be tested interest
                      # iter=1000,
                       studies=NULL #number of studies to aggregate over
                       
){
  BF<-x$BF
  iter<-x$iter
  n.hyp<-length(hyp)
  hyp_index<-substr(hyp,2,2)
  n<-x$n
  
  #if none specified, all studies will be aggregated over - this may take longer time
  if(is.null(studies)){
    studies<-x$studies
  }
  #subset only tested hypotheses, eg Hi vs. Hc vs. Hu (exlude H0)
  BF.temp<-BF[,substr(dimnames(BF)[[2]],3,3) %in% hyp_index,,,drop=FALSE]  
  
  #placeholder for the aggregate PMPs
  aggrPMP<-BF.temp
  
    for(i in 1:iter){       # for each iteration t
      for(t in 1:studies){    # for each study t
        for(h in 1:n.hyp){  #for each hypothesis h
          for(r in 1:length(dimnames(BF.temp)[[3]])){ #for each population 
            #PMP.RQ2.1.4h[t,s,h,i]<-BF.RQ2.1[t,s,h,i]/sum(BF.RQ2.1[t,s,,i]
            aggrPMP[t,h,r,i]<-prod(BF.temp[1:t,h,r,i])/sum(apply(matrix(BF.temp[1:t,,r,i],nrow = t,ncol = n.hyp),2,prod))
            
          }
        } 
      }
    }
  
  dimnames(aggrPMP)[[2]]<-paste0("PMP_", substr(dimnames(BF.temp)[[2]],3,3))
  
  return(list(aggrPMP=aggrPMP,
              r2=x$r2,
              pcor=x$pcor,
              n=n,
              hypotheses=hyp,
              populations=x$populations,
              model=x$model,
              iter=iter,
              studies=studies
              ))
}


#a function to transform the BFs to PMPs, create a confusion matrix, and compute power and alpha
power_matrix_BES<-function(x, # a list with BFs created with sim_BES()
                           hyp=c(H0="H0",H1= "H1",Hc= "Hc"), # a named vector with elements indicating the tested hypotheses, and the names indicating the true populaiton for this hypothesis
                           n, #sample size of the studies. Either a number or a vector. 
                           t.max #number of studies. Either a number or a vector 
                           # hyp_names = dimnames(BF)[[2]], #a character vector indicating the names of the testerd hypotheses
                           # pop_names=substr(dimnames(BF)[[3]],6,7)
){

  hyp_index = substr(hyp,2,2)
  pop_names<-paste0("TRUE_", names(hyp))
  n<-x$n
  #t.max=max(t.range)
  

  #if no aggregation is needed you can see the 1000 across all 40 studies as independent => 40 000 iterations
  if(t.max==1){
    x$populations<-x$populations[-4] # remove the population Hu
    x$BF<-apply(x$BF[,paste0("BF",hyp_index,"u"),pop_names,], c(2,3), abind::abind)
    x$iter<-nrow(x$BF)
    x$studies<-1
    x$BF %>% dimnames()
    
    #compute PMPi = BFi/sum(BF of all hypotheses of interest)
    PMP<-x$BF
    
    for(i in 1:dim(x$BF)[[1]]){ #for each iteration
      for(j in 1:dim(x$BF)[[2]]){ #for each tested hypothesis
        for(z in 1:dim(x$BF)[[3]]){ # for each population
          
          PMP[i,j,z] <- x$BF[i,j,z]/sum(x$BF[i,,z])
          
        }  
      }
    }
    
    #add a silent 4th dimension with lenght 1, so that the shape of the array is the same as in the 
    #else{} condition, where the 4th dim is t
    dim(PMP)<- c(dim(PMP),1)
    
   #If more than 1 studies should be aggregated over => BES 
  }else{
    
    x$BF<-x$BF[,paste0("BF",hyp_index,"u"),pop_names,]
    
    PMP_BES_list<-aggregatePMP2(x, hyp = hyp, studies = t.max)
    
    #reorder the dimensions such that PMP_BES[iter, PMP_hyp, pop] for t aggregated studies only
    PMP<-aperm(PMP_BES_list$aggrPMP[1:t.max,,,,drop=FALSE],#subset only the row with t number of aggregated studies studies
               c(4,2,3,1)
    )
    #print(dimnames(PMP))

  }
  
  
  #create an array with the same dimensions as PMP but that will be indicate only the highest PMPs
  max.PMP<-PMP
  #produce power matrix
  for(i in 1:dim(PMP)[[1]]){ #for each iteration
    for(z in 1:dim(PMP)[[3]]){ # for each population
      for(j in 1:dim(PMP)[[4]]){
      
        #get the column index of the hypothesis with the highest PMPs
        max.index<-which(PMP[i,,z,j]==max(PMP[i,,z,j]))
        # replace the max PMPs with 1 and the remaining PMPs with 0 for iteration i and population z
        max.PMP[i,max.index,z,j]<-1
        max.PMP[i,-max.index,z,j]<-0
      }
    }  
  }
  
  conf_matrix<-apply(max.PMP, c(2,3,4), sum)/dim(max.PMP)[1]
  
  acc_data<-data.frame(n=n,
                       t=1:t.max,
                       acc=NA
                       )

  for(j in 1:dim(PMP)[[4]] ){
    acc_data$acc[j] <- na.omit(sum(diag(conf_matrix[,,j]))/sum(conf_matrix[,,j]))

  }
  
  return(list(#matrix=conf_matrix,
             # metrics=TPFP,
             # plot_data=plot_data,
              acc_data=acc_data,
              sim_conditions=x[c("r2", "pcor", "hypotheses","populations", "model","iter")]
  )
  )
  
}#  end power_matrix_BES()

#a function to plot the power and alpha of hypotheses across different n
power_plot<-function(x.n, # a list of lists with BFs created with sim_individual() across different n,
                     hyp,
                     n,
                     t.range=1
){

  all.n<-c(50,100,150,200,300,500,800,1200)
  n.ind<-which( all.n%in%n )
  
  if(length(t.range)==1){
    x.axis<-"n"
  }else if(length(t.range)>1){
    x.axis<-"t"
  }
  
#  plot_data<-data.frame()
  acc_data<-data.frame()
  for(s in n.ind){
        # plot_data<-rbind(plot_data,power_matrix_BES(x=x.n[[s]], hyp=hyp, t.range=t.range)$plot_data)
         acc_data<-rbind(acc_data,power_matrix_BES(x=x.n[[s]], hyp=hyp, t.max=max(t.range))$acc_data)
  }
  
  
  hyp_info<-paste("Hypothesis (Population):",
                     paste(paste0(hyp, "(",names(hyp), ")"), collapse = " vs. "),
                     collapse = " ")
  plot_caption<-paste(paste0("R2 = ", x.n[[1]]$r2),
                      paste0("pcor = ", x.n[[1]]$pcor),
                      paste0("iterations:", x.n[[1]]$iter),
                      collapse = ";")
  
  plot_title<- "Accuracy of BES (probability of the PMP of the true hypothesis to be the highest)"
  
  if(x.axis =="n"){
    plot_caption<-paste(paste("Aggregated studies:", max(t.range)),
                        plot_caption,
                        collapse = "; "
    )
  }
  
  
  plot_acc<-acc_data %>% 
    filter(case_when(x.axis=="n" ~ t == max(t),
                     x.axis=="t" ~ t %in% t.range
                     )) %>%  
    mutate(n=as.factor(n),
           t=as.factor(t)) %>% 
    ggplot(aes_string(x=x.axis, y="acc"))+
    {if(x.axis == "t" & length(n)>1) geom_point(aes(color=n)) }+
    {if(x.axis == "t" & length(n)>1)  geom_line(aes(color=n, group=n))}+
    {if(x.axis != "t") geom_point() }+
    {if(x.axis != "t")  geom_line(aes( group=1))}+
    labs(title="Accuracy of BES (probability of the PMP of the true hypothesis to be the highest)",
         x=x.axis,
         subtitle = hyp_info,
         caption = plot_caption
    )+
    # scale_x_continuous(breaks = t.range)+
    scale_y_continuous(breaks = seq(0,1, 0.1),
                       limits = c(0,1))+
    theme(axis.text.x = element_text(angle = 45))+
    theme_minimal()+
    geom_hline(yintercept = 1)+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = .87, color="gray", linetype="dashed")


  
  plot_acc <- ggplotly(plot_acc) %>% 
    layout(title = list(text = paste0(plot_title,
                                      '<br>',
                                      '<sup>',
                                      hyp_info,
                                      '</sup>')),
           annotations = list(text = plot_caption,
                              x=1, y=-0.08,
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=11))
           )

  return(list(#plot_TF=plot_TF,
              #plot_data=plot_data,
              plot_acc=plot_acc,
              acc_data=acc_data,
              sim_conditions=x.n[[1]][c("r2", "pcor", "hypotheses","populations", "model","iter")]
  ))
} # end power_plot



# Simulate --------------------------------------------------------------------------------
# # for a range of sample sizes
# n<-c(50,100,150,200,300,500,800,1200)
# 
# power_BES<-list()
# for (s in 1:length(n)){
#   power_BES[[s]]<-sim_BES(r2=0.13,#effect size r-squared
#                           pcor=0.3,#correlation between the predictor variables
#                           n=n[s], #sample size
#                           hypothesis = "V1=V2=V3; V1>V2>V3", #tested hypotheses;
#                           ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
#                                           H1=c(3,2,1), #population H1 = TRUE
#                                           Hc=c(1,2,3),
#                                           Hu= c(1,1,1) #the values do not matter - it will be either Hi or Hc
#                           ), # definition of the populations, as determined by the ratio between the regression coefficients b1:b2:b3; should be a named list of numeric vectors, where each vector corresponds to the ratio of betas in hte respective hypothesis (note the order of hypotheses!); the names should only contain the subscript of the hypothesis (e.g "1" or "i" or"0")
#                           model="linear", #linear, logistic or probit regression
#                           iter=1000,
#                           #BES arguments
#                           studies=40,
#                           ratio_HiHc=2 # every 2nd study comes from Hc, the others from Hi
#   )
# }

#Shape of the data
#list[[n]][t, BF, heterog, i]

#save(power_BES, file = "Outputs/power_BES.RData")

#Number of conditions:n=8 x ratio = 4, x studies=40 x iter=1000 = 1 280 000 BFs
8*4*40*1000

#Load power_BES  -----------------------
load("Outputs/power_BES.RData")


## BES --------------------------------------------
#choose hypotheses
# hyp=c(H0="Hu",H1= "H1", Hc="Hc")
# n<-c(50,100,150,200,300,500,800,1200)


power_plot(x.n=power_BES,
           hyp=c(H1="H1",Hu= "Hu"),
           n=c(50,100,150,200,300,500,800),
           t=1:5)$plot_acc



#H0 --------------------------------

power_plot(x.n=power_BES,
           hyp=c(H0="H0", H1="Hu"),
           n=c(300,500,800,1200),
           t=1:10)$plot_acc

power_plot(x.n=power_BES,
           hyp=c(H0="H0", H1="H1"),
           n=c(300,500,800,1200),
           t=1:10)$plot_acc

power_plot(x.n=power_BES,
           hyp=c(H0="H0",H1= "H1", Hu="Hu"),
           n=c(300,500,800,1200),
           t=1:10)$plot_acc

power_plot(x.n=power_BES,
           hyp=c(H0="H0",H1= "H1", Hc="Hu"),
           n=c(300,500,800,1200),
           t=1:10)$plot_acc


power_plot(x.n=power_BES,
           hyp=c(H1= "H1", Hc="Hc", Hu="Hu"),
           n=c(50,100,150,200,300,500,800),
           t=1:10)$plot_acc

#H1 -------------------------------
power_plot(x.n=power_BES,
           hyp=c(H1= "H1", Hu="Hu"),
           n=c(50,100,150,200,300,500,800),
           t=1:10)$plot_acc

power_plot(x.n=power_BES,
           hyp=c(H1= "H1", H0="Hu"),
           n=c(50,100,150,200,300,500,800),
           t=1:10)$plot_acc

# I don't understand...
power_plot(x.n=power_BES,
           hyp=c(Hc= "Hc", H0="Hu"),
           n=c(50,100,150,200,300,500,800),
           t=1:10)$plot_acc
