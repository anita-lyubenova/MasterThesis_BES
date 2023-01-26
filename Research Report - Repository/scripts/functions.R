library(tidyverse)
library(magrittr)
library(furrr)
library(BFpack)
library(Rcpp)
library(RcppArmadillo)
library(MASS)
library(mvtnorm)
library(bain)
library(knitr)

# library(ggplot2)
# library(ggstatsplot)
# library(plotly)
# library(readxl)
# library(writexl)
# library(lavaan)
# library(devtools)
# library(SSDbain)
# library(jtools)
# library(ggrepel)
#
cormat <- function(partial_cor, diag_length) {
  r <- diag(diag_length)    # fill correlation matrix with partial correlation
  r[r != 1] <- partial_cor  # between variables
  r
}

coefs <- function(r2, ratio, rho, model = c("normal", "logit", "probit")) {
  
  # variance of predicted values (Var(yhat))
  if (model == "normal") {
    var_y <- r2
  }
  else if (model == "logit") {
    var_y <- (r2 * pi^2 / 3) / (1 - r2)
  }
  else if (model == "probit") {
    var_y <- r2 / (1 - r2)
  }
  # value of the regression coefficients
  sqrt(var_y / sum(ratio %*% t(ratio) * rho)) * ratio
}

gen_dat <- function(r2, betas, rho, n, model = c("normal", "logit", "probit"), 
                    mutate_args = NULL, select_args = quos(everything())) {
  # generate predictors X
  X <- mvrnorm(n, mu = rep(0, length(betas)), Sigma = rho)
  
  # generate outcome variable Y
  if (model == "normal") {
    Y <- X %*% betas + rnorm(n = n, mean = 0, sd = sqrt(1 - r2))
  }
  if (model == "logit") {
    Y <- rbinom(n, 1, 1 / (1 + exp(-(X %*% betas))))
  }
  if (model == "probit") {
    Y <- rbinom(n, 1, pnorm(X %*% betas))
  }
  # output data
  bind_cols(X = as.data.frame(X), Y = Y) #%>%
    # mutate(!!!mutate_args) %>%
    # select(!!!select_args)
}

q_glm <- quietly(glm)

data_and_model <- function(r2, betas, rho, n, model, 
                           formula, 
                           hypothesis, complement = TRUE,
                           mutate_args = NULL, select_args = quos(everything())) {
  
  if (model == "normal") {
    
    # calculate bayes factors for normal data

    gen_dat(r2, betas, rho, n, model, mutate_args, select_args) %>%
      lm(formula = formula, data = .) %>%
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory
    
  } else {
      
    # for logit and probit data, first chck if there is no separation. If there
    # is complete separation of the outcome, then draw a new data set.
    warnings <- 0
      
    while(length(warnings) > 0) {
      mod <- gen_dat(r2, betas, rho, n, model, mutate_args, select_args) %>%
        q_glm(formula = formula, 
              family = binomial(link = model),
              data = .)
      warnings <- mod$warnings
    }
    
    # and calculate bayes factors
    mod$result %>% 
      BF(hypothesis = hypothesis, complement = complement) %$%
      BFtable_confirmatory
    
  }
}

#function to create a set of colors for the plots based on teh cateogires
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#function to perform sample size determination for specified power levels
#when testing Hi agaitst its complement

power_to_N<-function(power.lvls,
                     r2,
                     pcor,
                     ratio_beta,
                     k=k,
                     H1, #e.g. "beta1>beta2>beta3",
                     T_sim=10000
              
){
  
  betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
  
  power<-data.frame(power=power.lvls,
                       k=k,
                       pcor=pcor,
                       r2=r2,
                       d=betas[length(betas)],
                       b1=betas[1],
                       b2=betas[2],
                       q=NA,
                       n=NA)
  
  #obtain effect size q as the difference between the z-standardized semipartial correlations (empirically)
  m<-gen_dat(r2=r2, 
             betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
             rho=cormat(pcor, length(ratio_beta)),
             n=10000000,
             "normal")%$%
    lm(Y ~ V1 + V2) %>% 
    summ(part.corr=TRUE)
  
  
  part.cor1<-m$coeftable[2,5]
  part.cor2<-m$coeftable[3,5]
  
  z1<-log((1+part.cor1)/(1-part.cor1))
  z2<-log((1+part.cor2)/(1-part.cor2))
  
  q=z1-z2
  power$q<-q
  
  for(p in 1:length(power.lvls)){
    
    power[p,"n"] <-SSDRegression(Hyp1 = H1, Hyp2 = "Hc", k=k,
                                    rho = cormat(pcor, k),
                                    R_square1=r2,
                                    R_square2 = r2,
                                    T_sim = T_sim,
                                    BFthresh=1,
                                    eta=power.lvls[p],
                                    standardize = TRUE,
                                    ratio = ratio_beta
    )[[1]]
    
    
    
    
  }
  return(power)
  
  
}




run.sim<-function(pcor,
                  r2,
                  ratio_beta,
                  q,
                  hypothesis,
                  iter,
                  seed=123,
                  t=10,
                  planned.n, # a table with the sample sizes per condition, a sheet from SimPlanning excel file
                  manipulated # what was manipulated in the simulation (e.g. avg. power, spread, median-mean, etc.)
){
  #predefined options
  models <- c("normal")
  complement<-TRUE
  #caluclate beta coefficients
  betas<-coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal")
  
  k<-length(ratio_beta)
  d<-betas[1]-betas[2]
  
  #BFic loop-------
  row.names<-paste0("Iter.", seq(1:iter))
  column.names<-c(paste0("Study.", seq(1:t)), "log.aggr.BF", "aggr.PMP")
  slice.names<-paste0("Condition.", seq(1:nrow(planned.n)))
  
  cond.names<-paste0("Condition.", 1:nrow(planned.n))
    
  BFic<-array(NA, dim = c(iterations=iter, studies=length(column.names), conditions=nrow(planned.n)),
              dimnames = list(row.names,column.names, slice.names))
  
  
  #for each condition (manipulated sample size distribution in the set of studies) (slice m in the array)
  for(m in 1:nrow(planned.n)){
    
    n<-planned.n[m, 3:(3+t-1)] %>% as.numeric()
    
    #for each iteration (row i in the array)
    for(i in 1:iter) {
      
      #for each study; column s in the array
      for(s in 1:length(n)){
        
        seed=seed+1
        set.seed(seed)
        
        print(paste("Condition m:", m, ", Iteration i:", i, "Study s:", s))
        
        BF<-gen_dat(r2=r2, 
                    betas=coefs(r2, ratio_beta, cormat(pcor, length(ratio_beta)), "normal"),
                    rho=cormat(pcor, length(ratio_beta)),
                    n=n[s],
                    "normal")%$%
          lm(Y ~ V1 + V2) %>%
          BF(hypothesis = hypothesis, complement = complement) %$%
          BFtable_confirmatory %>% as.data.frame()%$% BF
        
        BFic[i,s,m]<-BF[1]/BF[2] 
        
        
      }# end iterations loop i
      
      #after all 10 studies in the set were simulated and evaluated in iteration i => calculate the log aggregate BF for iteration i
      BFic[i,11,m] <-sum(log(BFic[i,1:10,m]))
      #calculate the aggregate PMPs
      BFic[i,12,m] <- prod(BFic[i,1:10,m])/(prod(BFic[i,1:10,m]) + 1)
      
    }# end study loop s
    
  }#end conditions loop; THE END of BFic loop
  
  #BES-power (aggr.PMPs) ----------------------------------------------------

  vioplot.ic.df<-tryCatch(
    expr = {
      data.frame(BFic[,"aggr.PMP",1:nrow(planned.n)]) %>% # a df with nrow=iter, and ncol=number of conditions
        pivot_longer(cols = cond.names,
                     names_to = "condition",
                     values_to = "aggr.PMP") %>% #long format df wtih 2 columns: condition and aggr.PMP, where each condition is repeated iter number of times
        arrange(match(condition, cond.names)) %>% #sort the df by Condition
        mutate("{manipulated}":=rep(planned.n$Manipulated, each=iter)) #create a column that labels what was value of the manipulated aspect (e.g. power, sum of squares, median-mean, etc.) in each condition

    },
    function(e){
      message('Error in vioplot.ic.df')
      print(e)
    },
    warning = function(w){
      message('Warning in vioplot.ic.df')
      print(w)
    }
  )

  # condition as factor
  vioplot.ic.df$condition<-tryCatch(factor(vioplot.ic.df$condition, levels = unique(vioplot.ic.df$condition)) )

  #compute BESpower as the proportion of aggr. PMPs that were above a certain threshold: .75, .90, .95
  BESpower.ic<-
    tryCatch(
      expr={
        vioplot.ic.df %>%
          group_by_at(c("condition", manipulated)) %>%
          summarize(correct.75 = sum(aggr.PMP>.75)/iter,
                    correct.90 = sum(aggr.PMP>.90)/iter,
                    correct.95 = sum(aggr.PMP>.95)/iter
          )
      },
      function(e){
        message('Error in BESpower.ic')
        print(e)
      },
      warning = function(w){
        message('Warning in BESpower.ic')
        print(w)
      }
    )


  # Vioplots (aggr.PMPs) ----------------------------------------------------
  vioplot.ic<-
    tryCatch(
      expr={
        vioplot.ic.df %>%
          #boxplot with the PMPs per condition
          ggbetweenstats(x = condition,
                         y = aggr.PMP,
                         pairwise.comparisons = FALSE,
                         results.subtitle=FALSE,
                         type = "nonparametric",
                         plot.type = "boxviolin",
                         centrality.plotting=FALSE
          ) +
          labs(
            x = manipulated,
            y = "aggregate PMP",
            title = paste("Distribution of aggregate PMPs from", t ," studies when testing Hi:",hypothesis ," against Hc across", iter, "iterations
                  when Hi is true in the population across condions (", manipulated ,")"),
            subtitle = "Each point represents an aggregate PMP from 10 studies from one iteration",
            caption = paste("Population specifications: pcor:",pcor, ";r2 =", r2 , "; b1:b2 = ",ratio_beta[1],":",ratio_beta[2],"; d = b1 - b2 =", d, "q =",q ,"t =",t ,"; Hi:", hypothesis)

          )+
          # Customizations
          theme(
            # This is the new default font in the plot
            text = element_text( size = 10, color = "black"),
            axis.text.x = element_text(size=8)
          )+
          geom_hline(yintercept=c(0.75, 0.90, 0.95), linetype="dashed",
                     color = "red", size=0.8)+
          scale_x_discrete(labels=planned.n$Manipulated
          )+
          annotate("label",
                   x = seq(1:nrow(planned.n))+0.3,
                   y = rep(c(0.77), times=nrow(planned.n)),
                   label =paste("P(PMP>.75) =", BESpower.ic$correct.75),
                   size=2.7)+
          annotate("label",
                   x = seq(1:nrow(planned.n))+0.3,
                   y = rep(c(0.92), times=nrow(planned.n)),
                   label =paste("P(PMP>.90) =", BESpower.ic$correct.90),
                   size=2.7)+
          annotate("label",
                   x = seq(1:nrow(planned.n))+0.3,
                   y = rep(c(0.97), times=nrow(planned.n)),
                   label =paste("P(PMP>.95) =", BESpower.ic$correct.95),
                   size=2.7)
      },
      function(e){
        message('Error in vioplot.ic')
        print(e)
      },
      warning = function(w){
        message('Warning in vioplot.ic')
        print(w)
      }

    )




  #Plot: Manipulated aspect x BES power --------------------------------------------

  #how the power of individual studies varies with the power of BES
  BESpower.ic.long<-
    tryCatch(
      expr={
        BESpower.ic %>%
          pivot_longer(cols=c("correct.75", "correct.90", "correct.95"),
                       names_to = "stakes_level",
                       values_to = "BES_power"
          )

      },
      function(e){
        message('Error in BESpower.ic.long')
        print(e)
      },
      warning = function(w){
        message('Warning in BESpower.ic.long')
        print(w)
      }
    )

  BESpower.ic.long$stakes_level<-tryCatch(as.factor(BESpower.ic.long$stakes_level))
  levels(BESpower.ic.long$stakes_level)<-tryCatch(c("low (PMP>.75)", "high (PMP>.90)", "very high (PMP>.95)"))


  plot.BESpower.per.cond<-
    tryCatch(
      expr={
        BESpower.ic.long %>%
          ggplot(aes(x=!!(sym(manipulated)), y=BES_power, group=stakes_level, color=stakes_level))+
          geom_line()+
          geom_point()+
          geom_text_repel(aes(y=BES_power, label=round(BES_power,2), color=stakes_level),
                          size=4)+
          labs(title = paste("Variation of BES-power (y-axis) across conditions(", manipulated ,") (x-axis) and level of the stakes (separate lines)"),
               x=manipulated,
               y="BES-power",
               color="Stakes",
               caption =paste("Specifications: pcor:",pcor, ";r2 =", r2 , "; b1:b2 = ",ratio_beta[1],":",ratio_beta[2],"; d = b1 - b2 =", d, "q =",q ,"t =",t, "; Hi:", hypothesis)
          )+
          scale_y_continuous(breaks = seq(0.45,1, 0.05))+
          scale_x_continuous(breaks = BESpower.ic[[manipulated]])+
          theme_minimal()

      },
      function(e){
        message('Error in plot.BESpower.per.cond')
        print(e)
      },
      warning = function(w){
        message('Warning in plot.BESpower.per.cond')
        print(w)
      }
    )



  list(BFic = BFic, BESpower.ic = BESpower.ic, vioplot.ic = vioplot.ic, BESpower.ic.long = BESpower.ic.long,  plot.BESpower.per.cond=plot.BESpower.per.cond)
  #list(BFic = BFic)
} #end run.sim1() -------------------

