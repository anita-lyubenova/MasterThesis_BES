#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)
# [compl_ inluded additional populations with heterogeneity due to sampled betas with varying SDs]
# after merging them into BFdat

load("Outputs/accuracy/dat_merged.RData")
source("Scripts/accuracy/accuracy_functions.R")
library(magrittr)

#Diff---------------------------------------------------
# Performance measure: Difference in accuracy

#difference in accuracy
H1u_H1_HETEROGp1.5<- aggregatePMP(x=dat,
                                  h=c("H1","Hu"),
                                  studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1",Hu="TRUE_Hc", Hu="HETEROG_H1p1.5")) 


H1cu_H1_HETEROGp1.5<-aggregatePMP(x=dat,
                                  h=c("H1" ,"Hc","Hu"),
                                  studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1",Hc="TRUE_Hc", Hu="HETEROG_H1p1.5")) 


loss<-H1cu_H1_HETEROGp1.5$acc-H1u_H1_HETEROGp1.5$acc 


large.acc<-H1u_H1_HETEROGp1.5$acc >.87 &H1cu_H1_HETEROGp1.5$acc >.87
large.acc[large.acc==TRUE]<-"green"
large.acc[large.acc==FALSE]<-"red"
    
large.acc<-reshape2::melt(large.acc, na.rm = TRUE)
lab.col<-loss                                 
lab.col[H1u_H1_HETEROGp1.5$acc >.87 &H1cu_H1_HETEROGp1.5$acc >.87]<-"green"
lab.col[H1u_H1_HETEROGp1.5$acc >.87 &H1cu_H1_HETEROGp1.5$acc <.87]<-"red"
lab.col[H1u_H1_HETEROGp1.5$acc <.87 &H1cu_H1_HETEROGp1.5$acc <.87]<-"black"  
lab.col<-reshape2::melt(lab.col, na.rm = TRUE)
      



loss_corrplot/H1u_H1_HETEROGp1.5_corrplot/H1cu_H1_HETEROGp1.5_corrplot

colors = c("blue", "white", "red")
tl.cex = 12

corr <- reshape2::melt(loss, na.rm = TRUE)
colnames(corr) <- c("Var1", "Var2", "value")

if (!is.null(p.mat)) {
  p.mat <- reshape2::melt(p.mat, na.rm = TRUE)
  corr$coef <- corr$value
  corr$pvalue <- p.mat$value
  corr$signif <- as.numeric(p.mat$value <= sig.level)
  p.mat <- subset(p.mat, p.mat$value > sig.level)
  if (insig == "blank") {
    corr$value <- corr$value * corr$signif
  }
}
corr$abs_corr <- abs(corr$value) * 10
p <- ggplot2::ggplot(data = corr, mapping = ggplot2::aes_string(x = "Var1", 
                                                                y = "Var2", fill = "value"))
p <- p + ggplot2::geom_tile(color = "white")
p <- p + ggplot2::scale_fill_gradient2(low = colors[1], 
                                       high = colors[3], mid = colors[2], midpoint = 0, limit = c(-1, 
                                                                                                  1), space = "Lab", name = "Labname")
label <- round(x = corr[, "value"], digits = 2)

p <- p + ggplot2::geom_text(mapping = ggplot2::aes_string(x = "Var1", 
                                                            y = "Var2"
                                                         ), label = label, color=lab.col$value, size = 4)
p

corr <- reshape2::melt(loss, na.rm = TRUE)
label <- round(corr$value,2)
lab.col<-loss                                 
lab.col[H1u_H1_HETEROGp1.5$acc >.87 &H1cu_H1_HETEROGp1.5$acc >.87]<-"green"
  lab.col[H1u_H1_HETEROGp1.5$acc >.87 &H1cu_H1_HETEROGp1.5$acc <.87]<-"red"
    lab.col[H1u_H1_HETEROGp1.5$acc <.87 &H1cu_H1_HETEROGp1.5$acc <.87]<-"black"  
      lab.col<-reshape2::melt(lab.col, na.rm = TRUE)
      
      
loss_corrplot<-loss %>% 
  ggcorrplot(outline.col = "white",
             lab = TRUE)

loss_corrplot$layers[[2]]<-NULL
loss_corrplot+
  geom_text(#mapping = aes_string(x = "Var1",
            #                     y = "Var2"),
            label = label,
            color=lab.col$value,
            size = 4)


# Accuracy of individual studies---------------------------------------

a<-aggregatePMP(x=dat,
                h=c("H1" ,"Hu"),
                studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_H0")) %>% 
  acc_corrplot()
a
b<-aggregatePMP(x=dat,
                h=c("H1" ,"Hu"),
                studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.75")) %>% 
  acc_lineplot()
a/b
c<-aggregatePMP(x=dat,
                h=c("H1" ,"Hu"),
                studies = 30) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_Hc")) %>% 
  acc_lineplot()
c


a<-aggregatePMP(dat,
             hyp=c("H1","Hc","Hu"),
             studies=20
) %>%
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hc="TRUE_Hc", Hu="HETEROG_H1p.75"))
