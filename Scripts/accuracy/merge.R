# Merging ------------------------------------------------------

library(tidyverse)
#load files
load("Outputs/accuracy/simulated files/power_BES.RData")
load("Outputs/accuracy/simulated files/compl_power_BES_processed.RData")
load("Outputs/accuracy/simulated files/compl_power_BES2_processed.RData")
load("Outputs/accuracy/simulated files/compl_power_BES3_processed.RData")
## Files structure ------------------------------------------------
#the data files power_BES and compl_power_BES have a similar structure

# *elemens present only in compl_power_BES
names(power_BES[[1]])
names(compl_power_BES[[1]])
#NOT code - just to provide the files stucture
# power_BES[[n]]list(BF [t, BF, population(H0,H1, Hc, Hu:H1+Hc), iter],
#                    ###unique to compl_ ###
#                    sampled_betas* [t, beta, heterogeneity (propSD), iter],
#                    est_betas*,
#                    est_SE*,
#                    betas*,
#                    propSD=c(0.1, 0.3, 0.5),
#                    
#                    ####common###
#                    r2=0.13,
#                    pcor=0.3,
#                    n,
#                    hypothesis="V1>V2>V3",
#                    populations
#                    model="linear",
#                    iter=1000,
#                    studies=40
#                    )

## Combine files ----------------------------------------------------------
#combine power_BES and compl_power_bes


c0<-
  power_BES[1:5] %>% #subset only n that is present in compl_
  lapply(function(x) x$BF[,-1,,])#remove BF0u

#keep only the array with the BFs
c1<-lapply(compl_power_BES, function(x) x$BF)
c2<-lapply(compl_power_BES2, function(x) x$BF)
c3<-lapply(compl_power_BES3, function(x) x$BF)

# Unlist n ------------------------------
library(abind)
for(j in 0:3){
  x<-eval(parse(text = paste0("c", j)))
  unl<-x[[1]]
  for(i in 2:5){
    unl<-abind(unl, x[[i]], along = 5)
  }
  #save the unlisted arrays in c0, c1, etc.
  assign(paste0("c", j),unl)
}


#merge the BF arrays of the old and new files along the 3rd dimension(population)
BFdat<-abind(c0,c1,c2,c3, along = 3)
#check
BFdat %>% dimnames()
dimnames(BFdat)[[5]]<-names(compl_power_BES)


dat<-list(BF=BFdat,
          r2=0.13,
          pcor=0.3,
          populations=list( # ratio beta
            TRUE_H0=c("b1:b2:b3 = 1:1:1"),
            TRUE_H1=c("b1:b2:b3 = 3:2:1"), #population H1 = TRUE
            TRUE_Hc=c("b1:b2:b3 = 1:2:3"),
            TRUE_Hu=paste(paste("50%", c("H1", "Hc")), collapse = " & "),
            HETEROG_H1p.1=c("b1:b2:b3 = 3:2:1 + +heterogeneity: SD_betas=0.1*betas"),
            HETEROG_H1p.3=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=0.3*betas"),
            HETEROG_H1p.5=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=0.5*betas"),
            HETEROG_H1p.75=c("b1:b2:b3 = 3:2:1 + +heterogeneity: SD_betas=0.75*betas"),
            HETEROG_H1p1=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=1*betas"),
            HETEROG_H1p1.25=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=1.25*betas"),
            HETEROG_H1p1.5=c("b1:b2:b3 = 3:2:1 + heterogeneity: SD_betas=1.5*betas"),
            TRUE_H1large=c("b1:b2:b3 = 9:3:1"),
            TRUE_H1small=c("b1:b2:b3 = 2:1.5:1")
          ),
          hypothesis="V1>V2>V3",
          model="linear",
          iter=1000,
          studies=40
)

save(dat, file = "Outputs/accuracy/dat_merged.RData")
