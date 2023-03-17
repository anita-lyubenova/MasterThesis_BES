#analyze simulated data generated in Scripts/power an alpha.R (power_BES.RData)
# and its complementing simulation created in Scripts/parallelization.R (compl_power_BES)

#the data files have a similar structure

# *only in compl_power_BES
power_BES[[1]]$
compl_power_BES$n50$
names(power_BES[[1]])
names(compl_power_BES[[1]])
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


#combine power_BES and compl_power_bes


#subset only n that is present in compl_
c1<-power_BES[1:5]
#remove BF0u
c1<-lapply(c1, function(x) x$BF[,-1,,])

c2<-compl_power_BES

names(c1)<-names(c2)

map_abind_3_BF<-function(x,y){
  yBF<-y$BF
  co<-abind(x, yBF, along = 3)
  return(co)
}

test<-Map(map_abind_3_BF,c1,c2)
#check
test$n50 %>% dimnames()


