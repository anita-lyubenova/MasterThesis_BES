#run functions.R first


#Sim1 --------------------------------------------
sim0.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim0")[1,]
sim1.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim1")

sim1.k2.p2.t10.exact<-run.sim(pcor = sim0$pcor,
                              r2 = sim0$r2,
                              ratio_beta = eval(parse(text=sim0$ratio_beta)),
                              q=sim0$q,
                              iter=10000,
                              seed=123,
                              hypothesis = "V1 > V2",
                              t=10,
                              planned.n = sim1.plan,
                              manipulated = "average power"
)

save(sim1.k2.p2.t10.exact, file="Outputs/sim1/sim1.k2.p2.t10.exact.RData")


# Sim2-----------------------------------

sim0.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim0")[1,]
sim2.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim2")


sim2.k2.p2.t10.exact<-run.sim(pcor = sim0.plan$pcor,
                              r2 = sim0.plan$r2,
                              ratio_beta = eval(parse(text=sim0.plan$ratio_beta)),
                              q=sim0.plan$q,
                              iter=10000,
                              seed=123,
                              hypothesis = "V1 > V2",
                              t=10,
                              planned.n = sim2.plan,
                              manipulated = "sum of squares"
)

save(sim2.k2.p2.t10.exact, file="Outputs/sim/sim2.k2.p2.t10.exact.RData")

# Sim 3 ----------------------

## Sim3.1 ---------------------
sim0.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim0")[1,]
sim3.1.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim3.1")[,1:14]
sim3.2.plan<-read_xlsx("Simulations planning/SimPlanning.k2.p2.t10.exact_09.10.xlsx", sheet="Sim3.2")




sim3.1.k2.p2.t10.exact<-run.sim(pcor = sim0.plan$pcor,
                              r2 = sim0.plan$r2,
                              ratio_beta = eval(parse(text=sim0.plan$ratio_beta)),
                              q=sim0.plan$q,
                              iter=10000,
                              seed=123,
                              hypothesis = "V1 > V2",
                              t=10,
                              planned.n = sim3.1.plan,
                              manipulated = "median-mean (decreasing median, fixed mean)"
)

save(sim3.1.k2.p2.t10.exact, file="Outputs/sim/sim3.1.k2.p2.t10.exact.RData")
sim3.1.k2.p2.t10.exact$plot.BESpower.per.cond
sim3.1.k2.p2.t10.exact$vioplot.ic


## Sim3.2 ---------------------
sim3.2.k2.p2.t10.exact<-run.sim(pcor = sim0.plan$pcor,
                                r2 = sim0.plan$r2,
                                ratio_beta = eval(parse(text=sim0.plan$ratio_beta)),
                                q=sim0.plan$q,
                                iter=10000,
                                seed=123,
                                hypothesis = "V1 > V2",
                                t=10,
                                planned.n = sim3.2.plan,
                                manipulated = "median-mean (fixed median, decreasing mean)"
)


save(sim3.2.k2.p2.t10.exact, file="Outputs/sim/sim3.2.k2.p2.t10.exact.RData")

sim3.2.k2.p2.t10.exact$plot.BESpower.per.cond
sim3.2.k2.p2.t10.exact$BESpower.ic



