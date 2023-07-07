# Simulation steps:
# 1) sample study_delta
# 2) generate data
#  2.1) compute Cohen's D
#  2.2) compute variance of Cohen's D
# 3) obtain BF from bain(d,hypotheses,n = N,Sigma = var)


# CHECKS ------------------------------------------------------------------------------------------

# Step 2) -----------------------------------

obtain_BF_d<-function(N,delta,tau,hypotheses="d>0"){
  # 1) sample study_delta
  study_delta<-rnorm(1,delta,tau)
  
  # 2) d<-generate_d(N, study_delta) 
  Y_C <- rnorm(n = N / 2, mean = 0, sd = 1)  # control group
  ybar_C <- mean(Y_C)
  sd_C <- sd(Y_C)
  
  Y_T <- rnorm(n = N / 2, mean = delta, sd = 1) #treatment group 
  ybar_T <- mean(Y_T)
  sd_T <- sd(Y_T)
  
  # 2.1) calculate Cohen's d
  sd_pool <- sqrt((sd_C^2 + sd_T^2) / 2)
  d <- (ybar_T - ybar_C) / sd_pool
  
  # 2.2) compute variance of Cohen's D
  var<- 4 / N + d^2 / (2 * (N - 2))
  
 data.frame(d=d,
            var=var) %>% return()

}


set.seed(123)
step2<-lapply(1:10000, function(i){obtain_BF_d(N=300, delta=0, tau=0.75)}) %>% rlist::list.rbind()

step2$d %>% hist()
#d is normally distributed

step2$var %>% hist()
#the variance is nicely skewed

delta=0.5
tau=0.9
#d=1.1
N=300
hypotheses="d>0"
# Step 3) ----------------------------------------
obtain_BF_full<-function(N,delta,tau,hypotheses="d>0"){
  # 1) sample study_delta
  study_delta<-rnorm(1,delta,tau)
  
  # 2) d<-generate_d(N, study_delta) 
  Y_C <- rnorm(n = N / 2, mean = 0, sd = 1)  # control group
  ybar_C <- mean(Y_C)
  sd_C <- sd(Y_C)
  
  Y_T <- rnorm(n = N / 2, mean = delta, sd = 1) #treatment group 
  ybar_T <- mean(Y_T)
  sd_T <- sd(Y_T)
  
  # 2.1) calculate Cohen's d
  sd_pool <- sqrt((sd_C^2 + sd_T^2) / 2)
  d <- (ybar_T - ybar_C) / sd_pool
  
  # 2.2) compute variance of Cohen's D
  var_d<- 4 / N + d^2 / (2 * (N - 2))
  
  # 3) obtain BF
  names(d) <- "d"
  
  bain_list <- bain(d,
                    hypotheses,
                    n = N,
                    Sigma = var_d)
  
  BF<-bain_list$fit %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    pull(var=BF.u, name = rowname)
  
    return(c(BF,d,var_d))
}


studies=30
iter=300
delta=0
tau=1
N=300

set.seed(123)
reps_aggrBF10<-lapply(1:iter, function(r) {
  
  BF<-lapply(1:studies, function(i){
    obtain_BF_full(N=N, delta=delta, tau=tau)
    }) %>%
    rlist::list.rbind() %>%
    as.data.frame() %>% 
    rename(var=V5)
  
  logBF<-BF %>% 
    dplyr::select(-Hu) %>% 
    mutate_at(c("H1","Hc"), log) %>% 
    mutate(logBFic=H1-Hc,
           PMP1=exp(logBFic)/(exp(logBFic)+1))
  
  aggrlogBFic<-sum(logBF$logBFic)
  aggrPMP1<-exp(aggrlogBFic)/(exp(aggrlogBFic)+1)
  
  logBF %>% 
    mutate(
           aggrlogBFic=aggrlogBFic,
           aggrPMP1=aggrPMP1
           ) %>% 
    return()
  
})

#add iteration indicator
a<-reps_aggrBF10 %>%
  rlist::list.rbind() %>% 
  mutate(iter=rep(1:iter, each=studies))

saveRDS(a, file = "simulation/output/investigate simulation failure(1).RDS")

nrow(a)
colnames(a)
a$PMP1 %>% unique() %>% hist()
a$logBFic %>% unique() %>% hist() # the distribution of BFic is normal around 0 - makes sense
a$aggrlogBFic%>% unique() %>% hist()
plot()

a %>% 
  filter(iter<10) %>% 
    ggplot()+
    geom_jitter(aes(x=iter, y=PMP1),width = 0.3, height = 0)+
    geom_point(aes(x=iter, y =aggrPMP1))
    scale_x_continuous(breaks = seq(1,10,1))+
    theme_minimal()+
    theme(panel.grid.minor.x=element_line(colour = "darkgrey"))


