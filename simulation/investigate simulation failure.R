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

d=1.1
N=300
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
  names(var_d) <- "var"
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

set.seed(123)
set.seed(234)
step3<-lapply(1:10, function(i){obtain_BF_full(N=300, delta=0, tau=1)}) %>% rlist::list.rbind() %>% as.data.frame()

log3<-step3 %>% 
  dplyr::select(H1, Hc) %>% 
  log() %>% 
  mutate(BFic=H1-Hc)

aggr_l_BFic<-sum(log3$BFic)

PMP<-c(PMP,exp(aggr_l_BFic)/(exp(aggr_l_BFic)+1))

set.seed(234)

reps_aggrBF10<-lapply(1:10, function(r) {
  
  step3<-lapply(1:10, function(i){obtain_BF_full(N=300, delta=0, tau=1)}) %>% rlist::list.rbind() %>% as.data.frame()
  
  log3<-step3 %>% 
    dplyr::select(-Hu) %>% 
    mutate_at(H1,Hc, log)
    mutate(BFic=H1-Hc)
  
  aggr_l_BFic<-sum(log3$BFic)
  PMP<-exp(aggr_l_BFic)/(exp(aggr_l_BFic)+1)
  
  log3 %>% 
    mutate(
           aggr_l_BFic=aggr_l_BFic,
           PMP=PMP
           ) %>% 
    return()
  
  
})
reps<-reps_aggrBF10 %>%
  rlist::list.rbind() %>% 
  mutate(rep=rep(1:10, each=1000))


reps$PMP %>% unique() %>% hist()
reps$aggr_l_BFic %>% unique() %>% hist()

# step3$H1 %>% hist()
# step3$Hc %>% hist()
# step3_l<-step3[,c(1,3)] %>% 
#   reshape2::melt() %>% 
#   rename(hyp=variable,
#          BF=value)
# step3_l %>% 
#   ggplot(aes(x=BF, group=hyp, fill=hyp), alpha=0.2)+
#   geom_histogram(aes(y=..count..))+
#   theme_minimal()
# 
# step3_l %>% 
#   group_by(hyp) %>% 
#   summarise(n=n(),
#             mean=mean(BF),
#             median=median(BF)
#             )
# sum(log(step3$H1))
# sum(log(step3$Hc2))

