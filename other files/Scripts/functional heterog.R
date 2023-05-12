log(2)

par(mfrow=c(2,1))

lx<-rlnorm(10000, meanlog = 1, sdlog = 0.5)
density(lx) %>% plot(type="l")
mean(lx)

x<-rnorm(10000, mean = 1, sd = 0.5)
density(x) %>% plot(type="l")

log(lx) %>% density() %>%  plot(type="l")


u1<-rlnorm(10000, meanlog = 1, sdlog = 0.86*1)
density(u1) %>% plot(type="l")

log(u1) %>% density() %>% plot(type="l")
par(mfrow=c(1,1))
log(1)
