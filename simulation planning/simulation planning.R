library(readxl)
library(dplyr)
library(psych)
library(ggplot2)


#list all sheets
all.sheets<-excel_sheets("simulation planning/data/Linden & Honekopp main results.xlsx")
MA.sheets<-all.sheets[2:4]
#import all excel sheets, where each sheet becomes an element in the list dat_list
datl <- lapply(MA.sheets, #for each name in all.sheets...
               function(x){
                 read_excel("simulation planning/data/Linden & Honekopp main results.xlsx", sheet = x) #... read the respective sheet into a list element
               }
)

dat<-bind_rows(datl)

# Set size T --------------------------------------------
dat$k %>% hist()
ts<-data.frame(twin1=winsor(dat$k, trim = .1))
hist(ts$twin1)
#HETREROGENEITY ----------------------------------------------------------------------------------------------------

# T=p*d => p=T/d
p=dat$T/dat$abs_d
p
hist(p)

#winsorized mean
mean.win1<-psych::winsor.mean(p, trim = 0.1)
psych::winsor.sd(p, trim = 0.1)
p.win1<-psych::winsor(p, trim=0.1)
hist(p.win1)

mean.win2<-psych::winsor.mean(p, trim = 0.2)
psych::winsor.sd(p, trim = 0.2)
p.win2<-psych::winsor(p, trim=0.2)
hist(p.win2)

ps<-data.frame(pwin1=winsor(p, trim = .1))

ps.long<-reshape2::melt(ps) %>% 
  mutate(mean.win=case_when(variable=="pwin1" ~ mean.win1,
                            variable=="pwin2" ~ mean.win2
                            ),
         variable=as.factor(variable)
         )
levels(ps.long$variable)<- c("Trim 10%", "Trim 20%")

p.plot1<-ggplot(ps) +
  geom_histogram(aes(x = pwin1),
                 binwidth = 0.1,fill = "#F8766D", color = scales::muted("#F8766D",50))+
  #scale_x_continuous(breaks = seq(0,2, by=0.1))+
  geom_vline(xintercept=c(0.5,mean.win1), color="black")+
  # geom_text(mapping = aes(x=mean.win1+0.3,
  #                         y=22
  #     ),
  #     label=paste0("M[win]==", round(mean.win1, 2)),
  #     color="black",
  #     parse = TRUE)+
  theme_minimal()+
  scale_x_continuous(breaks = c(0.2, 0.5,0.86, 1.2,1.4,1.6,1.8,2))+
  labs(x="cv", y="Meta-analyses count"
      # title="Distribution of 10% winsorized p across 150 meta-analyses"
       )+
  theme(plot.title = element_text(size=7))
p.plot1

ggsave("simulation planning/output/hist_p.png", plot = p.plot1, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")


# p.plot2<-ggplot(ps.long) +
#   geom_histogram(aes(x = value, fill = variable),
#                  binwidth = 0.1, color = "black", alpha=0.7)+
#   geom_vline(mapping = aes(xintercept=mean.win, color=variable))+
#   geom_text(mapping = aes(x=mean.win+0.25,
#                           y=38,
#                           label=paste0("M[win]==", round(mean.win, 2)), 
#                           color=variable
#                           ),
#             parse = TRUE)+
#   scale_x_continuous(breaks = seq(0,2,0.2))+
#   facet_wrap(~variable)+
#   theme_bw()+
#   labs(x="p", y="Count")+
#   theme(legend.position = "none")
# p.plots



# SAMPLE SIZES --------------------------------------------------------------------------------------------

#list all sheets
all.sheets.raw<-excel_sheets("simulation planning/data/Linden & Hönekopp raw data.xlsx")


#subset the meta-analyes only
MA.sheets.raw<-grep("\\d{4}$",all.sheets.raw, value = TRUE)
#import all excel sheets, where each sheet becomes an element in the list dat_list
rawdatl <- lapply(MA.sheets.raw, #for each name in all.sheets...
                   function(x){
                     read_excel("simulation planning/data/Linden & Hönekopp raw data.xlsx", sheet = x) #... read the respective sheet into a list element
                   }
)

rawdat<-bind_rows(rawdatl)
hist(rawdat$N)
summary(rawdat$N)

n.win<-winsor(rawdat$N, trim = .1)
hist(n.win)

simulations.n<-c(25,50,75,100,150,200,300,500)

# ss<-data.frame(raw.n=rawdat$N,
#                nwin1=winsor(rawdat$N, trim = .1),
#                nwin2=winsor(rawdat$N, trim = .2)
#                )

ss<-data.frame(raw.n=rawdat$N,
               nwin1=winsor(rawdat$N, trim = .1))

dist_n<-ggplot(ss) +
  geom_histogram(aes(x = nwin1),
                 binwidth = 10, fill = "#619CFF", color = scales::muted("#619CFF",50))+
  scale_x_continuous(breaks = seq(0,500, by=50))+
  scale_y_continuous(breaks=seq(0,800, 100))+
  geom_vline(xintercept=simulations.n, color="black")+
  theme_minimal()+
  labs(x="Sample size", y= "Meta-analyses count"
       #title = "Distribution of 10% winsorized N across studies included in 150 meta-anayses"
       )+
  theme(plot.title = element_text(size=7))

dist_n

ggsave("simulation planning/output/hist_n.png", plot = dist_n, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

# Tau ---------------------------------------------------------------

dat$T %>% hist
tau_hist<-dat %>% 
  ggplot()+
  geom_histogram(aes(x=T),
                 binwidth = 0.025, fill = "gray", color = scales::muted("gray"))+
  scale_x_continuous(breaks = seq(0,1,0.05))+
  scale_y_continuous(breaks = seq(0,15,1))+
  theme_minimal()

# Cohens D ----------------------------------------------------
D_hist<-dat %>% 
  ggplot()+
  geom_histogram(aes(x=d),
                 binwidth = 0.1,fill = "gray", color = scales::muted("gray"))+
   scale_x_continuous(breaks = seq(-1.5,2.5,0.1))+
   scale_y_continuous(breaks = seq(0,17,1))+
  theme_minimal()

# Predictive intervals ----------------------------------------------------
#compute proportion o fthe predictive intervals that is different in sign than the overall delta

## Load results data ----------------------------------------------------------------
#list all sheets
all.sheets<-excel_sheets("simulation planning/data/Linden & Honekopp main results.xlsx")
MA.sheets<-all.sheets[2:4]
#import all excel sheets, where each sheet becomes an element in the list dat_list
datl <- lapply(MA.sheets, #for each name in all.sheets...
               function(x){
                 read_excel("simulation planning/data/Linden & Honekopp main results.xlsx", sheet = x) #... read the respective sheet into a list element
               }
)

dat<-bind_rows(datl)

pint<-function(delta, tau, percent = 0.95){
  lb<-(1-percent)/2
  int<-qnorm(p=c(lb, 1-lb),mean=delta, sd=tau)
  prop<-pnorm(q=0, delta, tau)
  c(int, prop) %>% setNames(c("lb", "ub", "prop<0"))
}

pint(0.2,0.3, 0.95)

ints<-sapply(1:nrow(dat), function(i){
  pint(delta = dat$d[i], tau = dat$T[i], 0.95)
}) %>% t() %>% round(3)

dat<-cbind(dat,ints)

dat<-dat %>% 
  mutate(prop_opposite = case_when(d>0 ~ `prop<0`,
                                   d<0 ~ 1-`prop<0`
                                   )
         )

dat$prop_opposite %>% hist

sum(dat$prop_opposite>0.3)
#20 out of 150 meta-analyses had proportion of the predictive interval with sign different than 
# the overall effect larger than 0.3
sum(dat$prop_opposite>0.25)


dat %>% 
  ggplot(aes(x=prop_opposite))+
  geom_histogram(
                 binwidth = 0.025, fill = "gray", color = "black",
                 boundary=0.025)+
  scale_x_continuous(breaks = seq(0,0.5,0.05))+
  scale_y_continuous(breaks = seq(0,55,5))+
  theme_minimal()+
  theme( panel.grid.minor = element_line(color="white"))

dat %>% 
  ggplot(mapping=aes(x=`T`, y=prop_opposite, color=d))+
  geom_point(shape=16, size=3)+
  geom_smooth()+
  scale_color_viridis_b()+
  scale_y_continuous(limits = c(0,0.5))+
  theme_minimal()

dat$k
tau.I2_LH<-dat %>% 
  filter(k<300) %>%
  ggplot(mapping=aes(x=`T`, y=I2))+
  geom_point(shape=16, size=3)+
  geom_smooth()+
  scale_color_viridis_b(n.breaks=6)+
  scale_y_continuous(limits = c(0,100), breaks =seq(0,100, by=10))+
  scale_x_continuous(limits = c(0,1), breaks =seq(0,1, by=0.1))+
  theme_minimal()



dat %>%
  filter(k<300,
         dat$abs_d<0.5) %>% 
  ggplot(mapping=aes(x=k, y=I2, color=`T`))+
  geom_point(shape=16, size=3)+
  scale_x_continuous(limits = c(0,135), breaks =seq(0,140, by=10))+
  scale_color_viridis_b(n.breaks=12)+
  geom_smooth()
# van Erp et al. -----------------------------------------------------------------

dat_erp <- read_excel("simulation planning/data/van Erp et al. data.xlsx",.name_repair = "universal")

tau.I2_erp<-dat_erp %>% 
  filter(Type.of.ES=="Cohen's d") %>%
  ggplot(mapping=aes(x=tau, y=I.2))+
  geom_point(shape=16, size=3)+
  geom_smooth()+
  scale_y_continuous(limits = c(0,100), breaks =seq(0,100, by=10))+
  scale_x_continuous(limits = c(0,1), breaks =seq(0,1, by=0.1))+
  theme_minimal()

library(patchwork)
library(plotly)
tau.I2_LH+tau.I2_erp

dat_erp %>%
  ggplot(mapping=aes(x=tau, y=Q))+
  geom_point(shape=16, size=3)+
  geom_smooth()+
  # scale_y_continuous(limits = c(0,100), breaks =seq(0,100, by=10))+
  # scale_x_continuous(limits = c(0,1), breaks =seq(0,1, by=0.1))+
  theme_minimal() 


dat_erp %>%
  ggplot(mapping=aes(x=Reference, y=tau))+
  geom_point(shape=16, size=3)+
  geom_smooth()+
  # scale_y_continuous(limits = c(0,100), breaks =seq(0,100, by=10))+
  # scale_x_continuous(limits = c(0,1), breaks =seq(0,1, by=0.1))+
  theme_minimal() 




a<-dat_erp %>% 
  group_by(Reference) %>% 
  summarise(prop_larger0.30=sum(tau>0.30)) 

table(a$prop_larger0.30)[2:8] %>% sum()
#31 of the 61 meta-analyses reported 1 or more effects with tau larger than 0.3
table(a$prop_larger0.30)[3:8] %>% sum()
#16 of the 61 meta-analyses reported 2 or more effects with tau larger than 0.3

#HOWEVER: it is not clear what are the effect sizes related to these tau... if they are large -> won't matter in conjoint testing









