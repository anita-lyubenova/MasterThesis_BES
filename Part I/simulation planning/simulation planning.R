library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(plotly)

#list all sheets
all.sheets<-excel_sheets("Part I/simulation planning/Linden & Honekopp main results.xlsx")
MA.sheets<-all.sheets[2:4]
#import all excel sheets, where each sheet becomes an element in the list dat_list
datl <- lapply(MA.sheets, #for each name in all.sheets...
               function(x){
                 read_excel("Part I/simulation planning/Linden & Honekopp main results.xlsx", sheet = x) #... read the respective sheet into a list element
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
                 binwidth = 0.1, fill = "#F8766D", color = scales::muted("#F8766D",50))+
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

ggsave("Part I/simulation planning/output/hist_p.png", plot = p.plot1, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")


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

## Prop studies originatin from H1-------------------------
source("Part I/simulation/simulation functions.R")

### cv=0.86 -----------------------------
set.seed(123)
r2=0.13
pcor=0.3
ratio_beta<-c(3,2,1)
p<-0.86
n.studies=10000
#sample new ratios from a log-normal distribution  
#with mean = ratio, and sd=p*ratio
m<-ratio_beta
s=p*ratio_beta
# in order to draw from a log-normal dist with these mean and sd
# the location and shape parameters must be reparametrized
#https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))

betas<-c()

for(i in 1:n.studies){
  new_ratio<-c()
for(r in 1:length(ratio_beta)){
  new_ratio[r]<-rlnorm(n=1, location[r], shape[r])
}

betas<-rbind(betas,coefs(r2, new_ratio, cormat(pcor, length(new_ratio)), "normal"))
}

H1.true<-sapply(1:nrow(betas), function(i){
  betas[i,1]> betas[i,2] & betas[i,2] > betas[i,3]
})
sum(H1.true)/n.studies



### cv = 0.50 ------------------------
set.seed(123)
r2=0.13
pcor=0.3
ratio_beta<-c(3,2,1)
p<-0.50
n.studies=10000
#sample new ratios from a log-normal distribution  
#with mean = ratio, and sd=p*ratio
m<-ratio_beta
s=p*ratio_beta
# in order to draw from a log-normal dist with these mean and sd
# the location and shape parameters must be reparametrized
#https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))

betas<-c()

for(i in 1:n.studies){
  new_ratio<-c()
  for(r in 1:length(ratio_beta)){
    new_ratio[r]<-rlnorm(n=1, location[r], shape[r])
  }
  
  betas<-rbind(betas,coefs(r2, new_ratio, cormat(pcor, length(new_ratio)), "normal"))
}

H1.true<-sapply(1:nrow(betas), function(i){
  betas[i,1]> betas[i,2] & betas[i,2] > betas[i,3]
})
sum(H1.true)/n.studies



# SAMPLE SIZES --------------------------------------------------------------------------------------------

#list all sheets
all.sheets.raw<-excel_sheets("Part I/simulation planning/Linden & Hönekopp raw data.xlsx")


#subset the meta-analyes only
MA.sheets.raw<-grep("\\d{4}$",all.sheets.raw, value = TRUE)
#import all excel sheets, where each sheet becomes an element in the list dat_list
rawdatl <- lapply(MA.sheets.raw, #for each name in all.sheets...
                   function(x){
                     read_excel("Part I/simulation planning/Linden & Hönekopp raw data.xlsx", sheet = x) #... read the respective sheet into a list element
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



ggsave("Part I/simulation planning/output/hist_n.png", plot = dist_n, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

