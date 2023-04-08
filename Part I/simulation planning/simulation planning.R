library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(plotly)
#HETREROGENEITZ ----------------------------------------------------------------------------------------------------

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
  geom_vline(xintercept=mean.win1, color="black")+
  geom_text(mapping = aes(x=mean.win1+0.2,
                          y=22
  ),
  label=paste0("M[win]==", round(mean.win1, 2)), 
  color="black",
  parse = FALSE)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(0,2, 0.2))+
  labs(x="p", y="Meta-analyses count")
p.plot1

ggsave("Part I/simulation planning/output/Methods_f1_pdist.png", plot = p.plot1, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")


p.plot2<-ggplot(ps.long) +
  geom_histogram(aes(x = value, fill = variable),
                 binwidth = 0.1, color = "black", alpha=0.7)+
  geom_vline(mapping = aes(xintercept=mean.win, color=variable))+
  geom_text(mapping = aes(x=mean.win+0.25,
                          y=38,
                          label=paste0("M[win]==", round(mean.win, 2)), 
                          color=variable
                          ),
            parse = TRUE)+
  scale_x_continuous(breaks = seq(0,2,0.2))+
  facet_wrap(~variable)+
  theme_bw()+
  labs(x="p", y="Count")+
  theme(legend.position = "none")
p.plots






# SAMPLE SIZES --------------------------------------------------------------------------------------------
# IMPORT DATA ----------------------------------------------------------------------------------------------------

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

# ss<-data.frame(raw.n=rawdat$N,
#                nwin1=winsor(rawdat$N, trim = .1),
#                nwin2=winsor(rawdat$N, trim = .2)
#                )

ss<-data.frame(raw.n=rawdat$N,
               nwin1=winsor(rawdat$N, trim = .1))

ggplot(ss) +
  geom_histogram(aes(x = nwin1),
                 binwidth = 10, fill = "#619CFF", color = scales::muted("#619CFF",50))+
  scale_x_continuous(breaks = seq(0,500, by=50))+
  scale_y_continuous(breaks=seq(0,800, 100))+
  theme_minimal()+
  labs(x="Sample size", y= "Meta-analyses count")


