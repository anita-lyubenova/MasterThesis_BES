load("Outputs/accuracy/dat_merged.RData")
source("Scripts/accuracy/accuracy_functions.R")


##############################

H1u_H1_Hu<- aggregatePMP(x=dat,
                         h=c("H1","Hu"),
                         studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_Hu")) %>% 
  acc_lineplot()

H1cu_H1_Hu<-aggregatePMP(x=dat,
                         h=c("H1" ,"Hc","Hu"),
                         studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="TRUE_Hu")) %>% 
  acc_lineplot()
ggsave("Outputs/Thesis Draft/Fig2.png", plot = H1u_H1_Hu, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")
ggsave("Outputs/Thesis Draft/Fig3.png", plot = H1cu_H1_Hu, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")


###############################
H1u_H1_HETEROGp1.5<- aggregatePMP(x=dat,
                                  h=c("H1","Hu"),
                                  studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p1.5")) %>% 
  acc_lineplot()

H1cu_H1_HETEROGp1.5<-aggregatePMP(x=dat,
                                  h=c("H1" ,"Hc","Hu"),
                                  studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p1.5")) %>% 
  acc_lineplot()
ggsave("Outputs/Thesis Draft/Fig4.png", plot = H1u_H1_HETEROGp1.5, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")
ggsave("Outputs/Thesis Draft/Fig5.png", plot = H1cu_H1_HETEROGp1.5, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

########################################

###############################
H1u_H1_HETEROGp1.25<- aggregatePMP(x=dat,
                                   h=c("H1","Hu"),
                                   studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p1.25")) %>% 
  acc_lineplot()

H1cu_H1_HETEROGp1.25<-aggregatePMP(x=dat,
                                   h=c("H1" ,"Hc","Hu"),
                                   studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p1.25")) %>% 
  acc_lineplot()
ggsave("Outputs/Thesis Draft/Fig6.png", plot = H1u_H1_HETEROGp1.25, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")
ggsave("Outputs/Thesis Draft/Fig7.png", plot = H1cu_H1_HETEROGp1.25, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

########################################
###############################
H1u_H1_HETEROGp1<- aggregatePMP(x=dat,
                                h=c("H1","Hu"),
                                studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p1")) %>% 
  acc_lineplot()

H1cu_H1_HETEROGp1<-aggregatePMP(x=dat,
                                h=c("H1" ,"Hc","Hu"),
                                studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p1")) %>% 
  acc_lineplot()
ggsave("Outputs/Thesis Draft/Fig8.png", plot = H1u_H1_HETEROGp1, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")
ggsave("Outputs/Thesis Draft/Fig9.png", plot = H1cu_H1_HETEROGp1, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

########################################

###############################
H1u_H1_HETEROGp.75<- aggregatePMP(x=dat,
                                  h=c("H1","Hu"),
                                  studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.75")) %>% 
  acc_lineplot()

H1cu_H1_HETEROGp.75<-aggregatePMP(x=dat,
                                  h=c("H1" ,"Hc","Hu"),
                                  studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.75")) %>% 
  acc_lineplot()
ggsave("Outputs/Thesis Draft/Fig10.png", plot = H1u_H1_HETEROGp.75, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")
ggsave("Outputs/Thesis Draft/Fig11.png", plot = H1cu_H1_HETEROGp.75, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

########################################

###############################
H1u_H1_HETEROGp.5<- aggregatePMP(x=dat,
                                 h=c("H1","Hu"),
                                 studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.5")) %>% 
  acc_lineplot()

H1cu_H1_HETEROGp.5<-aggregatePMP(x=dat,
                                 h=c("H1" ,"Hc","Hu"),
                                 studies = 20) %>% 
  accuracyPMP(hyp_to_pop = c(H1="TRUE_H1", Hu="HETEROG_H1p.5")) %>% 
  acc_lineplot()
ggsave("Outputs/Thesis Draft/Fig12.png", plot = H1u_H1_HETEROGp.5, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")
ggsave("Outputs/Thesis Draft/Fig13.png", plot = H1cu_H1_HETEROGp.5, width = 4, height = 2.5, units = "in", dpi = 300, bg="white")

########################################
ggsave("Outputs/Thesis Draft/Fig2.png", plot = median.PMP1.noHu.plot, width = 6, height = 2.5, units = "in", dpi = 300, bg="white")

