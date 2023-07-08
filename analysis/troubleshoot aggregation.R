source("analysis/analysis functions.R")

#Load data
dat<-readRDS("pre-processing/output/processed_data_combined.rds")
dimnames(dat)[[3]]
pops<-dimnames(dat)[[3]]
tau<-attributes(dat)$tau
