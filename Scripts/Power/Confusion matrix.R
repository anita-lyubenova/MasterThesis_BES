#Try to quantify the true positives and true negatives in hpyothesis evaluation with BF
source("scripts/load_packages.R")
source("scripts/ThomVolker scripts/functions.R")

# a function that 
# (1) simulates data based on specified populations in line with hypohteses of interest
# (2) Computes BFs for each hypohteses of interest in each population
bain_power_sim<-function(
         r2=0.13,#effect size r-squared
         pcor=0.3,#correlation between the predictor variables
         n, #sample size
         hypotheses, #tested hypotheses;
         ratio_beta, # definition of the populations, as determined by the ratio between the regression coefficients b1:b2:b3; should be a named list of numeric vectors, where each vector corresponds to the ratio of betas in hte respective hypothesis (note the order of hypotheses!); the names should only contain the subscript of the hypothesis (e.g "1" or "i" or"0")
         model="linear", #linear, logistic or probit regression
         iter=1000
){
  
  # BF.u<-matrix(NA,nrow = iter, ncol = length(ratio_beta), 
  #            dimnames = list(1:iter, 
  #                            names(ratio_beta)
  #                            ))
  # a list to store the population level coefficients for each hypothesis-population
  betas<-list()
  #a placeholder for the BFs
  BF.u<-array(NA,
              dim = c(iter,
                      length(ratio_beta),
                      length(ratio_beta)),
              dimnames = list(1:iter,
                              paste0("BF", substr(names(ratio_beta),2,2), "u"),
                              paste0("TRUE_", names(ratio_beta))))
  
  #for each population deifned by ratio_beta
  for(b in 1:length(ratio_beta)){

    betas[[b]]<-coefs(r2, ratio_beta[[b]], cormat(pcor, length(ratio_beta)), "normal")
    
    for(i in 1:iter){
      print(paste0("Sample size: ", n ,"; Population: ", b ,"; Iteration: ", i))
      #generate BFs
      BF.u[i,,b]<-gen_dat(r2=r2,
                        betas=unlist(betas[[b]]),
                        rho=cormat(pcor, length(ratio_beta)),
                        n=n,
                        "normal")%$%
        lm(Y ~ V1 + V2 +V3) %>%
        bain(hypothesis = hypotheses)%$%
        fit %>%
        extract(c(1:length(unlist(strsplit(hypotheses, ";"))),nrow(.)),"BF.u")#subset only BFiu for the specified hypotheses and the complement
      
      #[c(1:length(unlist(strsplit(hypotheses, ";"))),nrow(.)),]$BF.u #%>%
        #extract(rownames(.) %in% names(ratio_beta),"BF.u")
    }
  }
  
  return(list(BF=BF.u,
              r2=r2,
              pcor=pcor,
              n=n,
              hypotheses=hypotheses,
              populations=ratio_beta,
              model=model,
              iter=iter
              ))
}

# Data simulation ------------------
hypotheses="V1=V2=V3; V1>V2>V3"
n<-c(50,100,150,200,300,500,800,1200)

power_linear<-list()

for(s in 1:length(n)){
  power_linear[[s]]<-bain_power_sim(
    r2=0.13,#effect size r-squared
    pcor=0.3,#correlation between the predictor variables
    n=n[s], #sample size
    hypotheses="V1=V2=V3; V1>V2>V3", #tested hypotheses;
    ratio_beta=list(H0=c(1,1,1), #population H0 = TRUE
                    H1=c(3,2,1), #population H1 = TRUE
                    Hc=c(1,2,3)), #population Hc = TRUE
    iter=2000
  )
}

save.image(file="Outputs/workspace_confusion matrix_v2.RData")

# Data processing ----------------------------------------------
load("Outputs/workspace_Confusion matrix.RData")


dim(power_linear)
dim(power_linear[[1]])
dimnames(power_linear[[1]]$BF)

#a function to transform the BFs to PMPs, create a confusion matrix, and compute power and alpha
power_matrix<-function(x, # a list with BFs created with bain_power_sim()
                       hyp=1:dim(BF)[[2]] # a numeric vector with column indices of the BF array indicating the tested hypotheses; for them PMPs will be computed
                      # hyp_names = dimnames(BF)[[2]], #a character vector indicating the names of the testerd hypotheses
                      # pop_names=substr(dimnames(BF)[[3]],6,7)
                       ){
  #subset the array such that it only includes BFiu of hypotheses of interest
  BF<-x$BF[,hyp,]
  iter<-x$iter
  n<-x$n
  
  hyp_index = substr(dimnames(BF)[[2]],3,3)
  pop_names = names(x$populations)
  
  #compute PMPi = BFi/sum(BF of all hypotheses of interest)
  PMP<-BF
  for(i in 1:dim(BF)[[1]]){ #for each iteration
    for(j in 1:dim(BF)[[2]]){ #for each tested hypothesis
      for(z in 1:dim(BF)[[3]]){ # for each population
        
        PMP[i,j,z] <- BF[i,j,z]/sum(BF[i,,z])
        
      }  
    }
  }
  
  dimnames(PMP)[[2]]<-paste0("PMP_", hyp_index)
  dimnames(PMP)[[3]]<-paste0("TRUE_", pop_names)
  
  #create an array with the same dimensions as PMP but that will be indicate only the highest PMPs
  max.PMP<-PMP
  #produce power matrix
  for(i in 1:dim(PMP)[[1]]){ #for each iteration
    for(z in 1:dim(PMP)[[3]]){ # for each population
      
      #get the column index of the hypothesis with the highest PMPs
      max.index<-which(PMP[i,,z]==max(PMP[i,,z]))
      # replace the max PMPs with 1 and the remaining PMPs with 0 for iteration i and population z
      max.PMP[i,max.index,z]<-1
      max.PMP[i,-max.index,z]<-0
    }  
  }
  
  conf_matrix<-apply(max.PMP, c(2,3), sum)/dim(max.PMP)[1]
  
  power_alpha<-matrix(NA,
                      nrow=nrow(conf_matrix),
                      ncol = 3,
                      dimnames = list(rownames(conf_matrix),
                                      c("n","power", "alpha")
                                      )
                      ) %>% data.frame()
  

   ro<-rownames(conf_matrix)
   co<-colnames(conf_matrix)
   
   power_alpha$n<-n
   
  for(h in 1:length(hyp_index)){
    power_alpha$power[h]<-conf_matrix[substr(ro,nchar(ro), nchar(ro)) %in% hyp_index[h], substr(co,nchar(co), nchar(co)) %in% hyp_index[h]]
    power_alpha$alpha[h]<-sum(conf_matrix[substr(ro,nchar(ro), nchar(ro)) %in% hyp_index[h],!substr(co,nchar(co), nchar(co)) %in% hyp_index[h]])/2 #divided by 2 because there are two populations under which alpha is accessed
  }
  
  
  return(list(matrix=conf_matrix,
              power_alpha=power_alpha
              )
         )

}


x<-power_linear[[1]]
power_matrix(x, hyp = c(2,3))

#put power and alpha for each hypothesis across all n in the same data frame
for(s in 1:length(n)){
  power_matrix(x, hyp = c(2,3))$power_alpha
}



