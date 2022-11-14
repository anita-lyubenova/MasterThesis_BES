
if (!require("parallel")) install.packages("parallel") # install this package first (once)
library(parallel)
nrCPUcores <- detectCores(all.tests = FALSE, logical = TRUE) - 1


### Simulation iterations per evaluated scenario
n.iter.sim <- 1000 # 1000

# Determine range of N
MinN1 <- 50 
MaxN1 <- 100
StepN1 <- 25
StepN2 <- 100
MinN2 <- MaxN1 + StepN2
MaxN2 <- 500
#c(seq(MinN1, MaxN1, StepN1), seq(MinN2, MaxN2, StepN2))
if(MaxN2 <= MinN2){"MaxN2 <= MinN2"}


#############################

# ----------------------------
# Packages and functions
# ----------------------------

  # Check if packages are installed 
  list.of.packages <- c("MASS", "Matrix")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # Loading relevant packages (my code)
  library(MASS)   # to call mvrnorm
  #library(Matrix) # to transform vcov matrix to nearest positive definite matrix 
  
  #install.packages(matrixStats)
  library(matrixStats) # colSds()
  
  #install.packages(esc)
  library(esc) # esc_mean_se()
  
  
  if (!require("metafor")) install.packages("metafor")
  library(metafor)
  #
  if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
  library(restriktor)

# ----------------------
# Population settings
# ----------------------
  

### Variable means and covariance matrices in population (specified without intercept)

n.coef <- 2 # Number of dummy variables (model without intercept)
  
  mu <- rep(0, n.coef)
  intercept <- 0


### Underlying statistical models for data generating process

n.studies <- 4
#model <- "lm(AL~0+D1+D2, data=sample)"


### Coefficient ratios
ratios.2 <- c(2,1) 
ratios.133 <- c(1.33, 1)
ratios.0 <- c(0,0)
#
NrRatios <- 1
ratio <- 1
#ratio <- 2
#ratio <- 3


# -------------------------
# Investigated hypotheses
# -------------------------
    
### Experiment 1
H11 <- 'HedgesG < 0.2'
H12 <- 'HedgesG < 0.5'
H13 <- 'HedgesG < 0.8'
H14 <- 'HedgesG > 0.8'

### Experiment 2
H21 <- 'HedgesG < 0.2'
H22 <- 'HedgesG > 0.2; HedgesG < 0.5'
H23 <- 'HedgesG > 0.5; HedgesG < 0.8'
H24 <- 'HedgesG > 0.8'

### Experiment 3
H31 <- 'HedgesG < 0.2'
H32 <- 'HedgesG > 0.2'
H33 <- 'HedgesG < 0.5'
H34 <- 'HedgesG > 0.5'
H35 <- 'HedgesG < 0.8'
H36 <- 'HedgesG > 0.8'

### Experiment 4/7
H41 <- 'HedgesG < 0.2'
H42 <- 'HedgesG > 0.2'
H40 <- 'HedgesG == 0.2'

### Experiment 5/8
H51 <- 'HedgesG < 0.5'
H52 <- 'HedgesG > 0.5'
H50 <- 'HedgesG == 0.5'

### Experiment 6/9
H61 <- 'HedgesG < 0.8'
H62 <- 'HedgesG > 0.8'
H60 <- 'HedgesG == 0.8'

NrExp <- 6+3 # 4 to 6 with and without H0

n.hypos <- c(4, 4, 6, 2, 2, 2, 3, 3, 3)

# Note True hypothesis depends on r2!
TrueHypo <- "Depends on r2"
TrueHypo_0 <- "1, nl abs(HedgesG) = 0 < 0.2"


# ------------------------------------
# Preparation of simulation execution
# ------------------------------------
info <- sessionInfo()
start.time <- Sys.time()


### Empty results objects
rangeN <- c(seq(MinN1, MaxN1, StepN1), seq(MinN2, MaxN2, StepN2))
NrN <- length(rangeN)
r2_values <- c(0.35, 0.25, 0.20, 0.15, 0.08, 0.02, 0.00)
NrR2withRatio <- NrRatios * length(r2_values)
#
if(NrRatios == 1){
  if(ratio == 1){
    names_r2ratio <- c("R2=0.35, 2:1", "R2=0.25, 2:1", "R2=0.20, 2:1", "R2=0.15, 2:1", "R2=0.08, 2:1", "R2=0.02, 2:1", "R2=0.00, 2:1")
  }
  if(ratio == 2){
    names_r2ratio <- c("R2=0.35, 1.33:1", "R2=0.25, 1.33:1", "R2=0.20, 1.33:1", "R2=0.15, 1.33:1", "R2=0.08, 1.33:1", "R2=0.02, 1.33:1", "R2=0.00, 1.33:1")
  }
  if(ratio == 3){
    names_r2ratio <- c("R2=0.35, under H0", "R2=0.25, under H0", "R2=0.20, under H0", "R2=0.15, under H0", "R2=0.08, under H0", "R2=0.02, under H0", "R2=0.00, under H0")
  }
}
names_exp <- c("Experiment 1: overlap", "Experiment 2: ranges", "Experiment 3: all, so with overlap",
               "Experiment 4: small ES", "Experiment 5: medium ES", "Experiment 6: large ES",
               "Experiment 7: small ES, with equal", "Experiment 8: medium ES, with equal", "Experiment 9: large ES, with equal"
               )
# length(names_exp) == NrExp


# Hypothesis rates
HypoRates <- array(data.frame(NA),
             dim = c(NrN, NrR2withRatio, NrExp),
             dimnames = list(rangeN, names_r2ratio, names_exp)
            )

# Final weights
FinalWeights <- array(data.frame(NA),
               dim = c(NrN, NrR2withRatio, NrExp),
               dimnames = list(rangeN, names_r2ratio, names_exp)
                )

FinalAvWeights <- array(data.frame(NA),
                        dim = c(NrN, NrR2withRatio, NrExp),
                        dimnames = list(rangeN, names_r2ratio, names_exp)
                )
FinalQ5Weights <- array(data.frame(NA),
                        dim = c(NrN, NrR2withRatio, NrExp),
                        dimnames = list(rangeN, names_r2ratio, names_exp)
                )
FinalQ95Weights <- array(data.frame(NA),
                        dim = c(NrN, NrR2withRatio, NrExp),
                        dimnames = list(rangeN, names_r2ratio, names_exp)
)
FinalMinWeights <- array(data.frame(NA),
                        dim = c(NrN, NrR2withRatio, NrExp),
                        dimnames = list(rangeN, names_r2ratio, names_exp)
)
FinalMaxWeights <- array(data.frame(NA),
                         dim = c(NrN, NrR2withRatio, NrExp),
                         dimnames = list(rangeN, names_r2ratio, names_exp)
)


# --------------------------------
# Simulation execution
# --------------------------------

set.seed(12345)

  
    
# -------------------------------
# loop over coefficient ratios
# -------------------------------
r2.iteration <- 0 # This will go from 1 to NrRatios*NrR2, which is handy in writing the output
for (ratios in 1:NrRatios) {
  # ratios <- 1
  
  if(NrRatios == 1){
    if(ratio == 1){
      b.ratios <- ratios.2
    }else if(ratio == 2){
      b.ratios <- ratios.133
    }else if(ratio == 3){
      b.ratios <- ratios.0
    }  
  }else{
    if(ratios == 1){
      b.ratios <- ratios.2
    }else if(ratios == 2){
      b.ratios <- ratios.133
    }else if(ratios == 3){
      b.ratios <- ratios.0
    }  
  }
  
  # -------------------------------
  # loop over explained variances
  # -------------------------------
  for (r2 in r2_values) {
    # r2 <- r2_values[1]
    
    r2.iteration <- r2.iteration + 1
    # r2 <- r2_values[r2.iteration%%3]
    
    # ------------------------
    # loop over sample sizes
    # ------------------------
    samplesize.iteration <- 0
    
    for(samplesize in rangeN) {
      # samplesize <- rangeN[2]
      
      samplesize.iteration <<- samplesize.iteration + 1
      # samplesize <- rangeN[samplesize.iteration]
      
      
      # Create empty matrices for storing statistics of interest
        
      # ------------------------------------------------------------------------
      # Loop over individual simulation iterations within a specific scenario
      # ------------------------------------------------------------------------
      
      # Empty results objects for Final gorica weights per iteration
      probs.final <- array(NA, dim = c(n.iter.sim, NrExp, max(n.hypos)))
      
      for (sim.iteration in 1:n.iter.sim){
        # sim.iteration <- 1
        
        # Print out simulation process
        if (sim.iteration == 1 | sim.iteration%%250 == 0) {
          cat(paste0("------------------------------------------ \n"))
          cat(paste0("Ratios: ", ratios, " of ", NrRatios, "\n"))
          if(NrRatios == 3){
            cat(paste0("R2: ", r2.iteration, " of ", length(r2_values), ", but runs with Ratios and thus goes from 1 to 3x3=9", "\n"))
          }
          if(NrRatios == 1){
            cat(paste0("R2: ", r2.iteration, " of ", length(r2_values), "\n"))
          }
          cat(paste0("Sample Size: ", samplesize.iteration, " of ", NrN, "\n"))
          cat(paste0("Simulation iteration: ", sim.iteration, " of ", n.iter.sim, "\n"))
          cat(paste0(Sys.time(), "\n"))
        }
        
        #Param_studies <- list()
        #CovMx_studies <- list()
        #
        # with intercept
        #data <- matrix(NA, nrow = (n.coef+1)*n.studies, ncol = (3+(n.coef+1)))
        # without intercept
        data <- matrix(NA, nrow = (n.coef)*n.studies, ncol = 4)
        teller <- 0
        
        # ------------------------------------------------------
        # In each simulation iteration: Loop over four studies
        # ------------------------------------------------------
        for (study in 1:n.studies) {
          # study <- 1
      
          # Determine true beta coefficients in data generating process
          
          D1 <- matrix(0, nrow = samplesize)
          D1[round(samplesize/2 + 1):samplesize] <- 1
          sample <- data.frame(D1 = D1)
          D2 <- (1 - D1)
          sample$D2 <- D2
          
          #sigma <- cov(as.matrix(sample[,1:n.coef], ncol = n.coef))
          sigma <- matrix(-1, nrow = n.coef, ncol = n.coef)
          diag(sigma) <- 1
          
          # Define error variance
          var.e <- 1 - r2 # (because all vars are standardized, var(resid)=(1-R2)*sigma_y)
          
          
          if(sum(abs(b.ratios)) == 0){ # all zeros (thus under H0)
            betas <- b.ratios
          }else{
            # Solve for x here
            fun <- function (x) {
              (t(b.ratios*x) %*% sigma %*% b.ratios*x) / (t(b.ratios*x) %*% sigma %*% b.ratios*x + var.e) - r2
            }
            
            x <- uniroot(fun, lower=0, upper=100)$root
          
            # Construct betas
            betas <- b.ratios*x
          }
        
          
          epsilon <- rnorm(samplesize, sd=sqrt(var.e))
          sample$AL <- D1 * betas[1] + D2 * betas[2] + epsilon
          
          
          # Obtain Vcov and beta-hat
          fit <- lm(AL ~ 0 + D1 + D2, data=sample)
          ES <- esc_mean_se(grp1m = coef(fit)[1], grp1se = sqrt(vcov(fit)[1,1]), grp1n = sum(D1),
                      grp2m = coef(fit)[2], grp2se = sqrt(vcov(fit)[2,2]), grp2n = sum(D2), 
                      es.type = "g")
          betahat <- ES$es 
          VCovMatrix <- ES$var
          names(betahat) <- c("HedgesG")
          
          
          data[(teller+1):(teller+(n.coef)), 1] <- study
          #data[(teller+1):(teller+(n.coef)), 2] <- names(betahat)
          data[(teller+1):(teller+(n.coef)), 3] <- betahat
          data[(teller+1):(teller+(n.coef)), 4] <- VCovMatrix
          teller <- teller + (n.coef)
            
        } # end loop over the four studies that are aggregated within a simulation iteration
        #
        data <- as.data.frame(data) 
        # with intercept
        #data[,2] <- rep(names(betahat))
        # without intercept
        data[,2] <- rep(names(betahat))
        #data
        

        
        # Meta-an #
        #
        # Univariate
        colnames(data) = c("Study", "outcome_", "yi", "vi") 
        metaan <- rma(yi, vi, data=data, method="ML")
        #metaan
      
        
        # GORICA on meta-an estimates #
        #
        #Substract estmates from meta-an, to be used in goric function
        est <- coef(metaan) # In an example, probably better to do abs()
        names(est) <- names(betahat)
        se_est <- metaan$se
        VCOV_est <- metaan$vb
        #
        # Apply GORICA #
        # set seed: to obtain same results when you re-run it
        #set.seed(123) 
        # Apply GORICA
        # Set of hypo's
        results1 <- goric(est, VCOV = VCOV_est, H11, H12, H13, H14, comparison = "none", type = "gorica")
        #results2 <- goric(est, VCOV = VCOV_est, H21, H22, H23, H24, comparison = "none", type = "gorica")
        # Using bootstrap
        results2 <- goric(est, VCOV = VCOV_est, H21, H22, H23, H24, comparison = "none", type = "gorica",
                          mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)
        results3 <- goric(est, VCOV = VCOV_est, H31, H32, H33, H34, H35, H36, comparison = "none", type = "gorica") 
        results4 <- goric(est, VCOV = VCOV_est, H41, comparison = "complement", type = "gorica")
        results5 <- goric(est, VCOV = VCOV_est, H51, comparison = "complement", type = "gorica")
        results6 <- goric(est, VCOV = VCOV_est, H61, comparison = "complement", type = "gorica")
        results7 <- goric(est, VCOV = VCOV_est, H40, H41, H42, comparison = "none", type = "gorica") 
        results8 <- goric(est, VCOV = VCOV_est, H50, H51, H52, comparison = "none", type = "gorica") 
        results9 <- goric(est, VCOV = VCOV_est, H60, H61, H62, comparison = "none", type = "gorica") 
        
         
        probs.final[sim.iteration,1, 1:n.hypos[1]] <- results1$result[, 5]
        probs.final[sim.iteration,2, 1:n.hypos[2]] <- results2$result[, 5]
        probs.final[sim.iteration,3, 1:n.hypos[3]] <- results3$result[, 5]
        probs.final[sim.iteration,4, 1:n.hypos[4]] <- results4$result[, 5]
        probs.final[sim.iteration,5, 1:n.hypos[5]] <- results5$result[, 5]
        probs.final[sim.iteration,6, 1:n.hypos[6]] <- results6$result[, 5]
        probs.final[sim.iteration,7, 1:n.hypos[7]] <- results7$result[, 5]
        probs.final[sim.iteration,8, 1:n.hypos[8]] <- results8$result[, 5]
        probs.final[sim.iteration,9, 1:n.hypos[9]] <- results9$result[, 5]
        
    } # end loop over individual simulation iterations
      

      
      # --------------------------------------
      # Compute Hypothesis Rates &
      # Finally store statistics of interest
      # --------------------------------------
      
      # ------------------------
      # loop over experiments
      # ------------------------
      for (experiment in 1:NrExp) {
        # experiment <- 1
        #
        # hypothesis rates - now there can be exact same weights!
        # So, index of the first(!) maximum does not suffices.
        hypo.true <- apply(probs.final[, experiment, 1:n.hypos[experiment]], 1, which.max) 
        Max <- apply(probs.final[, experiment, 1:n.hypos[experiment]], 1, max)
        hypo.true <- apply(probs.final[, experiment, 1:n.hypos[experiment]] == Max, 1, which)
        TableHT <- table(unlist(hypo.true))
        count.hypo.true <- matrix(0, nrow = 1, ncol = n.hypos[experiment])
        count.hypo.true[as.numeric(names(TableHT))] <- TableHT
        HypoRates[samplesize.iteration,r2.iteration,experiment] <- list(count.hypo.true)
       
        
        # Final goric weights
        FinalWeights[samplesize.iteration,r2.iteration,experiment] <- list(as.data.frame(probs.final[, experiment, 1:n.hypos[experiment]]))
        
        FinalAvWeights[samplesize.iteration,r2.iteration,experiment] <- list(colMeans(probs.final[, experiment, 1:n.hypos[experiment]]))
        
        quant5and95 <- apply(probs.final[, experiment, 1:n.hypos[experiment]], 2, quantile, probs = c(.05, .95))
        FinalQ5Weights[samplesize.iteration,r2.iteration,experiment] <- list(quant5and95[1,])
        FinalQ95Weights[samplesize.iteration,r2.iteration,experiment] <- list(quant5and95[2,])
        
        FinalMinWeights[samplesize.iteration,r2.iteration,experiment] <- list(apply(probs.final[, experiment, 1:n.hypos[experiment]], 2, min))
        
        FinalMaxWeights[samplesize.iteration,r2.iteration,experiment] <- list(apply(probs.final[, experiment, 1:n.hypos[experiment]], 2, max))
      } # end loop over experiments
        
    } # end loop over sample sizes
    
  } # end loop over explained variances 

} # end loop over coefficient ratios

cat(paste0("------------------------------------------ \n"))
cat(paste0("----------- Simulation is DONE ----------- \n"))
cat(paste0("------------------------------------------ \n"))

# Only keep relevant objects
end.time <- Sys.time()
run.time <- end.time - start.time

# Save data/output
if(NrRatios == 1){
  if(ratio == 1){
    Name <- paste0("4regrES_All_ratio2vs1_Sim", n.iter.sim)
  }
  if(ratio == 2){
    Name <- paste0("4regrES_All_ratio133vs1_Sim", n.iter.sim)
  }
  if(ratio == 3){
    Name <- paste0("4regrES_All_ratioH0_Sim", n.iter.sim)
  }
}
#
rm(list=setdiff(ls(), c("Name", "TrueHypo", "TrueHypo_0", "n.hypos", "n.iter.sim", "r2_values", "rangeN", "HypoRates", "FinalWeights", "FinalAvWeights", "FinalQ5Weights", "FinalQ95Weights", "FinalMinWeights", "FinalMaxWeights", "run.time", "info"))) # only saves these objects
save.image(paste0("Results_", Name, ".RData"))


### end ###