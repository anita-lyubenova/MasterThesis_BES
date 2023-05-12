
# --------------------------------- METADATA -----------------------------------
#   Thom Volker
#   13-05-2022
#   Simulation 2 in Volker & Klugkist "Combining support for hypotheses over 
#   heterogeneous studies with Bayesian Evidence Synthesis: A simulation study"
# ______________________________________________________________________________



# ----------------------------- Simulation set-up ------------------------------
#
#   Data-generating mechanism is outlined in file `00_run_simulations.R`.
#   Over 1000 iterations, we apply BES to a collection of three studies, 
#   generated with normal, logit and probit regression models, using the effect
#   sizes R^2 = {0.02, 0.09, 0.25}, the relative strength of predictor variables
#   B = {0, 1, 1, 1, 2, 3}, common correlation between predictors rho = 0.3, 
#   and sample sizes n = {25, 50, 100, 200, 400, 800}.
#
#   In each study, we fit a complete regression model containing all six
#   predictors, and we evaluate the hypothesis beta_4 < beta_5 < beta_6, which 
#   corresponds to V4 < V5 < V6 in the code
#
#   In this simulation, we also let a single study out of the three have a
#   a sample size of n = 25, whereas the other two have either n = 25 or 
#   n = 50 or n = 100 or n = 200 or n = 400 or n = 800. 
#
# ______________________________________________________________________________



# ------------------------Specify adjusted sample sizes ------------------------

# The original sample sizes. 
n_initial <- n                       

# Create data.frame with all simulation conditions
output <- expand_grid(nsim = 1:nsim,
                      n_initial = n_initial,
                      pcor = pcor,
                      r2 = r2,
                      model = models)

# Sample one of the three studies, this study is in the next step specified as
# having a sample size of n = 25, rather than the original sample size. 

n25 <- map(1:(nrow(output)/3), ~sample(c(FALSE,FALSE,TRUE))) %>% unlist()



# ------------------------------ Run simulations -------------------------------


output <-
  output %>%
  mutate(n     = ifelse(n25, 25, n_initial),
         rho   = map(pcor, ~cormat(.x, length(ratio_beta))),     # 2. create covariance matrix
         betas = pmap(list(r2, rho, model),                      # 3. calculate regression
                      function(r2, rho, model) {                 #    coefficients given
                        coefs(r2, ratio_beta, rho, model)        #    model specifications
                      }),
         fit  = future_pmap(list(r2, betas, rho, n, model),      # 4. generate normal, logit and
                            function(r2, betas, rho, n, model) { #    probit data, fit regression
                              data_and_model(r2 = r2,            #    model and evaluate hypothesis
                                             betas = betas, 
                                             rho = rho,
                                             n = n,
                                             model = model,
                                             formula = Y ~ V1 + V2 + V3 + V4 + V5 + V6,
                                             hypothesis = "V4 < V5 < V6")
                            }, 
                            .options = options))

# --------------------------- Save output simulations --------------------------

save.image(file = "output/02_sample_size_underpowered.RData")
