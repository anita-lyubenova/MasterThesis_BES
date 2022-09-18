
# --------------------------------- METADATA -----------------------------------
#   Thom Volker
#   13-05-2022
#   Simulation 1 in Volker & Klugkist "Combining support for hypotheses over 
#   heterogeneous studies with Bayesian Evidence Synthesis: A simulation study"
# ______________________________________________________________________________



# ----------------------------- Simulation set-up ------------------------------
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
# ______________________________________________________________________________



# ------------------------------ Run simulations -------------------------------

output <- 
  expand_grid(nsim = 1:nsim,                                 # 1. make data.frame with all
              n = n,                                         #    simulation conditions
              pcor = pcor, 
              r2 = r2, 
              model = models) %>%
  mutate(rho   = map(pcor, ~cormat(.x, length(ratio_beta))), # 2. create covariance matrix
         betas = pmap(list(r2, rho, model),                  # 3. calculate regression
                      function(r2, rho, model) {             #    coefficients given
                        coefs(r2, ratio_beta, rho, model)    #    model specifications
                      }),
         fit  = future_pmap(list(r2, betas, rho, n, model),  # 4. generate normal, logit
                      function(r2, betas, rho, n, model) {   #    and probit data, fit
                        data_and_model(r2 = r2,              #    regression model and
                                       betas = betas,        #    evaluate hypothesis
                                       rho = rho,
                                       n = n,
                                       model = model,
                                       formula = Y ~ V1 + V2 + V3 + V4 + V5 + V6,
                                       hypothesis = "V4 < V5 < V6")
                        }, 
                      .options = options))


# -------------------------- Save output simulations ---------------------------

save.image(file = "output/01_sample_size_equal.RData")

