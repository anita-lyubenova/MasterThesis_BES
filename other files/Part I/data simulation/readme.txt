the function single_sim() produces a single lm objec based on data simulated according to the following conditions:
- r2         #R-squared of the model
- pcor	     # correlation between the predictor variables
- ratio_beta # indicates the number of predictors and the (average) ratio of the coefficients; the values are sampled from a mvrnorm(ratio_beta, Sigma = p*ratio_beta)
- p          # controls heterogeneity by defining the mvrnorm from which the ratios are sampled
- n 
- model = c("linear", "logistic", "probit")


That is, there are 6 dimensions that can be varied.
For each set of simulation conditions, there is one dimension containing the iterations, and another one containing the tested hypothesis. Thus, the final file is a 6+2=8 dimensional array. It is not necessary for each dimension to contain more than 1 level. E.g., if only R2=0.13 is of interest, then the R2-dimension will have only one level. However, it is important to have the 8-dimensional strucutre in case the simulations are extended to different R2s. 


planned simulation conditions
r2=0.13
pcor=0.3
ratio_beta=list(H1_321 = c(3,2,1),
		Hc_123 = c(1,2,3),
		H0_r000 = c(0,0,0)
) 
n = c(25,50,100,150,200,250,300,350,500, 800)
p = c(0, 0.5, 0.6829787)

iter=1000
t=1:30

array[t, hyp, pop, iter, n]

H1_r.13_pcor.3_b321_p0_linear