## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup_lies, warning=FALSE------------------------------------------------
library(EcoEnsemble)
library(mgcv)
library(ggplot2)
library(cowplot)
set.seed(1234)

## ----configure_priors---------------------------------------------------------
d <- 3 # The number of variables.

priors <- EnsemblePrior(d,
                        ind_st_params = IndSTPrior(AR_params=c(10,2)), 
                        ind_lt_params = IndLTPrior("beta",
                                                   cor_params = list(matrix(5, d, d), matrix(1, d, d))),
                        sha_st_params = ShaSTPrior(AR_params=c(10,2)),
)

## ----sampling priors, eval = FALSE--------------------------------------------
#  M <- 4 # The number of models we are considering.
#  prior_density <- prior_ensemble_model(priors, M = M)
#  ex.fit <- rstan::extract(prior_density$samples) # Extracted samples

## ----truth_best_guesses, eval = FALSE-----------------------------------------
#  Time <- 50
#  
#  true_par <-which.max(ex.fit$lp__)
#  
#  latent <- matrix(NA, Time, (M+2)*d)
#  
#  #Priors on initial values
#  latent[1, 1:d] <- rnorm(d, 0, 1)
#  latent[1, -(1:d)] <- t(chol(ex.fit$SIGMA_init[true_par,-(1:d) ,-(1:d)])) %*% rnorm((M+1)*d, 0, 1)
#  
#  #Find all the latent values of the dynamic linear model
#  SIGMA <- ex.fit$SIGMA[true_par,,]
#  SIGMA_chol <- t(chol(SIGMA))
#  for (t in 2:Time) {
#    latent[t,] <- ex.fit$AR_params[true_par,] * latent[t-1,] + SIGMA_chol %*% rnorm((M+2)*d, 0, 1)
#  }
#  
#  #The truth is simply the first d values
#  truth_latent <- latent[,1:d]
#  
#  #The best guesses are sums of the truth and discrepancies
#  simulator_best_guesses <- array(NA,dim=c(Time,d,M))
#  for(i in 1:M){
#    simulator_best_guesses[,,i] <- t(
#      t(latent[,1:d] + latent[,(d+1):(2*d)] + latent[,(1+i) * d + (1:d)]) +
#        ex.fit$ind_lt[true_par,i,] + ex.fit$sha_lt[true_par,] )
#  }
#  

## ----load_datasets, echo = FALSE----------------------------------------------
#Here I will load in the data
M <- 4
Time <- 50
load("data/SyntheticData_PriorSamples.RData")

## ----plot_data, fig.dim = c(7, 4)---------------------------------------------
plot(truth_latent[,1], ylim=c(-2.5, 3), pch = 19)
lines(simulator_best_guesses[,1,1], col = 2)
lines(simulator_best_guesses[,1,2], col = 3)
lines(simulator_best_guesses[,1,3], col = 4)
lines(simulator_best_guesses[,1,4], col = 5)


## ----add_noise----------------------------------------------------------------
Times_obs <- round(Time * 0.8)
obs <- matrix(NA,Times_obs,d)
for(i in 1:d){
  g1 <- gam(y~s(x,k = 15),data=data.frame(x=1:Times_obs,y = truth_latent[1:Times_obs,i]))
  obs[,i] <- predict(g1)
}

obs.cov <- cov(obs - truth_latent[1:Times_obs,])


model.cov <- array(0,dim=c(M,d,d))
models_output <- array(NA,dim=c(M,Time,d))
for (j in 1:M){
  for(i in 1:d){
    g1 <- gam(y~s(x, k = 15),data=data.frame(x=1:Time,y=simulator_best_guesses[,i,j]))
    models_output[j,,i] <- predict(g1)
  }
  model.cov[j,,] <- cov(models_output[j,,] - simulator_best_guesses[,,j])
}


## ----plot_obs, fig.dim = c(7, 6)----------------------------------------------
#Observations and model outputs
plot(obs[,1], ylim=c(-2.5, 3), xlim = c(1,Time), pch = 19)
lines(models_output[1,,1], col=2, lwd = 2)
lines(models_output[2,,1], col=3, lwd = 2)
lines(models_output[3,,1], col=4, lwd = 2)
lines(models_output[4,,1], col=5, lwd = 2)

## -----------------------------------------------------------------------------
#Create the data frames that we'll use for EcoEnsemble
val_obs <- data.frame(obs); cov_obs <- obs.cov
val_model_1 <- data.frame(models_output[1,,]); cov_model_1 <- model.cov[1,,]
val_model_2 <- data.frame(models_output[2,,]); cov_model_2 <- model.cov[2,,]
val_model_3 <- data.frame(models_output[3,,]); cov_model_3 <- model.cov[3,,]
val_model_4 <- data.frame(models_output[4,,]); cov_model_4 <- model.cov[4,,]

#Set the dimnames to ensure EcoEnsemble can identify the information.
SPECIES_NAMES <- c("Species 1", "Species 2", "Species 3")
dimnames(val_obs) <- list(paste(1:Times_obs), SPECIES_NAMES)
dimnames(val_model_1) <- list(paste(1:Time), SPECIES_NAMES)
dimnames(val_model_2) <- list(paste(1:Time), SPECIES_NAMES)
dimnames(val_model_3) <- list(paste(1:Time), SPECIES_NAMES)
dimnames(val_model_4) <- list(paste(1:Time), SPECIES_NAMES)

dimnames(cov_obs) <- list(SPECIES_NAMES, SPECIES_NAMES)
dimnames(cov_model_1) <- list(SPECIES_NAMES, SPECIES_NAMES)
dimnames(cov_model_2) <- list(SPECIES_NAMES, SPECIES_NAMES)
dimnames(cov_model_3) <- list(SPECIES_NAMES, SPECIES_NAMES)
dimnames(cov_model_4) <- list(SPECIES_NAMES, SPECIES_NAMES)


## ----fitting, eval = FALSE----------------------------------------------------
#  fit <- fit_ensemble_model(observations = list(val_obs, cov_obs),
#                            simulators = list(list(val_model_1, cov_model_1, "Model 1"),
#                                              list(val_model_2, cov_model_2, "Model 2"),
#                                              list(val_model_3, cov_model_3, "Model 3"),
#                                              list(val_model_4, cov_model_4, "Model 4")),
#                            priors = EnsemblePrior(d),
#                            control = list(adapt_delta = 0.9))
#  samples <- generate_sample(fit)

## ----results, eval = FALSE----------------------------------------------------
#  var_index <- 1
#  
#  df <- data.frame("Year" = paste(1:Time),
#                   "Ensemble" = apply(samples@mle[, var_index, ], 1, median),
#                   "Lower"  = apply(samples@samples[, var_index, ], 1, quantile, 0.1),
#                   "Upper"  = apply(samples@samples[, var_index, ], 1, quantile, 0.9),
#                   "Actual" = truth_latent[,var_index])
#  df$Year <- as.numeric(df$Year)
#  df <-  reshape2::melt(df, id.vars=c("Year", "Lower", "Upper"), variable.name="Simulator")
#  
#  # We only want uncertainty bands for the Ensemble values, else we set zero width bands
#  df[df$Simulator == "Actual", c("Lower", "Upper")] <- df[df$Simulator != "Actual", "value"]
#  
#  ggplot(df, aes(x=`Year`, y=`value`)) +
#    geom_line(aes(group=`Simulator`,colour=`Simulator`)) +
#    geom_ribbon(aes(ymin=`Lower`, ymax =`Upper`, fill = `Simulator`), alpha=0.2)
#  

## ----plot_truth, echo = FALSE, fig.dim = c(7, 4)------------------------------
knitr::include_graphics("data/p_truth.png")

## ----eval = FALSE-------------------------------------------------------------
#  fit_M1 <- fit_ensemble_model(observations = list(val_obs, cov_obs),
#                               simulators = list(list(val_model_1, cov_model_1, "Model 1")),
#                               priors = EnsemblePrior(d),
#                               control = list(adapt_delta = 0.9))
#  samples_M1 <- generate_sample(fit_M1)
#  
#  fit_M2 <- fit_ensemble_model(observations = list(val_obs, cov_obs),
#                               simulators = list(list(val_model_1, cov_model_1, "Model 1"),
#                                              list(val_model_2, cov_model_2, "Model 2")),
#                            priors = EnsemblePrior(d),
#                            control = list(adapt_delta = 0.9))
#  samples_M2 <- generate_sample(fit_M2)
#  
#  fit_M3 <- fit_ensemble_model(observations = list(val_obs, cov_obs),
#                            simulators = list(list(val_model_1, cov_model_1, "Model 1"),
#                                              list(val_model_2, cov_model_2, "Model 2"),
#                                              list(val_model_3, cov_model_3, "Model 3")),
#                            priors = EnsemblePrior(d),
#                            control = list(adapt_delta = 0.9))
#  samples_M3 <- generate_sample(fit_M3)
#  
#  plot_grid(
#    plot(samples_M1, variable=1) + ggtitle("1 Model")  + theme(legend.position = "none"),
#    plot(samples_M2, variable=1) + ggtitle("2 Models") + theme(legend.position = "none"),
#    plot(samples_M3, variable=1) + ggtitle("3 Models") + theme(legend.position = "none"),
#    plot(samples   , variable=1) + ggtitle("4 Models") + theme(legend.position = "none"))

## ----model_comparison, echo = FALSE, fig.dim = c(7, 6)------------------------
knitr::include_graphics("data/p_NumberOfModels.png")

## ----quantify_variance, eval = FALSE------------------------------------------
#  def.par <- par(no.readonly=TRUE) #old pars
#  par(mfrow = c(d, 1))
#  legend_ys <- c(0.4, 0.18, 0.12)
#  for(i in 1:d){
#    plot.ts(apply(samples_M1@samples[40:50,i,],1, var),
#            main = paste0("Species ", i), ylab="Variance" )
#    lines(apply(samples_M2@samples[40:50,i,],1, var), col = 2)
#    lines(apply(samples_M3@samples[40:50,i,],1, var), col = 3)
#    lines(apply(samples@samples[40:50,i,],1, var), col = 4)
#    legend(1, legend_ys[i], legend = c("1 Model", "2 Models", "3 Models", "4 Models"),
#            col = 1:4, lty = 1)
#  }
#  par(def.par)

## ----plot_variances, echo=FALSE, fig.dim = c(7,6)-----------------------------
load("data/SyntheticData_AddingModelsData.RData")
def.par <- par(no.readonly=TRUE) #old pars
par(mfrow = c(d, 1), mar = c(4, 4, 1.4, 1.4))
legend_ys <- c(0.4, 0.18, 0.12)
for(i in 1:d){
  plot.ts(var_m1[, i], 
          main = paste0("Species ", i), ylab="Variance" )
  lines(var_m2[, i], col = 2)
  lines(var_m3[, i], col = 3)
  lines(var_m4[, i], col = 4)
  legend(1, legend_ys[i], legend = c("1 Model", "2 Models", "3 Models", "4 Models"),
          col = 1:4, lty = 1)
}
par(def.par)

