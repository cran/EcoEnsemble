## ----setup_lies, warning=FALSE, message=FALSE, silent = TRUE------------------
library(EcoEnsemble)
library(mgcv)
library(ggplot2)
library(cowplot)
set.seed(5678)

## ----configure_priors, eval = FALSE-------------------------------------------
#  d <- 3 # The number of variables.
#  
#  priors <- EnsemblePrior(d,
#                          ind_st_params = IndSTPrior(AR_params=c(10,2)),
#                          ind_lt_params = IndLTPrior("beta",
#                                                     cor_params = list(matrix(5, d, d),
#                                                                       matrix(1, d, d)),var_params = list(1,0.5)),
#                          sha_st_params = ShaSTPrior(AR_params=c(10,2)),
#  )

## ----sampling priors, eval = FALSE--------------------------------------------
#  M <- 3 # The number of simulators we are considering.
#  MM <- 2 # The number of drivers we are considering.
#  prior_density <- prior_ensemble_model(priors, M = M, MM = MM)
#  ex.fit <- rstan::extract(prior_density$samples) # Extracted samples

## ----truth_best_guesses, eval = FALSE-----------------------------------------
#  Time <- 50
#  
#  true_par <-which.max(ex.fit$lp__)
#  
#  latent <- matrix(NA, Time, (M+MM+2)*d)
#  
#  #Priors on initial values
#  latent[1, 1:d] <- rnorm(d, 0, 1)
#  latent[1, -(1:d)] <- t(chol(ex.fit$SIGMA_init[true_par,-(1:d) ,-(1:d)])) %*% rnorm((M+MM+1)*d, 0, 1)
#  
#  #Find all the latent values of the dynamic linear model
#  SIGMA <- ex.fit$SIGMA[true_par,,]
#  SIGMA_chol <- t(chol(SIGMA))
#  for (t in 2:Time) {
#    latent[t,] <- ex.fit$AR_params[true_par,] * latent[t-1,] + SIGMA_chol %*% rnorm((M+MM+2)*d, 0, 1)
#  }
#  
#  #The truth is simply the first d values
#  truth_latent <- latent[,1:d]
#  
#  #The best guesses are sums of the truth and discrepancies
#  simulator_best_guesses <- array(NA,dim=c(Time,d,M*MM))
#  for(i in 1:M){
#    for (j in 1:MM){
#      simulator_best_guesses[,,MM*(i-1)+j] <- t(
#        t(latent[,1:d] + latent[,(d+1):(2*d)] + latent[,(i + 1)*d + (1:d)] + latent[,(M + j + 1)*d + (1:d)]) +
#          ex.fit$ind_lt[true_par,i,] + ex.fit$ind_lt_dri[true_par,j,] + ex.fit$sha_lt[true_par,])
#    }
#  }

## ----plot_data, eval = FALSE--------------------------------------------------
#  plotlist <- list()
#  for (sim in 1:M){
#    for (dri in 1:MM){
#      plotlist[[MM*(sim-1)+dri]] <- cbind(c(1:Time),simulator_best_guesses[,1,MM*(sim-1)+dri],rep(sim,Time),rep(dri,Time),rep("sim",Time))
#    }
#  }
#  plot_df <- data.frame(rbind(do.call(rbind, plotlist), cbind(c(1:Time), truth_latent[,1], rep(0,Time), rep(0,Time), rep("truth",Time))))
#  names(plot_df) <- c("Year", "Value", "Simulator", "Driver", "Type")
#  plot_df$Year <- as.numeric(plot_df$Year)
#  plot_df$Value <- as.numeric(plot_df$Value)
#  bgplot <- ggplot2::ggplot(plot_df) + geom_line(data = plot_df[which(plot_df$Type == "sim"),], aes(x = Year, y = Value, color = Simulator, linetype = Driver), linewidth = 0.8) + geom_line(data = plot_df[which(plot_df$Type == "truth"),], aes(x = Year, y = Value), linewidth = 1, color = "purple")

## ---- echo = FALSE, out.width="500px", out.height="500px"---------------------
knitr::include_graphics("data/bgplot.png")

## ----add_noise, eval = FALSE--------------------------------------------------
#  Times_obs <- round(Time * 0.8)
#  obs <- matrix(NA,Times_obs,d)
#  for(i in 1:d){
#    g1 <- gam(y~s(x,k = 15),data=data.frame(x=1:Times_obs,y = truth_latent[1:Times_obs,i]))
#    obs[,i] <- predict(g1)
#  }
#  
#  obs.cov <- cov(obs - truth_latent[1:Times_obs,])
#  
#  
#  model.cov <- array(0,dim=c(M*MM,d,d))
#  models_output <- array(NA,dim=c(M*MM,Time,d))
#  for (j in 1:(M*MM)){
#    for(i in 1:d){
#      g1 <- gam(y~s(x, k = 15),data=data.frame(x=1:Time,y=simulator_best_guesses[,i,j]))
#      models_output[j,,i] <- predict(g1)
#    }
#    model.cov[j,,] <- cov(models_output[j,,] - simulator_best_guesses[,,j])
#  }

## ----plot_obs, eval = FALSE---------------------------------------------------
#  #Observations and model outputs
#  plotlist <- list()
#  for (sim in 1:M){
#    for (dri in 1:MM){
#      plotlist[[MM*(sim-1)+dri]] <- cbind(c(1:Time),models_output[MM*(sim-1)+dri,,1],rep(sim,Time),rep(dri,Time),rep("sim",Time),rep("observed", Time))
#    }
#  }
#  plot_df <- data.frame(rbind(do.call(rbind, plotlist), cbind(c(1:Time), c(obs[,1],rep(0, Time - max(Times_obs))), rep(0,Time), rep(0,Time), rep("truth",Time), c(rep("observed", max(Times_obs)),rep("unobserved", Time - max(Times_obs))))))
#  names(plot_df) <- c("Year", "Value", "Simulator", "Driver", "Type", "Obs_Status")
#  plot_df$Year <- as.numeric(plot_df$Year)
#  plot_df$Value <- as.numeric(plot_df$Value)
#  obsplot <- ggplot(plot_df) + geom_line(data=plot_df[which(plot_df$Type == "sim"),], aes(x = Year, y = Value, color = Simulator, linetype = Driver), linewidth = 0.8) + geom_point(data = plot_df[intersect(which(plot_df$Type == "truth"),which(plot_df$Obs_Status == "observed")),], aes(x = Year, y = Value), color = "purple")

## ---- echo = FALSE, out.width="500px", out.height="500px"---------------------
knitr::include_graphics("data/obsplot.png")

## ---- eval = FALSE------------------------------------------------------------
#  #Create the data frames that we'll use for EcoEnsemble
#  val_obs <- data.frame(obs); cov_obs <- obs.cov
#  val_model_11 <- data.frame(models_output[1,,]); cov_model_11 <- model.cov[1,,]
#  val_model_12 <- data.frame(models_output[2,,]); cov_model_12 <- model.cov[2,,]
#  val_model_21 <- data.frame(models_output[3,,]); cov_model_21 <- model.cov[3,,]
#  val_model_22 <- data.frame(models_output[4,,]); cov_model_22 <- model.cov[4,,]
#  val_model_31 <- data.frame(models_output[5,,]); cov_model_31 <- model.cov[5,,]
#  val_model_32 <- data.frame(models_output[6,,]); cov_model_32 <- model.cov[6,,]
#  
#  #Set the dimnames to ensure EcoEnsemble can identify the information.
#  SPECIES_NAMES <- c("Species 1", "Species 2", "Species 3")
#  dimnames(val_obs) <- list(paste(1:Times_obs), SPECIES_NAMES)
#  dimnames(val_model_11) <- list(paste(1:Time), SPECIES_NAMES)
#  dimnames(val_model_21) <- list(paste(1:Time), SPECIES_NAMES)
#  dimnames(val_model_12) <- list(paste(1:Time), SPECIES_NAMES)
#  dimnames(val_model_22) <- list(paste(1:Time), SPECIES_NAMES)
#  dimnames(val_model_31) <- list(paste(1:Time), SPECIES_NAMES)
#  dimnames(val_model_32) <- list(paste(1:Time), SPECIES_NAMES)
#  
#  dimnames(cov_obs) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  dimnames(cov_model_11) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  dimnames(cov_model_21) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  dimnames(cov_model_12) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  dimnames(cov_model_22) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  dimnames(cov_model_31) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  dimnames(cov_model_32) <- list(SPECIES_NAMES, SPECIES_NAMES)
#  
#  val_model_1 <- list(val_model_11, val_model_12)
#  val_model_2 <- list(val_model_21, val_model_22)
#  val_model_3 <- list(val_model_31, val_model_32)
#  cov_model_1 <- list(cov_model_11, cov_model_12)
#  cov_model_2 <- list(cov_model_21, cov_model_22)
#  cov_model_3 <- list(cov_model_31, cov_model_32)

## ----fitting, eval = FALSE----------------------------------------------------
#  fit <- fit_ensemble_model(observations = list(val_obs, cov_obs),
#                                               simulators = list(list(val_model_1, cov_model_1, "Simulator 1", c("Driver 1", "Driver 2")),
#                                                                 list(val_model_2, cov_model_2, "Simulator 2", c("Driver 1", "Driver 2")),
#                                                                 list(val_model_3, cov_model_3, "Simulator 3", c("Driver 1", "Driver 2"))
#                                               ),
#                                               priors = EnsemblePrior(d),
#                                               control = list(adapt_delta = 0.9),drivers=T)
#  samples <- generate_sample(fit)

## ----results, eval = FALSE----------------------------------------------------
#  df_list <- list()
#  for (var_index in 1:3){
#    df <- data.frame("Year" = paste(1:Time),
#                     "Ensemble" = apply(samples@mle[, var_index, ], 1, median),
#                     "Lower"  = apply(samples@samples[, var_index, ], 1, quantile, 0.1),
#                     "Upper"  = apply(samples@samples[, var_index, ], 1, quantile, 0.9),
#                     "Actual" = truth_latent[,var_index])
#    df$Year <- as.numeric(df$Year)
#    df <-  reshape2::melt(df, id.vars=c("Year", "Lower", "Upper"), variable.name="Simulator")
#    # We only want uncertainty bands for the Ensemble values, else we set zero width bands
#    df[df$Simulator == "Actual", c("Lower", "Upper")] <- df[df$Simulator != "Actual", "value"]
#    df$Species = rep(SPECIES_NAMES[var_index], Time)
#    df_list[[var_index]] <- ggplot(df, aes(x=`Year`, y=`value`)) +
#    geom_line(aes(group=`Simulator`,colour=`Simulator`)) +
#    geom_ribbon(aes(ymin=`Lower`, ymax =`Upper`, fill = `Simulator`), alpha=0.2) + ggplot2::ggtitle(SPECIES_NAMES[var_index]) + ggplot2::theme(legend.position = "right")
#  }
#  
#  legend <- cowplot::get_plot_component(df_list[[1]], "guide-box-right")
#  df_list <- lapply(df_list, function(x) {x + ggplot2::theme(legend.position = "none")})
#  output_plot_nolegend <- cowplot::plot_grid(plotlist=df_list, nrow = 3, ncol = 1)
#  output_plot <- cowplot::plot_grid(output_plot_nolegend, legend, rel_widths = c(4,1), nrow = 1, ncol = 2)

## ----plot_truth, echo = FALSE, out.width="600px", out.height="600px"----------
knitr::include_graphics("data/output_plot.png")

## ---- eval = FALSE------------------------------------------------------------
#  samples_array <- as.array(fit@samples)
#  #Drop last element of third dimension as this is "lp__"
#  max(apply(samples_array[,,-dim(samples_array)[3]], 3, rstan::Rhat), na.rm = TRUE)
#  min(apply(samples_array[,,-dim(samples_array)[3]], 3, rstan::ess_bulk), na.rm = TRUE)
#  min(apply(samples_array[,,-dim(samples_array)[3]], 3, rstan::ess_tail), na.rm = TRUE)

## ----echo=FALSE---------------------------------------------------------------
1.007311; 836.737; 559.6022

## ---- eval = FALSE------------------------------------------------------------
#  rstan::check_divergences(fit@samples)

## ---- echo = FALSE------------------------------------------------------------
message(sprintf("%s of %s iterations ended with a divergence (%s%%).", 
            5, 4000, 100 * 5/4000), "\nTry increasing 'adapt_delta' to remove the divergences.")

## ---- eval = FALSE, fig.dim = c(7,4)------------------------------------------
#  rstan::traceplot(fit@samples, pars = "ind_st_sd[3,2]")

## ---- echo = FALSE, out.width = "500px", out.height = "300px"-----------------
knitr::include_graphics("data/drivers_trace.png")

