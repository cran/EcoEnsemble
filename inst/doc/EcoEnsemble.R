## ----include=FALSE------------------------------------------------------------
#Load in the data and load the library
library(EcoEnsemble)
#load("data/vignette_plots.Rdata")

## ----label="Observations"-----------------------------------------------------
SSB_obs
Sigma_obs

## ----label="EwESigma"---------------------------------------------------------
Sigma_ewe

## ----label="lmSigma"----------------------------------------------------------
Sigma_lm

## ----label="mizSigma"---------------------------------------------------------
Sigma_miz

## ----label="fsSigma"----------------------------------------------------------
Sigma_fs

## ----model_outputs, warning=FALSE, fig.dim = c(7, 3), message = FALSE, echo = FALSE----
library(tibble)
library(dplyr)
library(reshape2)
library(ggplot2)

#Inlcude years in the data frames
SSB_obs_tmp <- rownames_to_column(SSB_obs, var = "Year")
SSB_ewe_tmp <- rownames_to_column(SSB_ewe, var = "Year")
SSB_miz_tmp <- rownames_to_column(SSB_miz, var = "Year")
SSB_lm_tmp <- rownames_to_column(SSB_lm, var = "Year")
SSB_fs_tmp <- rownames_to_column(SSB_fs, var = "Year")

#Join dataframes together
df_all <- SSB_obs_tmp %>% 
    full_join(SSB_ewe_tmp, by = "Year", suffix = c("","_ewe")) %>%
    full_join(SSB_miz_tmp, by = "Year", suffix = c("","_miz")) %>%
    full_join(SSB_lm_tmp, by = "Year", suffix = c("","_lm")) %>%
    full_join(SSB_fs_tmp, by = "Year", suffix = c("","_fs"))

#Melt into long data format for ggplot
df_all <- melt(df_all, id.vars = "Year")
colnames(df_all) <- c("Year", "Simulator", "logSSB")
df_all$Year <- as.numeric(df_all$Year)

#Finally create the plots
df_n_pout <- df_all[grepl("N.pout", df_all$Simulator), ]
p1 <- ggplot(data=df_n_pout, aes(x=`Year`, y=`logSSB`, na.rm = TRUE)) + 
  geom_line(aes(group=`Simulator`,colour=`Simulator`)) +
  ggtitle("Norway pout")

df_herring <- df_all[grepl("Herring", df_all$Simulator), ]
p2 <- ggplot(data=df_herring, aes(x=`Year`, y=`logSSB`, na.rm = TRUE)) +    
  geom_line(aes(group=`Simulator`,colour=`Simulator`)) + 
  ggtitle("Herring")

df_cod <- df_all[grepl("Cod", df_all$Simulator), ]
p3 <- ggplot(data=df_cod, aes(x=`Year`, y=`logSSB`, na.rm = TRUE)) +    
  geom_line(aes(group=`Simulator`,colour=`Simulator`))+
  ggtitle("Cod")

df_sole <- df_all[grepl("Sole", df_all$Simulator), ]
p4 <- ggplot(data=df_sole, aes(x=`Year`, y=`logSSB`, na.rm = TRUE)) +    
  geom_line(aes(group=`Simulator`,colour=`Simulator`)) +
  ggtitle("Sole")

cowplot::plot_grid(p1, p2, p3, p4, ncol = 2, nrow = 2)


## -----------------------------------------------------------------------------
#Setting priors for an ensemble model with 4 variables of interest.
priors <- EnsemblePrior(4)

## -----------------------------------------------------------------------------
priors_custom <- EnsemblePrior(
                        d = 4,
                        ind_st_params = IndSTPrior("lkj", list(25, 0.25), 30),
                        ind_lt_params = IndLTPrior(
                          "beta",
                          list(c(25, 25, 25, 10),c(0.25, 0.25, 0.25, 0.1)),
                          list(matrix(40, 4, 4), matrix(40, 4, 4))
                          ),
                        sha_st_params = ShaSTPrior("lkj", list(25, 0.25), 30),
                        sha_lt_params = 3)

## ----label = "fit", eval=FALSE------------------------------------------------
#  fit_sample <- fit_ensemble_model(observations = list(SSB_obs, Sigma_obs),
#                                   simulators = list(list(SSB_ewe, Sigma_ewe, "EwE"),
#                                                     list(SSB_lm,  Sigma_lm,  "LeMans"),
#                                                     list(SSB_miz, Sigma_miz, "mizer"),
#                                                     list(SSB_fs,  Sigma_fs,  "FishSUMS")),
#                                   priors = priors)
#  samples <- generate_sample(fit_sample)

## ----label = "Initial_plots", eval=FALSE--------------------------------------
#  plot(samples)

## ----fig.dim = c(7, 4), echo=FALSE, warning = FALSE---------------------------
knitr::include_graphics("data/plot_initial_outputs.png")

## ----label = "Initial_plots_changed", fig.dim = c(7, 4), eval=FALSE-----------
#  plot(samples, variable = "Cod", quantiles = c(0.25, 0.75)) +
#      ggplot2::theme_classic() +
#      ggplot2::scale_color_brewer(palette="Set2")  +
#      ggplot2::ylab("SSB (log tonnes)")

## ----fig.dim = c(7, 4), echo=FALSE, warning = FALSE---------------------------
knitr::include_graphics("data/plot_initial_customised.png")

## ----visualise_priors, eval= FALSE--------------------------------------------
#  prior_model <- prior_ensemble_model(priors = priors, M = 4)
#  prior_samples <- sample_prior(observations = list(SSB_obs, Sigma_obs),
#                                   simulators = list(list(SSB_ewe, Sigma_ewe, "EwE"),
#                                                     list(SSB_lm,  Sigma_lm,  "LeMans"),
#                                                     list(SSB_miz, Sigma_miz, "mizer"),
#                                                     list(SSB_fs,  Sigma_fs,  "FishSUMS")),
#                                   priors = priors, prior_model)
#  

## ----plot_priors, fig.dim = c(7, 4), eval = FALSE-----------------------------
#  plot(prior_samples)

## ----fig.dim = c(7, 6), echo = FALSE, warning = FALSE-------------------------
knitr::include_graphics("data/plot_priors_only.png")

## ----label = "create_data_manual"---------------------------------------------
ens_data <- EnsembleData(observations = list(SSB_obs, Sigma_obs),
                          simulators = list(list(SSB_ewe, Sigma_ewe, "EwE"),
                                            list(SSB_lm,  Sigma_lm,  "LeMans"),
                                            list(SSB_miz, Sigma_miz, "mizer"),
                                            list(SSB_fs,  Sigma_fs,  "FishSUMS")),
                          priors = priors)


## ----label = "fitting_ensemble_DIY", eval=FALSE-------------------------------
#  mod <- get_mcmc_ensemble_model(priors)
#  samples <- rstan::sampling(mod, data = ens_data@stan_input)
#  fit <- EnsembleFit(ens_data, samples = samples)

## ----eval = FALSE-------------------------------------------------------------
#  # Generating samples using DIY functions
#  transf_data <- get_transformed_data(fit)
#  mle_sample <- gen_sample(1, ex.fit = rstan::extract(samples), transformed_data = transf_data,
#                           time = ens_data@stan_input$time)

## ----eval=FALSE---------------------------------------------------------------
#  fit <- fit_ensemble_model(observations = observations, simulators = simulators,
#                            priors = priors)
#  samples_tmp <- generate_sample(fit)
#  samples <- samples_tmp@samples

