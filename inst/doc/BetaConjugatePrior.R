## ----setup_lies,warning=FALSE-------------------------------------------------
library(EcoEnsemble)
set.seed(1234)

## ----eval = FALSE, message=FALSE, warning=FALSE, results='hide', silent=TRUE----
#  cor_pri_st <- rstan::stan_model(model_code = " functions{
#    real priors_cor_hierarchical_beta(matrix ind_st_cor, int N, matrix M){
#      real log_prior = 0;
#      for (i in 1:(N-1)){
#        for (j in (i+1):N){
#          log_prior += beta_lpdf(0.5*(ind_st_cor[i, j] + 1)| M[i, j], M[j, i] );
#        }
#      }
#      return log_prior;
#    }
#  
#    real beta_conj_prior(real alpha, real betas, real r, real s, real k){
#      real rl = 1/(1 + exp(-r));
#      real sl = 1/(1 + exp(-s));
#      real p = (sl * rl)^k;
#      real q = (sl * (1 - rl))^k;
#      real ret = alpha * log(p) + betas * log(q) - k * lbeta(alpha,betas);
#      return ret;
#    }
#  
#  }
#  
#  data {
#    vector[3] cor_p;
#  }
#  
#  parameters {
#    matrix <lower=0>[5,5] beta_pars;
#    corr_matrix[5] rho[4];
#  }
#  
#  model {
#    for (i in 1:4){
#      for (j in (i+1):5){
#        target += beta_conj_prior(beta_pars[i,j], beta_pars[j,i], cor_p[1], cor_p[2], cor_p[3]);
#      }
#    }
#  
#    for (i in 1:4){
#      target += priors_cor_hierarchical_beta(rho[i],5,beta_pars);
#      diagonal(beta_pars) ~ std_normal();
#    }
#  
#  }
#  
#  generated quantities {
#  
#    matrix [5,5] rhovars;
#    for (i in 1:4){
#      for (j in (i+1):5){
#        rhovars[i,j] = 4 * (beta_pars[i,j] * beta_pars[j,i])/(square(beta_pars[i,j] + beta_pars[j,i]) * (beta_pars[i,j] + beta_pars[j,i] + 1));
#        rhovars[j,i] = (2 * beta_pars[i,j]/(beta_pars[i,j] + beta_pars[j,i])) - 1;
#      }
#    }
#  
#    for (i in 1:5){
#      rhovars[i,i] = 4;
#    }
#  
#  }
#  ")
#  library(ggplot2)
#  rhoplots <- list()
#  kvals <- c(0.05, 5)
#  parvals <- do.call(expand.grid, c(rep(list(c(0.25, 3)), 2), list(kvals)))
#  #Sampling and gathering outputs for plotting
#  for (i in 1:nrow(parvals)){
#    fit_cor <- rstan::sampling(cor_pri_st, data = list(cor_p=as.numeric(parvals[i,])), iter = 2000, chains=4)
#    ex.fit <- rstan::extract(fit_cor)
#    rho_density <- density(ex.fit$rho[,1,1,2], from = -1, to = 1)
#    rhoplot_data <- data.frame(rho_density$x, rho_density$y)
#    names(rhoplot_data) <- c("rho", "Density")
#    rhoplot_data <- cbind(rhoplot_data, rep(parvals[i,1], nrow(rhoplot_data)), rep(parvals[i,2], nrow(rhoplot_data)), rep(parvals[i,3], nrow(rhoplot_data)))
#    names(rhoplot_data)[3:5] <- c("r", "s", "k")
#    rhoplots[[i]] <- rhoplot_data
#  }
#  #Construct plots
#  rhoplot_data <- do.call(rbind, rhoplots)
#  rhoplot_data <- cbind(rhoplot_data, rep("Beta conjugate prior", nrow(rhoplot_data)))
#  names(rhoplot_data)[6] <- "Prior"
#  unif_range <- seq(-1, 1, length.out = nrow(rhoplots[[1]]))
#  unif_data <- data.frame(cbind(rep(unif_range, nrow(parvals)), rep(dbeta((unif_range+1)/2,5/2,5/2)/2, nrow(parvals)), rhoplot_data[,3:5], rep("Uniform prior", nrow(rhoplot_data))))
#  names(unif_data) <- names(rhoplot_data)
#  rhoplot_data <- rbind(rhoplot_data, unif_data)
#  rhoplot1 <- ggplot(rhoplot_data[which(rhoplot_data$k == kvals[1]),]) + geom_area(aes(x = rho, y = Density, color = Prior, fill = Prior), alpha = 0.3, position = "identity") + scale_x_continuous(bquote(rho), sec.axis = sec_axis(~., name = "r", breaks = NULL, labels = NULL)) + scale_y_continuous(sec.axis = sec_axis(~., name = "s", breaks = NULL, labels = NULL)) + theme(aspect.ratio = 1) + facet_grid(rows = vars(r), cols = vars(s)) + scale_color_manual(values = c("blue", "green")) + scale_fill_manual(values = c("blue", "green"))
#  rhoplot2 <- ggplot(rhoplot_data[which(rhoplot_data$k == kvals[2]),], aes(x = rho, y = Density)) + geom_area(aes(color = Prior, fill = Prior), alpha = 0.3, position = "identity") + scale_x_continuous(bquote(rho), sec.axis = sec_axis(~., name = "r", breaks = NULL, labels = NULL)) + scale_y_continuous(sec.axis = sec_axis(~., name = "s", breaks = NULL, labels = NULL)) + theme(aspect.ratio = 1) + facet_grid(rows = vars(r), cols = vars(s)) + scale_color_manual(values = c("blue", "green")) + scale_fill_manual(values = c("blue", "green"))

## ----echo = FALSE, out.width="700px", out.height="700px"----------------------
knitr::include_graphics("data/rhoplot1.png")

## ----eval = FALSE, fig.dim = c(7,6), echo = FALSE-----------------------------
#  rhoplot1

## ----echo = FALSE, out.width="700px", out.height="700px"----------------------
knitr::include_graphics("data/rhoplot2.png")

## ----eval = FALSE, fig.dim = c(7,6), echo = FALSE-----------------------------
#  rhoplot2

## ----message=FALSE, warning=FALSE,eval=FALSE, results='hide', silent=TRUE-----
#  kvals <- c(0.1, 0.2, 0.4, 0.8, 1.6, 3.2)
#  #parvals <- cbind(rep(0.3, 6), rep(-1, 6), kvals)
#  parvals <- cbind(rep(-0.3, 6), rep(3, 6), kvals)
#  rhovarplots <- list()
#  rhomeanplots <- list()
#  for (i in 1:6){
#    fit_cor <- rstan::sampling(cor_pri_st, data = list(cor_p=parvals[i,]), iter = 2000, chains=4)
#    ex.fit <- rstan::extract(fit_cor)
#    rhovar_density <- density(ex.fit$rhovars[,1,2])
#    rhomean_density <- density(ex.fit$rhovars[,2,1])
#    rhovar_data <- data.frame(cbind(rhovar_density$x, rhovar_density$y, rep(kvals[i], length(rhovar_density$x))))
#    rhomean_data <- data.frame(cbind(rhomean_density$x, rhomean_density$y, rep(kvals[i], length(rhomean_density$x))))
#    names(rhovar_data) <- c("Variance", "Density", "k")
#    names(rhomean_data) <- c("Expectation", "Density", "k")
#    rhovarplots[[i]] <- rhovar_data
#    rhomeanplots[[i]] <- rhomean_data
#  }
#  rhovarplot_data <- do.call(rbind, rhovarplots)
#  rhomeanplot_data <- do.call(rbind, rhomeanplots)
#  rhovarplot <- ggplot(rhovarplot_data) + geom_area(aes(x = Variance, y = Density), color = "blue", fill = "blue", alpha = 0.3, position = "identity") + geom_vline(xintercept = 0.2, color = "green", linetype = "dashed", linewidth = 0.8) + facet_wrap(vars(k), nrow = 2, ncol = 3) + scale_x_continuous(sec.axis = sec_axis(~., name = "k", breaks = NULL, labels = NULL))
#  rhomeanplot <- ggplot(rhomeanplot_data) + geom_area(aes(x = Expectation, y = Density), color = "blue", fill = "blue", alpha = 0.3, position = "identity") + geom_vline(xintercept = 0, color = "green", linetype = "dashed", linewidth = 0.8) + facet_wrap(vars(k), nrow = 2, ncol = 3) + scale_x_continuous(sec.axis = sec_axis(~., name = "k", breaks = NULL, labels = NULL))

## ----echo = FALSE, out.width="600px", out.height = "500px"--------------------
knitr::include_graphics("data/rhovarplot.png")

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  rhovarplot

## ----echo = FALSE, out.width = "600px", out.height = "500px"------------------
knitr::include_graphics("data/rhomeanplot.png")

## ----eval=FALSE, fig.dim = c(8,5), echo = FALSE-------------------------------
#  rhomeanplot

## ----eval = FALSE, message = FALSE, warning = FALSE, silent = TRUE, results = 'hide'----
#  fit_cor <- rstan::sampling(cor_pri_st, data = list(cor_p=c(0.25, 3, 4), iter = 2000, chains=4))
#  ex.fit <- rstan::extract(fit_cor)
#  rhoplot_data  <- data.frame(ex.fit$rho[,1,1,2])
#  names(rhoplot_data) <- "rho"
#  rhoplot <- ggplot(rhoplot_data) + geom_histogram(aes(x = rho), color = "blue", fill = "blue", alpha = 0.3, binwidth = 0.05) + scale_x_continuous(bquote(rho))

## ----echo = FALSE, out.width = "400px", out.height = "400px"------------------
knitr::include_graphics("data/rhoplot.png")

## ----eval = FALSE, echo = FALSE, warning = FALSE, fig.dim = c(4,3)------------
#  rhoplot

## ----eval=FALSE,message=FALSE, warning = FALSE, results = 'hide',silent=TRUE----
#  priors <- EnsemblePrior(4,
#                          ind_st_params =IndSTPrior("hierarchical_beta_conjugate",list(-3,1,8,4),
#                                                    list(0.25,3,4),AR_params=c(2,2)),
#                          ind_lt_params = IndLTPrior("lkj",list(1,1),1),
#                          sha_st_params = ShaSTPrior("lkj",list(1,10),1,AR_params=c(2,2)),
#                          sha_lt_params = 5
#  )
#  prior_density <- prior_ensemble_model(priors, M = 4)
#  samples <- sample_prior(observations = list(SSB_obs, Sigma_obs),
#               simulators = list(list(SSB_ewe, Sigma_ewe,"EwE"),
#                     list(SSB_fs ,  Sigma_fs,"FishSums"),
#                     list(SSB_lm ,  Sigma_lm,"LeMans"),
#                     list(SSB_miz, Sigma_miz,"mizer")),
#               priors=priors,
#               sam_priors = prior_density)

## ----eval = FALSE, fig.dim = c(7, 6)------------------------------------------
#  plot(samples)

## ----echo = FALSE, out.height="500px", out.width="800px"----------------------
knitr::include_graphics("data/p_priors_beta_conj.png")

