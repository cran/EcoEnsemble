## ---- echo=FALSE,warning=FALSE,error=FALSE,message=FALSE----------------------
library(rstan)
library(EcoEnsemble)

## ----eval=FALSE---------------------------------------------------------------
#  inv_wish_st_mod <- stan_model(model_code="data {
#    int<lower=0> N;
#    real nu;
#    matrix[N,N] mat_cov;
#  }
#  parameters {
#    cov_matrix[N] Sigma;
#  }
#  model {
#    Sigma ~ inv_wishart(nu, mat_cov);
#  }
#  ")

## ----eval=FALSE---------------------------------------------------------------
#  fit.iw <- sampling(inv_wish_st_mod,data=list(N=4,nu=2*4,mat_cov=diag(4)))

## ----echo=FALSE---------------------------------------------------------------
load("data/ExploringPriors_invWishartDensities.RData")

## ----eval = FALSE-------------------------------------------------------------
#  ex.fit.iw <- extract(fit.iw)
#  plot(density(ex.fit.iw$Sigma[,1,1]),main="",xlab=expression(lambda),xlim=c(0,2))

## ----plotDensity, echo = FALSE------------------------------------------------
plot(invWishartDensity,main="",xlab=expression(lambda),xlim=c(0,2))

## ----eval = FALSE-------------------------------------------------------------
#  sd.2050 <- sqrt(33*ex.fit.iw$Sigma[,1,1])
#  plot(density(sqrt(33*ex.fit.iw$Sigma[,1,1])),main="",xlab="standard deviation in 2050",xlim=c(0,5))

## ----plotSDDensity, echo = FALSE----------------------------------------------
plot(density(sd.2050),main="",xlab="standard deviation in 2050",xlim=c(0,5))

## -----------------------------------------------------------------------------
x <- seq(5,20,length.out=1000)
plot(x,colMeans(sapply(x, function(xs){dnorm(xs,tail(SSB_obs$Cod,n=1),sd.2050)})),type="l",axes=F,ylab="density",xlab="Cod in 2050 (1000 tonnes)")
labsx <- log(signif(exp(seq(5,20,length.out=7)),1))
axis(1,at=labsx,labels=c(exp(labsx)/1000))
axis(2)
abline(v=tail(SSB_obs$Cod,n=1))
abline(v=range(SSB_obs$Cod),lty=2)
box()

## -----------------------------------------------------------------------------
xs <- seq(0,5,0.001)
plot(xs,dgamma(xs,1,1),xlab=expression(pi[gamma,i]^2),ylab="density",type="l")

## -----------------------------------------------------------------------------
pis <- rgamma(1e5,1,1)
plot(density(sqrt(2 * pis)),xlab="Standard deviation",ylab="Density",main="")

## -----------------------------------------------------------------------------
xs <- seq(0,5,length.out=1000)
plot(xs,2*colMeans(sapply(xs, function(x){dnorm(x,0,sqrt(2 * pis))})),type="l",axes=T,ylab="density",xlab="Difference of two models")

## ----eval=FALSE---------------------------------------------------------------
#  cor_pri_st <- stan_model(model_code = " functions{
#    real priors_cor_hierarchical_beta(matrix ind_st_cor, int N, matrix M){
#      real log_prior = 0;
#      for (i in 1:(N-1)){
#        for (j in (i+1):N){
#          log_prior += beta_lpdf(0.5*(ind_st_cor[i, j] + 1)| M[i, j], M[j, i] );
#        }
#      }
#      return log_prior;
#    }
#  }
#  data {
#  
#    vector[4] cor_p;
#  }
#  
#  parameters {
#    matrix <lower=0>[5,5] beta_pars;
#    corr_matrix[5] rho[4];
#  }
#  model {
#  
#    for (i in 1:4){
#      for (j in (i+1):5){
#        beta_pars[i,j] ~ gamma(cor_p[1],cor_p[2]);
#        beta_pars[j,i] ~ gamma(cor_p[3],cor_p[4]);
#      }
#    }
#    for(i in 1:4){
#      target += priors_cor_hierarchical_beta(rho[i],5,beta_pars);
#      diagonal(beta_pars) ~ std_normal();
#    }
#  }
#  ")
#  fit_cor <- sampling(cor_pri_st,data=list(cor_p=0.1 * c(1,1,1,1)),chains=4)
#  ex.fit <- extract(fit_cor)
#  def.par <- par(no.readonly=TRUE) #Old pars to reset afterwards
#  par(mfrow=c(2,2))
#  plot(density(ex.fit$beta_pars[,1,2]),xlab=expression(alpha[rho]),main="")
#  plot(density(log(ex.fit$beta_pars[,1,2]/ex.fit$beta_pars[,2,1])),main="",xlab="Expected log odds")
#  plot(density(ex.fit$rho[,1,1,2]),xlab=expression(rho),main="")
#  xs <- seq(-1,1,length.out=100)
#  lines(xs,dbeta((xs+1)/2,2,2)/2,col="red")
#  plot(density(apply(ex.fit$rho[,,1,2],1,var)),xlim=c(0,0.35),main="",xlab=expression(var(rho)))
#  par(def.par)

## ----echo=FALSE, fig.dim = c(7,6)---------------------------------------------
def.par <- par(no.readonly=TRUE) #old pars
load("data/ExploringPriors_HierBetaDensities.RData")
par(mfrow=c(2,2))
plot(beta_marginal,xlab=expression(alpha[rho]),main="")
plot(expected_log_odds,main="",xlab="Expected log odds")
plot(rho_density,xlab=expression(rho),main="")
xs <- seq(-1,1,length.out=100)
lines(xs,dbeta((xs+1)/2,2,2)/2,col="red")
plot(rho_var_density,xlim=c(0,0.35),main="",xlab=expression(var(rho)))

## ----eval=FALSE---------------------------------------------------------------
#  st_mod1 <- stan_model(model_code = "data {
#    vector[4] gam_p;
#  }
#  parameters {
#    vector [5] log_sha_st_var[4];
#    vector[5] gamma_mean;
#    vector<lower=0> [5] gamma_var;
#  }
#  model {
#    gamma_mean ~ normal(gam_p[1],gam_p[2]);
#    gamma_var ~ inv_gamma(gam_p[3],gam_p[4]);
#    for (i in 1:4){
#      log_sha_st_var[i] ~ normal(gamma_mean,sqrt(gamma_var));
#  
#    }
#  }
#  generated quantities{
#    vector[5] sha_st_var[4];
#    for (i in 1:4){
#      sha_st_var[i] = exp(log_sha_st_var[i]);
#    }
#  }
#  ")
#  test.fit_norm <- sampling(st_mod1,data=list(gam_p=c(-3,1,8,4)),chains=4)
#  ex.fit <- extract(test.fit_norm)
#  def.par <- par(no.readonly=TRUE) #old pars
#  par(mfrow=c(2,2))
#  plot(density(ex.fit$gamma_mean[,1]),main="",xlab=expression(mu[pi]))
#  plot(density(ex.fit$gamma_var[,1]),xlim=c(0,1),main="",xlab=expression(sigma[pi]^2))
#  plot(density(ex.fit$sha_st_var[,1,1]),xlim=c(0,1),main="",xlab=expression(pi["k,i"]^2))
#  plot(density(apply(ex.fit$sha_st_var[,,1],1,var)),xlim=c(0,0.2),xlab=expression(var(pi["k,i"]^2)) ,main="")
#  par(def.par)

## ----echo = FALSE,fig.dim=c(7, 6)---------------------------------------------
load("data/ExploringPriors_HierGammaDensities.RData")
par(mfrow=c(2,2))
plot(gamma_mean,main="",xlab=expression(mu[pi]))
plot(gamma_var,xlim=c(0,1),main="",xlab=expression(sigma[pi]^2))
plot(sha_st_var,xlim=c(0,1),main="",xlab=expression(pi["k,i"]^2))
plot(sha_st_var_var,xlim=c(0,0.2),xlab=expression(var(pi["k,i"]^2)) ,main="")

## -----------------------------------------------------------------------------
hist(rbeta(1e5,2,2)*2 - 1,probability = T,xlab=expression(r["k,i"]),main="")

## -----------------------------------------------------------------------------
plot(density(rgamma(1e5,1,10)),xlim=c(0,0.5), main="",xlab=expression(pi[eta]^2))

## -----------------------------------------------------------------------------
plot(density(sqrt(rgamma(1e5,1,10))),xlim=c(0,0.5), main="",xlab=expression(pi[eta]))

## -----------------------------------------------------------------------------
hist(rbeta(1e5,2,2)*2 - 1,probability = T,xlab=expression(r["k,i"]),main="")

## ----eval=FALSE,fig.dim=c(7,6)------------------------------------------------
#  priors <- EnsemblePrior(4,
#                          ind_st_params = IndSTPrior("hierarchical",list(-3,1,8,4),list(0.1,0.1,0.1,0.1),AR_params=c(2,2)),
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
#  plot(samples)

## ----echo =FALSE,fig.dim=c(7,6)-----------------------------------------------
knitr::include_graphics("data/p_priors.png")

