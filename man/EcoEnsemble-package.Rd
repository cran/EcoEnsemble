% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EcoEnsemble-package.R
\docType{package}
\name{EcoEnsemble-package}
\alias{EcoEnsemble-package}
\alias{EcoEnsemble}
\title{A general framework for combining ecosystem models}
\description{
The \code{EcoEnsemble} package implements the framework for combining ecosystem models laid out in Spence et al (2018).
}
\details{
The ensemble model can be implemented in three main stages:
\enumerate{
\item Eliciting priors on discrepancy terms: This is done by using the \code{EnsemblePrior} constructor.
\item Fitting the ensemble model: Using \code{fit_ensemble_model} with simulator outputs, observations and prior information. The ensemble model can be fit, obtaining either the point estimate, which maximises the posterior density, or running Markov chain Monte Carlo to generate a sample from the posterior denisty of the ensemble model.
\item Sampling the latent variables from the fitted model: Using \code{generate_sample} with the fitted ensemble object, the discrepancy terms and the ensemble's best guess of the truth can be generated. Similarly to \code{fit_ensemble_model}, this can either be a point estimate or a full sample.
}
}
\references{
Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. \url{https://mc-stan.org}

Spence, M. A., J. L. Blanchard, A. G. Rossberg, M. R. Heath, J. J. Heymans, S. Mackinson, N. Serpetti, D. C. Speirs, R. B. Thorpe, and P. G. Blackwell. 2018. "A General Framework for Combining Ecosystem Models." Fish and Fisheries 19: 1013-42. \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/faf.12310}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/CefasRepRes/EcoEnsemble}
  \item Report bugs at \url{https://github.com/CefasRepRes/EcoEnsemble/issues}
}

}
\author{
\strong{Maintainer}: Michael A. Spence \email{michael.spence@cefas.gov.uk} (\href{https://orcid.org/0000-0002-3445-7979}{ORCID})

Authors:
\itemize{
  \item James A. Martindale (\href{https://orcid.org/0000-0002-1913-5592}{ORCID})
  \item Michael J. Thomson (\href{https://orcid.org/0000-0003-0284-0129}{ORCID})
}

}
