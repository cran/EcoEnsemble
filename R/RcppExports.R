# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

KalmanFilter_back <- function(rhos, dee, R, Q, C, P, xhat, Time, y, obs) {
    .Call(`_EcoEnsemble_KalmanFilter_back`, rhos, dee, R, Q, C, P, xhat, Time, y, obs)
}

