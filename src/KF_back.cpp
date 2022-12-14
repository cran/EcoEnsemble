#include "RcppEigen.h"
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

// [[Rcpp::export]]
Eigen::MatrixXd KalmanFilter_back(Eigen::VectorXd rhos, Eigen::VectorXd dee, Eigen::VectorXd R, Eigen::MatrixXd Q, Eigen::MatrixXd C,// parameters
                            Eigen::MatrixXd P, Eigen::VectorXd xhat, int Time, Eigen::MatrixXd y, Eigen::MatrixXd obs){

  Eigen::MatrixXd xhat_b = Eigen::MatrixXd::Zero(Time, xhat.size());
  Eigen::MatrixXd P_ = P;
  // DO NOT CHANGE THE BELOW. For linux distributions, the below is not a valid definition
  //  because Time is not know at compile time. To have dynamically sized arrays we must use std::vector.
  //  This is not a problem for windows distributions, but will cause problems when you submit to CRAN.
  //Eigen::MatrixXd P_s[Time];
  //Eigen::MatrixXd G[Time];
  std::vector<Eigen::MatrixXd> P_s(Time, Eigen::MatrixXd(xhat.size(), y.cols()));
  std::vector<Eigen::MatrixXd> G(Time, Eigen::MatrixXd(xhat.size(), y.cols()));
  Eigen::MatrixXd A = rhos * rhos.transpose();
  Eigen::VectorXd xhat_ = xhat;
  Eigen::MatrixXd xhat_s = Eigen::MatrixXd::Zero(Time, xhat.size());
  Eigen::MatrixXd G_ = Eigen::MatrixXd::Zero(xhat.size(), y.cols());
  Eigen::MatrixXd er = Eigen::MatrixXd::Zero(Time, y.cols());
  double est;
  Eigen::MatrixXd Qs_inv = Eigen::MatrixXd::Zero(Time, y.cols());
  /// smoothing bit
  Eigen::MatrixXd Identity = Eigen::MatrixXd::Identity(xhat.size(), xhat.size());
  Eigen::MatrixXd L = Eigen::MatrixXd::Zero(xhat.size(), xhat.size());
  Eigen::VectorXd r = Eigen::VectorXd::Zero(xhat.size());
  int k_i;
  int l_j;

  for (int i = 0; i < Time; ++i)
  {
    //JM: Need to initialise some of the other variables
    //G[i] = Eigen::MatrixXd(xhat.size(), y.cols());
    //G.push_back(Eigen::MatrixXd(xhat.size(), y.cols()));

    P_ = P_.cwiseProduct(A) + Q;
    xhat_ = rhos.cwiseProduct(xhat_);

    //P_s[i] = P_ ;
    P_s.at(i) = P_;

    xhat_s.row(i) = xhat_.transpose() ;
    for (int j = 0; j < y.cols(); ++j) // change this to some input that lists the numbers of the elements of y that we are interested in, maybe another value that says how many we are interested in.
    {
      if (obs(i,j)==1.0)
      {
        G_.col(j) = P_ * C.row(j).transpose();
        double Qs = (C.row(j) * G_.col(j) + R(j));
        Qs_inv(i, j) =  1 /Qs;
        est = (xhat_ + dee).dot(C.row(j)) ;
        er(i,j) = y(i,j) - est;
        G[i].col(j) = Qs_inv(i,j) * G_.col(j);
        xhat_ = xhat_ + G[i].col(j) * er(i,j);
        P_ -=  G[i].col(j) * G_.col(j).transpose();
      }
    }
  }
  for (int i = 0; i < Time; ++i)
  {
    //k_i = Time + 1 - i;
    k_i = Time - i - 1;
    int num_cols = y.cols();
    for (int j = 0; j < num_cols; ++j)
    {
      //l_j = num_cols + 1 - j;
      l_j = num_cols - j - 1;
      if (obs(k_i,l_j) == 1.0)
      {
        L = Identity - G[k_i].col(l_j) * C.row(l_j);
        r = C.row(l_j).transpose()  * Qs_inv(k_i,l_j) * er(k_i,l_j) + L.transpose() * r;

      }
    }

    xhat_b.row(k_i) = xhat_s.row(k_i) + (P_s[k_i] * r).transpose();
    r = rhos.cwiseProduct(r);
  }
  return xhat_b;
}
