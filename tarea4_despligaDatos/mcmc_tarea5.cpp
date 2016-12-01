#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp:export]]
double loglikelihood2(NumericVector x, NumericVector y, NumericVector theta){
  double a = theta[0];
  double b = theta[1];
  double std = theta[2];
  
  return(std);
}

/*** R
timesTwo(42)
*/
