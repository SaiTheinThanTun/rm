#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
List test(double t, NumericVector y,NumericVector parms){
  List  ydot(6);
  
  // parms[1]=beta ,[2]=sigma,[3]=rho
  
  ydot[3] = parms["beta"];
  
  return ydot;
}