#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test2(NumericVector parms){
  //bool ydot;
  NumericVector tmp;
  int a = parms[0];
  int b = parms[1];
  
  
  //ydot = parms[0] <= parms[1];
  //tmp = ydot*parms[0];
  tmp = (a <= b)*a;
  
  return tmp;
}


/*** R
test2(c(5,3,45))
  */