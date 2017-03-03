#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test3(NumericVector parms, NumericVector multiplier){
  //bool ydot;
  NumericVector tmp;
  int a = parms[0];
  int b = parms[1];
  NumericVector c = multiplier;
  
  
  //ydot = parms[0] <= parms[1];
  //tmp = ydot*parms[0];
  tmp = (a <= b)*c;
  
  return tmp;
}


/*** R
test3(c(5,3,45), c(0,1,2,3,4,5))
test3(c(100,100), c(0,1,2,3,4,5))
  */