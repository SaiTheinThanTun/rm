#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test2(NumericVector parms){
  //bool ydot;
  NumericVector tmp;
  
  
  //ydot = parms[0] <= parms[1];
  //tmp = ydot*parms[0];
  tmp = (parms[0] <= parms[1])*parms[0];
  
  return tmp;
}


/*** R
test2(c(5,3,45))
  */