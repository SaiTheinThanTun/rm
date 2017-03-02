// functions{
//   vector lorenz(real t,vector y,vector parms){
//     vector[3] ydot;
//     // parms[1]=beta ,[2]=sigma,[3]=rho
//     
//     ydot[1]=parms[2]*(y[2]-y[1]);
//     ydot[2]=y[1]*(parms[3]-y[3])-y[2];
//     ydot[3]=y[1]*y[2]-parms[1]*y[3];
// 
//     return ydot;
//   }
// }
// model{
// }

#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
List lorenz(double t, NumericVector y,NumericVector parms){
    List  ydot(6);
  
    // parms[1]=beta ,[2]=sigma,[3]=rho

    ydot[0]=parms[2]*(y[1]-y[0]);
    ydot[1]=y[1]*(parms[2]-y[2])-y[1];
    ydot[2]=y[0]*y[1]-parms[0]*y[2];
    ydot[3]=parms[2]*(y[1]-y[0]);
    ydot[4]=y[1]*(parms[2]-y[2])-y[1];
    ydot[5]=y[0]*y[1]-parms[0]*y[2];

    return ydot;
}
