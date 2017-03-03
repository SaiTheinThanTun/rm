#library(rstan)
library(deSolve)
library(Rcpp)
sourceCpp("D:\\Dropbox\\IBM project_Sai\\RAI model\\v8_economic\\testing for Rcpp\\lorenz.cpp")

#expose_stan_functions("lorenz.stan")

# parms[1]=beta ,[2]=sigma,[3]=rho
parameters1<-c(2.66666,10,28)

# x=-10, y=-12, z=30.05
init.state<-c(-10,-12,30.05)

nderivs<-function(t,state,parameters){
  tmp<-lorenz(t,state,parameters)
  return(list(tmp))
}

run1<-ode(y=init.state,times=seq(0,15,0.01), func=lorenz, parms=parameters1)
plot(run1)
