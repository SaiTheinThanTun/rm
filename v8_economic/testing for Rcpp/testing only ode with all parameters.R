library(deSolve)
library(Rcpp)


parametersR <- c(
  bh = 7,                 # bites per human per year
  eta = 50,
  covEDAT0 = 30,
  covITN0 = 75,
  effITN = 30,
  covIRS0 = 0,
  effIRS = 15,
  muC = 1,
  muA = 1,
  muU = 1,
  percfail2018 = 30,
  percfail2019 = 10,
  percfail2020 = 20,
  
  EDATscale = 1,
  covEDATi = 90,
  ITNscale = 0.5,
  covITNi = 90,
  RCDscale = .25, # 2,
  covRCDi = 50,
  delayRCD = 4,
  
  clustRCDrad = 15,
  
  clustRCDcoex = 20,
  RCDsensC = 95,
  RCDsensA = 60,
  RCDsensU = 0,
  IRSscale = 1,
  covIRSi = 90,
  
  cmda_1 = 50,
  cmda_2 = 50,
  cmda_3 = 50,
  tm_1 = 1,          # timing of 1st round [2018 to 2021 - 1 month steps]
  tm_2 = 2,          # timing of 2nd round [2018+(1/12) to 2021 - 1 month steps]
  tm_3 = 3,          # timing of 3rd round [2018+(2/12) to 2021 - 1 month steps]
  dm = 6,
  lossd = 30,
  cm_1 = 80,
  cm_2 = 95,
  cm_3 = 95,
  
  MSATscale = 2,
  covMSATi = 80,
  MSATsensC = 95,
  MSATsensA = 60,
  MSATsensU = 0,
  RCDrad = 5,
  RCDs = 5, 
  
  
  
  bh_max = 50,
  covRCD0 = 0,
  covMSAT0 = 0,
  effv_1=0,
  effv_2=0,
  effv_3=0,
  rhoa = 70,
  rhou = 30,
  ps =90,
  pr=20,
  omega=2,
  nuC=3,
  nuA=6,
  nuU=100,
  mu=50,
  nuTr=14,
  nuTrp=7,
  dv=1,
  timei=2018,
  amp = 0.7,                   # relative amplitude seasonality [N]
  phi = 0.5,                   # phase angle seasonality [N]
  epsilonh=0.23,                 # per bite probability of an infectious mosquito infecting a human
  epsilonm=0.5,                  # per bite probability of an infectious human infecting a mosquito
  b=365/3,                       # per mosquito rate of biting
  deltam=365/14,                 
  gammam=365/10,
  kRCD = 0.017,                
  cRCD = 105,                
  bRCD = 0.024,
  gRCD = 230,
  muRCDw=4,
  sdRCDw=1.5
)

API <- 2.5
# initial prevalence
initprev <- 0.001*API
scenario_i<-c(EDATon = 1,
              ITNon = 1,
              RCDon = 1,
              RCDcoex = 1,
              IRSon = 1,
              MDAon = 1,
              primon = 1,
              MSATon = 1)
haha <- c(initprev, scenario_i,parametersR)

# population size
initP<-10000 

initS_0<-0.5*(1-initprev)*initP
initIC_0<-0
initIA_0<-initprev*initP
initIU_0<-0
initR_0<-0.5*(1-initprev)*initP
initTr_0<-0

state <- c(Y = 0, Cinc_det = 0, Cinc_tot = 0, 
           S_0 = initS_0, IC_0 = initIC_0, IA_0 = initIA_0, IU_0 = initIU_0, R_0 = initR_0, Tr_0 = initTr_0, Sm_0 = 0, Rm_0 = 0,
           S_1 = 0, IC_1 = 0, IA_1 = 0, IU_1 = 0, R_1 = 0, Tr_1 = 0, Sm_1 = 0, Rm_1 = 0,
           S_2 = 0, IC_2 = 0, IA_2 = 0, IU_2 = 0, R_2 = 0, Tr_2 = 0, Sm_2 = 0, Rm_2 = 0,
           S_3 = 0, IC_3 = 0, IA_3 = 0, IU_3 = 0, R_3 = 0, Tr_3 = 0, Sm_3 = 0, Rm_3 = 0
)

#out <- ode(y = state, times = times, func = modGMS, parms = parameters)
WmodGMSrcpp<-function(t,state,parameters){
  tmp<-modGMSrcpp(t,state,parameters)
  return(list(tmp))
}
out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = haha)