
library(deSolve)
library(shiny)
library(TSA)


#non-reactive parameters
# define the number of weeks to run the model
dt<-1/12
startyear<-2010
stopyear<-2025
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)
tsteps<-length(times)


#non-reactive function runGMS is now outside of the server function
runGMS<-function(initprev, scenario, param) 
{
  #MODEL PARAMETERS
  parameters <- c(scenario,
                  effv_1 = 0,                  # protective efficacy of a single dose of RTS,S [N]
                  effv_2 = 0,                 # protective efficacy of two doses of RTS,S [N]
                  effv_3 = 0,                 # protective efficacy of three doses of RTS,S [N]
                  dv = 1,                      # duration of vaccine protection [N]
                  timei = 2018,
                  dRCD = 4,
                  nuTr = 14,                   # days of infectiosness after treatment ACT [N]
                  nuTrp = 7,                   # days of infectiosness after treatment ACT+primaquine [N]
                  amp = 0.7,                   # relative amplitude seasonality [N]
                  phi = 0.5,                   # phase angle seasonality [N]
                  epsilonh=0.23,                 # per bite probability of an infectious mosquito infecting a human
                  epsilonm=0.5,                  # per bite probability of an infectious human infecting a mosquito
                  b=365/3,                       # per mosquito rate of biting
                  deltam=365/14,                 
                  gammam=365/10,
                  covRCD0 = 0,
                  kRCD = 0.032,                
                  cRCD = 3.5,
                  muRCDw=4,
                  sdRCDw=4,
                  covMSAT0=0,
                  omega = 2,                   # average duration of immunity (years) [N]
                  nuC = 9,                     # days of symptoms in the absence of treatment [N]
                  nuA = 60,                    # days of asymptomatic microscopically detectable carriage [N]
                  nuU = 60,                    # days of asymptomatic microscopically undetectable carriage [N]
                  rhoa = 70,                   # relative infectivity of asymptomatic microscopically detectable carriers compared with clinical infections (%) [N]
                  rhou = 30,                   # relative infectivity of asymptomatic microscopically undetectable carriers compared with clinical infections (%) [N]
                  ps = 90,                     # % of all non-immune new infections that are clinical [N]
                  pr = 20,                     # % of all immune new infections that are clinical [N]
                  mu = 50,                      # life expectancy (years) [N]
                  param)
  
  
  
  # MODEL INITIAL CONDITIONS
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
             S_3 = 0, IC_3 = 0, IA_3 = 0, IU_3 = 0, R_3 = 0, Tr_3 = 0, Sm_3 = 0, Rm_3 = 0, tauRCD2=0
  )
  
  # set up a function to solve the model
  modGMS<-function(t, state, parameters) 
  {
    with(as.list(c(state, parameters)),
         {
           #convert %s to proportions
           covEDATi<-0.9*covEDATi/100
           covEDAT0<-0.9*covEDAT0/100
           covITNi<-covITNi/100
           covITN0<-covITN0/100
           effITN <- effITN/100
           covIRSi<-covIRSi/100
           covIRS0<-covIRS0/100
           effIRS <- effIRS/100
           covRCDi<-covRCDi/100
           covRCD0<-covRCD0/100
           RCDs<-RCDs/100
           RCDsensC<-RCDsensC/100
           RCDsensA<-RCDsensA/100
           RCDsensU<-RCDsensU/100
           covMSATi<-covMSATi/100
           covMSAT0<-covMSAT0/100
           MSATsensC<-MSATsensC/100
           MSATsensA<-MSATsensA/100
           MSATsensU<-MSATsensU/100
           
           clustRCDrad<-clustRCDrad/100
           clustRCDcoex<-clustRCDcoex/100
           cm_1<-cm_1/100
           cm_2<-cm_2/100
           cm_3<-cm_3/100
           cmda_1<-cmda_1/100
           cmda_2<-cmda_2/100
           cmda_3<-cmda_3/100
           effv_1<-effv_1/100
           effv_2<-effv_2/100
           effv_3<-effv_3/100
           rhoa<-rhoa/100
           rhou<-rhou/100
           ps<-ps/100
           pr<-pr/100
           eta<-eta/100
           # convert time scales
           dm<-dm/12
           tm_1 <- 2018+(tm_1/12) #new
           tm_2 <- 2018+(tm_2/12) #new
           tm_3 <- 2018+(tm_3/12) #new
           # convert durations to rates
           lossd<-365/lossd
           omega<-1/omega
           nuC<-365/nuC
           nuA<-365/nuA
           nuU<-365/nuU
           nuRCD<- 52/delayRCD
           mu<-1/mu
           nTr<-365/nuTr
           nTrp<-365/nuTrp
           dRCD<-52/dRCD
           # imported cases
           muC<-muC/1000
           muA<-muA/1000
           muU<-muU/1000
           
           # swtich on interventions
           covEDATi <- EDATon*covEDATi+(1-EDATon)*covEDAT0
           covITNi <- ITNon*covITNi+(1-ITNon)*covITN0
           covRCDi <- RCDon*covRCDi+(1-RCDon)*covRCD0
           covIRSi <- IRSon*covIRSi+(1-IRSon)*covIRS0
           
           sS <- S_0+S_1+S_2+S_3
           sR <- R_0+R_1+R_2+R_3
           sIC <- IC_0+IC_1+IC_2+IC_3
           sIA <- IA_0+IA_1+IA_2+IA_3
           sIU <- IU_0+IU_1+IU_2+IU_3
           sTr <- Tr_0+Tr_1+Tr_2+Tr_3
           sSm <- Sm_0+Sm_1+Sm_2+Sm_3
           sRm <- Rm_0+Rm_1+Rm_2+Rm_3
           
           # define variables
           P <- (sS+sR+sIC+sIA+sIU+sTr+sSm+sRm)
           #seas<-1+amp*cos(2*3.14159*(Y-phi))
           nu <- 1/((1/nuC)+(1/nuA)+(1/nuU))
           #beta<-seas*b*epsilonh*epsilonm*bh/((bh*epsilonh+deltam)*(gammam/(gammam+deltam)))
           beta<-b*epsilonh*epsilonm*bh/((bh*epsilonh+deltam)*(gammam/(gammam+deltam)))
           mu_out <- mu+muC+muA+muU
           
           timei<-timei-startyear
           
           wsiEDAT<-(1-(Y<=timei))*(Y<=(timei+EDATscale))*((Y-timei)/EDATscale)+1*(Y>=(timei+EDATscale))
           wsiITN<-(1-(Y<=timei))*(Y<=(timei+ITNscale))*((Y-timei)/ITNscale)+1*(Y>=(timei+ITNscale))
           wsiRCD<-(1-(Y<=timei))*(Y<=(timei+RCDscale))*((Y-timei)/RCDscale)+1*(Y>=(timei+RCDscale))
           wsiIRS<-(1-(Y<=timei))*(Y<=(timei+IRSscale))*((Y-timei)/IRSscale)+1*(Y>=(timei+IRSscale))
           wsiMSAT<-(1-(Y<=timei))*(Y<=(timei+MSATscale))*((Y-timei)/MSATscale)+1*(Y>=(timei+MSATscale))
           covEDAT<-(1-wsiEDAT)*covEDAT0+wsiEDAT*covEDATi
           covITN<-(1-wsiITN)*covITN0+wsiITN*covITNi
           covRCD<-(1-wsiRCD)*covRCD0+wsiRCD*covRCDi
           covIRS<-(1-wsiIRS)*covIRS0+wsiIRS*covIRSi
           covMSAT<-(1-wsiMSAT)*covMSAT0+wsiMSAT*covMSATi
           
           nuTr<- primon*((Y<timei)*nTr+(Y>timei)*nTrp)+(1-primon)*nTr
           lossd<-1/((1/lossd)-(1/nuTr))
           
           lam <- (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(sIC+sTr+rhoa*sIA+rhou*sIU)/P
           
           # vaccine effects
           
           v_1<- MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dv-startyear))*effv_1
           v_2<- MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dv-startyear))*effv_2
           v_3<- MDAon*(Y>(tm_3-startyear))*(Y<=(tm_3+dv-startyear))*effv_3
           
           lam_1 <- (1-v_1)*lam
           lam_2 <- (1-v_2)*lam
           lam_3 <- (1-v_3)*lam
           
           tau <- covEDAT
           
           fail <- ((Y+startyear)<2019)*(percfail2018/100)+((Y+startyear)>=2019)*((Y+startyear)<2020)*(percfail2019/100)+((Y+startyear)>=2020)*(percfail2020/100)
           
           
           # set up treatment rate for RCD
           incm<-ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA
           #           rateRCD<-((1-eta)*(1+(1-RCDcoex)*clustRCD)+eta*(1+RCDcoex*clustRCDcoex))*(effRCD/P)*incm*covRCD
           propRCD<-(1-RCDcoex)*(1-exp(-kRCD*(RCDrad-cRCD)))+RCDcoex*RCDs
           fRCD<-dnorm(delayRCD, mean = muRCDw, sd = sdRCDw, log = FALSE)/dnorm(muRCDw, mean = muRCDw, sd = sdRCDw, log = FALSE)
           avrad<-clustRCDrad*exp(-kRCD*RCDrad)
           avcoex<-clustRCDcoex*(1-RCDs)
           rateRCD<-RCDon*nuRCD*covRCD*incm*(propRCD+fRCD*((1-RCDcoex)*(1-eta)*avrad+RCDcoex*avcoex))
           #           tauRCD<-1/((1/rateRCD)+(1/nuTr))
           tauRCD<-rateRCD
           tauRCD2 <- propRCD #(propRCD+fRCD*((1-RCDcoex)*(1-eta)*avrad+RCDcoex*avcoex))
           
           
           # MDA and RTS,S rounds
           m_1<- MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dm-startyear))*(-log((1-cm_1))/dm) 
           m_2<- MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dm-startyear))*(-log((1-cm_2))/dm) 
           m_3<- MDAon*(Y>(tm_3-startyear))*(Y<=(tm_3+dm-startyear))*(-log((1-cm_3))/dm) 
           m_4<-0
           
           treat <- ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)
                     +m_1*cmda_1*(IC_0+IA_0+IU_0)
                     +m_2*cmda_2*(IC_1+IA_1+IU_1)
                     +m_3*cmda_3*(IC_2+IA_2+IU_2)
                     +tauRCD*(RCDsensC*sIC+RCDsensA*sIA+RCDsensU*sIU)
           )
           
           muC <- (1-MSATon*MSATsensC*covMSAT)*muC
           muA <- (1-MSATon*MSATsensA*covMSAT)*muA
           muU <- (1-MSATon*MSATsensU*covMSAT)*muU
           
           
           
           
           # rate of change
           dY <- 1
           
           #dCinc_det <- ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)+tauRCD*(RCDsensC*sIC+RCDsensA*sIA+RCDsensU*sIU))                             #3
           dCinc_det <- ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA                             #3
           dCinc_tot <- ps*lam*sS+pr*lam*sR+pr*lam*sIU+pr*lam*sIA                                                                                                 #4
           dS_0 <- mu*P-mu_out*S_0+omega*R_0-lam*S_0+lossd*Sm_0-m_1*S_0                                                                                           #5
           dIC_0 <- muC*P-mu_out*IC_0+ps*(1-tau)*lam*S_0+pr*(1-tau)*lam*R_0+pr*(1-tau)*lam*IU_0+pr*(1-tau)*lam*IA_0-nuC*IC_0-m_1*IC_0-RCDsensC*tauRCD*IC_0        #6 
           dIA_0 <- muA*P-mu_out*IA_0+(1-ps)*lam*S_0+(1-pr)*lam*R_0+(1-pr)*lam*IU_0-pr*lam*IA_0+nuC*IC_0-nuA*IA_0+fail*nuTr*Tr_0-m_1*IA_0-RCDsensA*tauRCD*IA_0    #7
           dIU_0 <- muU*P-mu_out*IU_0-lam*IU_0-nuU*IU_0+nuA*IA_0-m_1*IU_0-RCDsensU*tauRCD*IU_0                                                                    #8
           dR_0 <- -mu_out*R_0-omega*R_0-lam*R_0+nuU*IU_0+lossd*Rm_0-m_1*R_0                                                                                      #9
           dTr_0 <- -mu_out*Tr_0+ps*tau*lam*S_0+pr*tau*lam*R_0+pr*tau*lam*IU_0+pr*tau*lam*IA_0-nuTr*Tr_0-m_1*Tr_0+tauRCD*(RCDsensC*IC_0+RCDsensA*IA_0+RCDsensU*IU_0) #10
           dSm_0 <- -mu_out*Sm_0+omega*Rm_0-lossd*Sm_0-m_1*Sm_0                                                                                                   #11
           dRm_0 <- -mu_out*Rm_0-omega*Rm_0+(1-fail)*nuTr*Tr_0-lossd*Rm_0-m_1*Rm_0                                                                                #12
           
           
           dS_1 <- -mu_out*S_1+omega*R_1-lam_1*S_1+lossd*Sm_1+(1-cmda_1)*m_1*S_0-m_2*S_1                                                                          #13
           dIC_1 <- -mu_out*IC_1+ps*(1-tau)*lam_1*S_1+pr*(1-tau)*lam_1*R_1+pr*(1-tau)*lam_1*IU_1+pr*(1-tau)*lam_1*IA_1-nuC*IC_1+(1-cmda_1)*m_1*IC_0-m_2*IC_1      #14
           dIA_1 <- -mu_out*IA_1+(1-ps)*lam_1*S_1+(1-pr)*lam_1*R_1+(1-pr)*lam_1*IU_1-pr*lam_1*IA_1+nuC*IC_1-nuA*IA_1+fail*nuTr*Tr_1+(1-cmda_1)*m_1*IA_0-m_2*IA_1  #15
           dIU_1 <- -mu_out*IU_1-lam_1*IU_1-nuU*IU_1+nuA*IA_1+(1-cmda_1)*m_1*IU_0-m_2*IU_1                                                                        #16
           dR_1 <- -mu_out*R_1-omega*R_1-lam_1*R_1+nuU*IU_1 +lossd*Rm_1+(1-cmda_1)*m_1*R_0-m_2*R_1                                                                #17
           dTr_1 <- -mu_out*Tr_1+ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1-nuTr*Tr_1+m_1*(cmda_1*(IC_0+IA_0+IU_0)+Tr_0)-m_2*Tr_1      #18
           dSm_1 <- -mu_out*Sm_1+omega*Rm_1-lossd*Sm_1+m_1*(cmda_1*S_0+Sm_0)-m_2*Sm_1                                                                             #19
           dRm_1 <- -mu_out*Rm_1-omega*Rm_1+(1-fail)*nuTr*Tr_1-lossd*Rm_1+m_1*(cmda_1*R_0+Rm_0)-m_2*Rm_1                                                          #20
           
           dS_2 <- -mu_out*S_2+omega*R_2-lam_2*S_2+lossd*Sm_2+(1-cmda_2)*m_2*S_1-m_3*S_2                                                                          #21
           dIC_2 <- -mu_out*IC_2+ps*(1-tau)*lam_2*S_2+pr*(1-tau)*lam_2*R_2+pr*(1-tau)*lam_2*IU_2+pr*(1-tau)*lam_2*IA_2-nuC*IC_2+(1-cmda_2)*m_2*IC_1-m_3*IC_2      #22
           dIA_2 <- -mu_out*IA_2+(1-ps)*lam_2*S_2+(1-pr)*lam_2*R_2+(1-pr)*lam_2*IU_2-pr*lam_2*IA_2+nuC*IC_2-nuA*IA_2+fail*nuTr*Tr_2+(1-cmda_2)*m_2*IA_1-m_3*IA_2  #23
           dIU_2 <- -mu_out*IU_2-lam_2*IU_2-nuU*IU_2+nuA*IA_2+(1-cmda_2)*m_2*IU_1-m_3*IU_2                                                                        #24
           dR_2 <- -mu_out*R_2-omega*R_2-lam_2*R_2+nuU*IU_2 +lossd*Rm_2+(1-cmda_2)*m_2*R_1-m_3*R_2                                                                #25
           dTr_2 <- -mu_out*Tr_2+ps*tau*lam_2*S_2+pr*tau*lam_2*R_2+pr*tau*lam_2*IU_2+pr*tau*lam_2*IA_2-nuTr*Tr_2+m_2*(cmda_2*(IC_1+IA_1+IU_1)+Tr_1)-m_3*Tr_2      #26
           dSm_2 <- -mu_out*Sm_2+omega*Rm_2-lossd*Sm_2+m_2*(cmda_2*S_1+Sm_1)-m_3*Sm_2                                                                             #27
           dRm_2 <- -mu_out*Rm_2-omega*Rm_2+(1-fail)*nuTr*Tr_2-lossd*Rm_2+m_2*(cmda_2*R_1+Rm_1)-m_3*Rm_2                                                          #28
           
           dS_3 <- -mu_out*S_3+omega*R_3-lam_3*S_3+lossd*Sm_3+(1-cmda_3)*m_3*S_2-m_4*S_3                                                                          #29
           dIC_3 <- -mu_out*IC_3+ps*(1-tau)*lam_3*S_3+pr*(1-tau)*lam_3*R_3+pr*(1-tau)*lam_3*IU_3+pr*(1-tau)*lam_3*IA_3-nuC*IC_3+(1-cmda_3)*m_3*IC_2-m_4*IC_3      #30
           dIA_3 <- -mu_out*IA_3+(1-ps)*lam_3*S_3+(1-pr)*lam_3*R_3+(1-pr)*lam_3*IU_3-pr*lam_3*IA_3+nuC*IC_3-nuA*IA_3+fail*nuTr*Tr_3+(1-cmda_3)*m_3*IA_2-m_4*IA_3  #31
           dIU_3 <- -mu_out*IU_3-lam_3*IU_3-nuU*IU_3+nuA*IA_3+(1-cmda_3)*m_3*IU_2-m_4*IU_3                                                                        #32
           dR_3 <- -mu_out*R_3-omega*R_3-lam_3*R_3+nuU*IU_3 +lossd*Rm_3+(1-cmda_3)*m_3*R_2-m_4*R_3                                                                #33
           dTr_3 <- -mu_out*Tr_3+ps*tau*lam_3*S_3+pr*tau*lam_3*R_3+pr*tau*lam_3*IU_3+pr*tau*lam_3*IA_3-nuTr*Tr_3+m_3*(cmda_3*(IC_2+IA_2+IU_2)+Tr_2)-m_4*Tr_3      #34
           dSm_3 <- -mu_out*Sm_3+omega*Rm_3-lossd*Sm_3+m_3*(cmda_3*S_2+Sm_2)-m_4*Sm_3                                                                             #35
           dRm_3 <- -mu_out*Rm_3-omega*Rm_3+(1-fail)*nuTr*Tr_3-lossd*Rm_3+m_3*(cmda_3*R_2+Rm_2)-m_4*Rm_3                                                          #36
           
           # return the rate of change
           list(c(dY,dCinc_det,dCinc_tot, 
                  dS_0, dIC_0, dIA_0, dIU_0, dR_0, dTr_0, dSm_0, dRm_0, 
                  dS_1, dIC_1, dIA_1, dIU_1, dR_1, dTr_1, dSm_1, dRm_1,
                  dS_2, dIC_2, dIA_2, dIU_2, dR_2, dTr_2, dSm_2, dRm_2,
                  dS_3, dIC_3, dIA_3, dIU_3, dR_3, dTr_3, dSm_3, dRm_3, tauRCD2
           ))
         }
    ) 
    
  }
  
  out <- ode(y = state, times = times, func = modGMS, parms = parameters)
  
  # MODEL OUTPUTS
  ipop <- 5:35
  iinc_det <- 3
  iinc_tot <- 4
  iprev <- c(6,  7,  8, 10, 14, 15, 16, 18, 22, 23, 24, 26, 30, 31, 32, 34)
  
  # population
  times<-out[,1]+startyear
  pop<-rowSums(out[,ipop])
  
  
  # clinical incidence detected per 1000 per month
  tci_det <- out[,iinc_det]
  clinmonth_det <- tci_det
  clinmonth_det[1] <- 0
  clinmonth_det[2:length(times)] <- 1000*(tci_det[2:length(times)] - tci_det[1:(length(times)-1)])/pop[2:length(times)]
  
  # clinical incidence total per 1000 per month
  tci_tot <- out[,iinc_tot]
  clinmonth_tot <- tci_tot
  clinmonth_tot[1] <- 0
  clinmonth_tot[2:length(times)] <- 1000*(tci_tot[2:length(times)] - tci_tot[1:(length(times)-1)])/pop[2:length(times)]
  
  trcd <- out[,37]
  
  # % prevalence
  prevalence <- 100*rowSums(out[,iprev])/pop
  GMSout<-matrix(NA,nrow=length(times),ncol=5)
  GMSout[,1]<-times
  GMSout[,2]<-clinmonth_det
  GMSout[,3]<-clinmonth_tot
  GMSout[,4]<-prevalence
  GMSout[,5]<-trcd
  
  return(GMSout)
}


scenario_0<-c(EDATon = 0,
              ITNon = 0,
              RCDon = 0,
              RCDcoex = 0,
              IRSon = 0,
              MDAon = 0,
              primon = 0,
              MSATon = 0)

scenario_i<-c(EDATon = 0,
              ITNon = 0,
              RCDon = 1,
              RCDcoex = 0,
              IRSon = 0,
              MDAon = 0,
              primon = 0,
              MSATon = 0)


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
  RCDs = 5
)

API <- 2.5
# initial prevalence
initprevR <- 0.001*API

GMSout0 <- runGMS(initprevR, scenario_0,parametersR)
GMSouti <- runGMS(initprevR, scenario_i,parametersR)


incidence <- tail(GMSout0[,3],1) #total clinical malaria cases
prevalence <- tail(GMSout0[,4],1)

