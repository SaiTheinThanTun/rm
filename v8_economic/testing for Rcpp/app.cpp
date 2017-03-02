#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//parameter<-list(covEADATi=asdfasfd,VA=VA)


int comp(double a,double b){
  int out;
  if(a<=b){
    out = 1;
  }else{
    out = 0;
  }
  return(out);
}

// [[Rcpp::export]]
List  modGMScpp(double t, List state, List parameters) 
{
  
  
  //#convert %s to proportions
  int covEDATi=0.9*parameter["covEDATi"]/100;
  covEDAT0=0.9*covEDAT0/100
  covITNi=covITNi/100
  covITN0=covITN0/100
  effITN = effITN/100
  covIRSi=covIRSi/100
  covIRS0=covIRS0/100
  effIRS = effIRS/100
  covRCDi=covRCDi/100
  covRCD0=covRCD0/100
  RCDs=RCDs/100
  RCDsensC=RCDsensC/100
  RCDsensA=RCDsensA/100
  RCDsensU=RCDsensU/100
  covMSATi=covMSATi/100
  covMSAT0=covMSAT0/100
  MSATsensC=MSATsensC/100
  MSATsensA=MSATsensA/100
  MSATsensU=MSATsensU/100
  
  clustRCDrad=clustRCDrad/100
  clustRCDcoex=clustRCDcoex/100
  cm_1=cm_1/100
  cm_2=cm_2/100
  cm_3=cm_3/100
  cmda_1=cmda_1/100
  cmda_2=cmda_2/100
  cmda_3=cmda_3/100
  effv_1=effv_1/100
  effv_2=effv_2/100
  effv_3=effv_3/100
  rhoa=rhoa/100
  rhou=rhou/100
  ps=ps/100
  pr=pr/100
  eta=eta/100
  # convert time scales
  dm=dm/12
  tm_1 = 2018+(tm_1/12) #new
  tm_2 = 2018+(tm_2/12) #new
  tm_3 = 2018+(tm_3/12) #new
  # convert durations to rates
  lossd=365/lossd
  omega=1/omega
  nuC=365/nuC
  nuA=365/nuA
  nuU=365/nuU
  # nuRCD= 52/delayRCD
  mu=1/mu
  nTr=365/nuTr
  nTrp=365/nuTrp
  dRCD=52/dRCD
  # imported cases
  muC=muC/1000
  muA=muA/1000
  muU=muU/1000
  
  # swtich on interventions
  covEDATi = EDATon*covEDATi+(1-EDATon)*covEDAT0
  covITNi = ITNon*covITNi+(1-ITNon)*covITN0
  covRCDi = RCDon*covRCDi+(1-RCDon)*covRCD0
  covIRSi = IRSon*covIRSi+(1-IRSon)*covIRS0
  
  sS = S_0+S_1+S_2+S_3
  sR = R_0+R_1+R_2+R_3
  sIC = IC_0+IC_1+IC_2+IC_3
  sIA = IA_0+IA_1+IA_2+IA_3
  sIU = IU_0+IU_1+IU_2+IU_3
  sTr = Tr_0+Tr_1+Tr_2+Tr_3
  sSm = Sm_0+Sm_1+Sm_2+Sm_3
  sRm = Rm_0+Rm_1+Rm_2+Rm_3
  
  # define variables
  P = (sS+sR+sIC+sIA+sIU+sTr+sSm+sRm)
  seas=1+amp*cos(2*3.14159*(Y-phi))
  nu = 1/((1/nuC)+(1/nuA)+(1/nuU))
  beta=seas*b*epsilonh*epsilonm*bh/((bh*epsilonh+deltam)*(gammam/(gammam+deltam)))
  mu_out = mu+muC+muA+muU
  
  timei=timei-startyear
  
  wsiEDAT=(1-comp(Y,timei))*comp(Y,(timei+EDATscale))*((Y-timei)/EDATscale)+1*(Y>=(timei+EDATscale))
  wsiITN=(1-(Y<=timei))*(Y<=(timei+ITNscale))*((Y-timei)/ITNscale)+1*(Y>=(timei+ITNscale))
  wsiRCD=(1-(Y<=timei))*(Y<=(timei+RCDscale))*((Y-timei)/RCDscale)+1*(Y>=(timei+RCDscale))
  wsiIRS=(1-(Y<=timei))*(Y<=(timei+IRSscale))*((Y-timei)/IRSscale)+1*(Y>=(timei+IRSscale))
  wsiMSAT=(1-(Y<=timei))*(Y<=(timei+MSATscale))*((Y-timei)/MSATscale)+1*(Y>=(timei+MSATscale))
  covEDAT=(1-wsiEDAT)*covEDAT0+wsiEDAT*covEDATi
  covITN=(1-wsiITN)*covITN0+wsiITN*covITNi
  covRCD=(1-wsiRCD)*covRCD0+wsiRCD*covRCDi
  covIRS=(1-wsiIRS)*covIRS0+wsiIRS*covIRSi
  covMSAT=(1-wsiMSAT)*covMSAT0+wsiMSAT*covMSATi
  
  nuTr= primon*((Y<timei)*nTr+(Y>timei)*nTrp)+(1-primon)*nTr
  lossd=1/((1/lossd)-(1/nuTr))
  
  lam = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(sIC+sTr+rhoa*sIA+rhou*sIU)/P
  
  # vaccine effects
  
  v_1= MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dv-startyear))*effv_1
  v_2= MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dv-startyear))*effv_2
  v_3= MDAon*(Y>(tm_3-startyear))*(Y<=(tm_3+dv-startyear))*effv_3
  
  lam_1 = (1-v_1)*lam
  lam_2 = (1-v_2)*lam
  lam_3 = (1-v_3)*lam
  
  tau = covEDAT
  
  fail = ((Y+startyear)<2019)*(percfail2018/100)+((Y+startyear)>=2019)*((Y+startyear)<2020)*(percfail2019/100)+((Y+startyear)>=2020)*(percfail2020/100)
  
  
  # set up treatment rate for RCD
  incm=ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA
  propRCD=(1-RCDcoex)*((1+exp(-kRCD*cRCD))*((1/(1+exp(-kRCD*(RCDrad-cRCD))))-(1/(1+exp(kRCD*cRCD)))))+RCDcoex*RCDs
  fRCD=dnorm(delayRCD, mean = muRCDw, sd = sdRCDw, log = FALSE)/dnorm(muRCDw, mean = muRCDw, sd = sdRCDw, log = FALSE)
  avrad=clustRCDrad/(1+exp(-bRCD*(gRCD-RCDrad)))
  eqRCDrad=cRCD-((1/kRCD)*log(((1+exp(kRCD*cRCD))/(1+RCDs*exp(kRCD*cRCD)))-1))
  avcoex=clustRCDcoex/(1+exp(-bRCD*(gRCD-eqRCDrad)))
  rateRCD=RCDon*covRCD*incm*(propRCD+fRCD*((1-RCDcoex)*(1-eta)*avrad+RCDcoex*avcoex))
  tauRCD=rateRCD
  
  
  # MDA and RTS,S rounds
  m_1= MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dm-startyear))*(-log((1-cm_1))/dm) 
  m_2= MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dm-startyear))*(-log((1-cm_2))/dm) 
  m_3= MDAon*(Y>(tm_3-startyear))*(Y<=(tm_3+dm-startyear))*(-log((1-cm_3))/dm) 
  m_4=0
  
  treat = ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)
  +m_1*cmda_1*(IC_0+IA_0+IU_0)
  +m_2*cmda_2*(IC_1+IA_1+IU_1)
  +m_3*cmda_3*(IC_2+IA_2+IU_2)
  +tauRCD*(RCDsensC*sIC+RCDsensA*sIA+RCDsensU*sIU)
  )
  
  muC = (1-MSATon*MSATsensC*covMSAT)*muC
  muA = (1-MSATon*MSATsensA*covMSAT)*muA
  muU = (1-MSATon*MSATsensU*covMSAT)*muU
  
  
  
  
  # rate of change
  dY = 1
  
  #dCinc_det = ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)+tauRCD*(RCDsensC*sIC+RCDsensA*sIA+RCDsensU*sIU))                             #3
  dCinc_det = ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA                             #3
  dCinc_tot = ps*lam*sS+pr*lam*sR+pr*lam*sIU+pr*lam*sIA                                                                                                 #4
  dS_0 = mu*P-mu_out*S_0+omega*R_0-lam*S_0+lossd*Sm_0-m_1*S_0                                                                                           #5
  dIC_0 = muC*P-mu_out*IC_0+ps*(1-tau)*lam*S_0+pr*(1-tau)*lam*R_0+pr*(1-tau)*lam*IU_0+pr*(1-tau)*lam*IA_0-nuC*IC_0-m_1*IC_0-RCDsensC*tauRCD*IC_0        #6 
  dIA_0 = muA*P-mu_out*IA_0+(1-ps)*lam*S_0+(1-pr)*lam*R_0+(1-pr)*lam*IU_0-pr*lam*IA_0+nuC*IC_0-nuA*IA_0+fail*nuTr*Tr_0-m_1*IA_0-RCDsensA*tauRCD*IA_0    #7
  dIU_0 = muU*P-mu_out*IU_0-lam*IU_0-nuU*IU_0+nuA*IA_0-m_1*IU_0-RCDsensU*tauRCD*IU_0                                                                    #8
  dR_0 = -mu_out*R_0-omega*R_0-lam*R_0+nuU*IU_0+lossd*Rm_0-m_1*R_0                                                                                      #9
  dTr_0 = -mu_out*Tr_0+ps*tau*lam*S_0+pr*tau*lam*R_0+pr*tau*lam*IU_0+pr*tau*lam*IA_0-nuTr*Tr_0-m_1*Tr_0+tauRCD*(RCDsensC*IC_0+RCDsensA*IA_0+RCDsensU*IU_0) #10
  dSm_0 = -mu_out*Sm_0+omega*Rm_0-lossd*Sm_0-m_1*Sm_0                                                                                                   #11
  dRm_0 = -mu_out*Rm_0-omega*Rm_0+(1-fail)*nuTr*Tr_0-lossd*Rm_0-m_1*Rm_0                                                                                #12
  
  
  dS_1 = -mu_out*S_1+omega*R_1-lam_1*S_1+lossd*Sm_1+(1-cmda_1)*m_1*S_0-m_2*S_1                                                                          #13
  dIC_1 = -mu_out*IC_1+ps*(1-tau)*lam_1*S_1+pr*(1-tau)*lam_1*R_1+pr*(1-tau)*lam_1*IU_1+pr*(1-tau)*lam_1*IA_1-nuC*IC_1+(1-cmda_1)*m_1*IC_0-m_2*IC_1      #14
  dIA_1 = -mu_out*IA_1+(1-ps)*lam_1*S_1+(1-pr)*lam_1*R_1+(1-pr)*lam_1*IU_1-pr*lam_1*IA_1+nuC*IC_1-nuA*IA_1+fail*nuTr*Tr_1+(1-cmda_1)*m_1*IA_0-m_2*IA_1  #15
  dIU_1 = -mu_out*IU_1-lam_1*IU_1-nuU*IU_1+nuA*IA_1+(1-cmda_1)*m_1*IU_0-m_2*IU_1                                                                        #16
  dR_1 = -mu_out*R_1-omega*R_1-lam_1*R_1+nuU*IU_1 +lossd*Rm_1+(1-cmda_1)*m_1*R_0-m_2*R_1                                                                #17
  dTr_1 = -mu_out*Tr_1+ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1-nuTr*Tr_1+m_1*(cmda_1*(IC_0+IA_0+IU_0)+Tr_0)-m_2*Tr_1      #18
  dSm_1 = -mu_out*Sm_1+omega*Rm_1-lossd*Sm_1+m_1*(cmda_1*S_0+Sm_0)-m_2*Sm_1                                                                             #19
  dRm_1 = -mu_out*Rm_1-omega*Rm_1+(1-fail)*nuTr*Tr_1-lossd*Rm_1+m_1*(cmda_1*R_0+Rm_0)-m_2*Rm_1                                                          #20
  
  dS_2 = -mu_out*S_2+omega*R_2-lam_2*S_2+lossd*Sm_2+(1-cmda_2)*m_2*S_1-m_3*S_2                                                                          #21
  dIC_2 = -mu_out*IC_2+ps*(1-tau)*lam_2*S_2+pr*(1-tau)*lam_2*R_2+pr*(1-tau)*lam_2*IU_2+pr*(1-tau)*lam_2*IA_2-nuC*IC_2+(1-cmda_2)*m_2*IC_1-m_3*IC_2      #22
  dIA_2 = -mu_out*IA_2+(1-ps)*lam_2*S_2+(1-pr)*lam_2*R_2+(1-pr)*lam_2*IU_2-pr*lam_2*IA_2+nuC*IC_2-nuA*IA_2+fail*nuTr*Tr_2+(1-cmda_2)*m_2*IA_1-m_3*IA_2  #23
  dIU_2 = -mu_out*IU_2-lam_2*IU_2-nuU*IU_2+nuA*IA_2+(1-cmda_2)*m_2*IU_1-m_3*IU_2                                                                        #24
  dR_2 = -mu_out*R_2-omega*R_2-lam_2*R_2+nuU*IU_2 +lossd*Rm_2+(1-cmda_2)*m_2*R_1-m_3*R_2                                                                #25
  dTr_2 = -mu_out*Tr_2+ps*tau*lam_2*S_2+pr*tau*lam_2*R_2+pr*tau*lam_2*IU_2+pr*tau*lam_2*IA_2-nuTr*Tr_2+m_2*(cmda_2*(IC_1+IA_1+IU_1)+Tr_1)-m_3*Tr_2      #26
  dSm_2 = -mu_out*Sm_2+omega*Rm_2-lossd*Sm_2+m_2*(cmda_2*S_1+Sm_1)-m_3*Sm_2                                                                             #27
  dRm_2 = -mu_out*Rm_2-omega*Rm_2+(1-fail)*nuTr*Tr_2-lossd*Rm_2+m_2*(cmda_2*R_1+Rm_1)-m_3*Rm_2                                                          #28
  
  dS_3 = -mu_out*S_3+omega*R_3-lam_3*S_3+lossd*Sm_3+(1-cmda_3)*m_3*S_2-m_4*S_3                                                                          #29
  dIC_3 = -mu_out*IC_3+ps*(1-tau)*lam_3*S_3+pr*(1-tau)*lam_3*R_3+pr*(1-tau)*lam_3*IU_3+pr*(1-tau)*lam_3*IA_3-nuC*IC_3+(1-cmda_3)*m_3*IC_2-m_4*IC_3      #30
  dIA_3 = -mu_out*IA_3+(1-ps)*lam_3*S_3+(1-pr)*lam_3*R_3+(1-pr)*lam_3*IU_3-pr*lam_3*IA_3+nuC*IC_3-nuA*IA_3+fail*nuTr*Tr_3+(1-cmda_3)*m_3*IA_2-m_4*IA_3  #31
  dIU_3 = -mu_out*IU_3-lam_3*IU_3-nuU*IU_3+nuA*IA_3+(1-cmda_3)*m_3*IU_2-m_4*IU_3                                                                        #32
  dR_3 = -mu_out*R_3-omega*R_3-lam_3*R_3+nuU*IU_3 +lossd*Rm_3+(1-cmda_3)*m_3*R_2-m_4*R_3                                                                #33
  dTr_3 = -mu_out*Tr_3+ps*tau*lam_3*S_3+pr*tau*lam_3*R_3+pr*tau*lam_3*IU_3+pr*tau*lam_3*IA_3-nuTr*Tr_3+m_3*(cmda_3*(IC_2+IA_2+IU_2)+Tr_2)-m_4*Tr_3      #34
  dSm_3 = -mu_out*Sm_3+omega*Rm_3-lossd*Sm_3+m_3*(cmda_3*S_2+Sm_2)-m_4*Sm_3                                                                             #35
  dRm_3 = -mu_out*Rm_3-omega*Rm_3+(1-fail)*nuTr*Tr_3-lossd*Rm_3+m_3*(cmda_3*R_2+Rm_2)-m_4*Rm_3                                                          #36
  
  //# return the rate of change
  
  List output;
  output["dY"]=dY;
  ,dCinc_det,dCinc_tot, 
  dS_0, dIC_0, dIA_0, dIU_0, dR_0, dTr_0, dSm_0, dRm_0, 
  dS_1, dIC_1, dIA_1, dIU_1, dR_1, dTr_1, dSm_1, dRm_1,
  dS_2, dIC_2, dIA_2, dIU_2, dR_2, dTr_2, dSm_2, dRm_2,
  dS_3, dIC_3, dIA_3, dIU_3, dR_3, dTr_3, dSm_3, dRm_3
  ))
  }
  ) 
  
}


'


fun = cxxfunction(
  signature(a = "numeric", b = "numeric"),
                   src, plugin = "Rcpp")

fun(1:3, 1:4)
