#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]


List modGMSrcpp(double t, NumericVector state, NumericVector parameters) 
{
  // switches
  int EDATon = parameters["EDATon"];
  int ITNon = parameters["ITNon"];
  int RCDon = parameters["RCDon"];
  int RCDcoex = parameters["RCDcoex"];
  int IRSon = parameters["IRSon"];
  int MDAon = parameters["MDAon"];
  int primon = parameters["primon"];
  int MSATon = parameters["MSATon"];
  // convert %s to proportions
  int covEDATi=0.9*parameters["covEDATi"]/100;
  int covEDAT0=0.9*parameters["covEDAT0"]/100;
  int covITNi=parameters["covITNi"]/100;
  int covITN0=parameters["covITN0"]/100;
  int effITN = parameters["effITN"]/100;
  int covIRSi=parameters["covIRSi"]/100;
  int covIRS0=parameters["covIRS0"]/100;
  int effIRS = parameters["effIRS"]/100;
  int covRCDi=parameters["covRCDi"]/100;
  int covRCD0=parameters["covRCD0"]/100;
  int RCDs=parameters["RCDs"]/100;
  int RCDsensC=parameters["RCDsensC"]/100;
  int RCDsensA=parameters["RCDsensA"]/100;
  int RCDsensU=parameters["RCDsensU"]/100;
  int covMSATi=parameters["covMSATi"]/100;
  int covMSAT0=parameters["covMSAT0"]/100;
  int MSATsensC=parameters["MSATsensC"]/100;
  int MSATsensA=parameters["MSATsensA"]/100;
  int MSATsensU=parameters["MSATsensU"]/100;
  
  int clustRCDrad=parameters["clustRCDrad"]/100;
  int clustRCDcoex=parameters["clustRCDcoex"]/100;
  int cm_1=parameters["cm_1"]/100;
  int cm_2=parameters["cm_2"]/100;
  int cm_3=parameters["cm_3"]/100;
  int cmda_1=parameters["cmda_1"]/100;
  int cmda_2=parameters["cmda_2"]/100;
  int cmda_3=parameters["cmda_3"]/100;
  int effv_1=parameters["effv_1"]/100;
  int effv_2=parameters["effv_2"]/100;
  int effv_3=parameters["effv_3"]/100;
  int rhoa=parameters["rhoa"]/100;
  int rhou=parameters["rhou"]/100;
  int ps=parameters["ps"]/100;
  int pr=parameters["pr"]/100;
  int eta=parameters["eta"]/100;
  // convert time scales
  int dm=parameters["dm"]/12;
  int tm_1 = 2018+(parameters["tm_1"]/12);
  int tm_2 = 2018+(parameters["tm_2"]/12);
  int tm_3 = 2018+(parameters["tm_3"]/12);
  // convert durations to rates
  int lossd=365/parameters["lossd"];
  int omega=1/parameters["omega"];
  int nuC=365/parameters["nuC"];
  int nuA=365/parameters["nuA"];
  int nuU=365/parameters["nuU"];
  
  int mu=1/parameters["mu"];
  int nTr=365/parameters["nuTr"];
  int nTrp=365/parameters["nuTrp"];
  //int dRCD=52/parameters["dRCD"]; //unused
  // imported cases
  int muC=parameters["muC"]/1000;
  int muA=parameters["muA"]/1000;
  int muU=parameters["muU"]/1000;
  
  //remaining variables
  int dv = parameters["dv"];
  int timei = parameters["timei"];
  int amp = parameters["amp"];
  int phi =  parameters["phi"];
  int epsilonh = parameters["epsilonh"];
  int epsilonm = parameters["epsilonm"];
  int b = parameters["b"];
  int deltam=parameters["deltam"];                 
  int gammam=parameters["gammam"];
  int kRCD = parameters["kRCD"];                
  int cRCD = parameters["cRCD"];                
  int bRCD = parameters["bRCD"];
  int gRCD = parameters["gRCD"];
  int muRCDw=parameters["muRCDw"];
  int sdRCDw=parameters["sdRCDw"];
  int percfail2018 = parameters["percfail2018"];
  int percfail2019 = parameters["percfail2019"];
  int percfail2020 = parameters["percfail2020"];
  int EDATscale =parameters["EDATscale"];
  int ITNscale=parameters["ITNscale"] ;
  int RCDscale = parameters["RCDscale"];
  int delayRCD =parameters["delayRCD"];
  int IRSscale =parameters["IRSscale"];
  int MSATscale=parameters["MSATscale"];
  int RCDrad =parameters["RCDrad"];
  int bh_max =parameters["bh_max"];
  
  int startyear=2007;
  
  //state
  int Y = state["Y"];
  //int Cinc_det = state["Cinc_det"]; //unused
  //int Cinc_tot = state["Cinc_tot"]; //unused
  int S_0 = state["S_0"];
  int IC_0 = state["IC_0"];
  int IA_0 = state["IA_0"];
  int IU_0 = state["IU_0"];
  int R_0 = state["R_0"];
  int Tr_0 = state["Tr_0"];
  int Sm_0 = state["Sm_0"];
  int Rm_0 = state["Rm_0"];
  int S_1 = state["S_1"];
  int IC_1 = state["IC_1"];
  int IA_1 = state["IA_1"];
  int IU_1 = state["IU_1"];
  int R_1 = state["R_1"];
  int Tr_1 = state["Tr_1"];
  int Sm_1 = state["Sm_1"];
  int Rm_1 = state["Rm_1"];
  int S_2 = state["S_2"];
  int IC_2 = state["IC_2"];
  int IA_2 = state["IA_2"];
  int IU_2 = state["IU_2"];
  int R_2 = state["R_2"];
  int Tr_2 = state["Tr_2"];
  int Sm_2 = state["Sm_2"];
  int Rm_2 = state["Rm_2"];
    int S_3 = state["S_3"];
  int IC_3 = state["IC_3"];
  int IA_3 = state["IA_3"];
  int IU_3 = state["IU_3"];
  int R_3 = state["R_3"];
  int Tr_3 = state["Tr_3"];
  int Sm_3 = state["Sm_3"];
  int Rm_3 = state["Rm_3"];
  
  
  
  
  // swtich on interventions
  covEDATi = EDATon*covEDATi+(1-EDATon)*covEDAT0;
  covITNi = ITNon*covITNi+(1-ITNon)*covITN0;
  covRCDi = RCDon*covRCDi+(1-RCDon)*covRCD0;
  covIRSi = IRSon*covIRSi+(1-IRSon)*covIRS0;
  
  int sS = S_0+S_1+S_2+S_3;
  int sR = R_0+R_1+R_2+R_3;
  int sIC = IC_0+IC_1+IC_2+IC_3;
  int sIA = IA_0+IA_1+IA_2+IA_3;
  int sIU = IU_0+IU_1+IU_2+IU_3;
  int sTr = Tr_0+Tr_1+Tr_2+Tr_3;
  int sSm = Sm_0+Sm_1+Sm_2+Sm_3;
  int sRm = Rm_0+Rm_1+Rm_2+Rm_3;
  
  // define variables
  int P = (sS+sR+sIC+sIA+sIU+sTr+sSm+sRm);
  int seas=1+amp*cos(2*3.14159*(Y-phi));
  //int nu = 1/((1/nuC)+(1/nuA)+(1/nuU)); //unused
  int bh=bh_max/(1+amp);
  int beta=seas*b*epsilonh*epsilonm*bh/((bh*epsilonh+deltam)*(gammam/(gammam+deltam)));
  int mu_out = mu+muC+muA+muU;
  
  timei=timei-startyear;
  
  int wsiEDAT=(1-(Y<=timei))*(Y<=(timei+EDATscale))*((Y-timei)/EDATscale)+1*(Y>=(timei+EDATscale));
  int wsiITN=(1-(Y<=timei))*(Y<=(timei+ITNscale))*((Y-timei)/ITNscale)+1*(Y>=(timei+ITNscale));
  int wsiRCD=(1-(Y<=timei))*(Y<=(timei+RCDscale))*((Y-timei)/RCDscale)+1*(Y>=(timei+RCDscale));
  int wsiIRS=(1-(Y<=timei))*(Y<=(timei+IRSscale))*((Y-timei)/IRSscale)+1*(Y>=(timei+IRSscale));
  int wsiMSAT=(1-(Y<=timei))*(Y<=(timei+MSATscale))*((Y-timei)/MSATscale)+1*(Y>=(timei+MSATscale));
  int covEDAT=(1-wsiEDAT)*covEDAT0+wsiEDAT*covEDATi;
  int covITN=(1-wsiITN)*covITN0+wsiITN*covITNi;
  int covRCD=(1-wsiRCD)*covRCD0+wsiRCD*covRCDi;
  int covIRS=(1-wsiIRS)*covIRS0+wsiIRS*covIRSi;
  int covMSAT=(1-wsiMSAT)*covMSAT0+wsiMSAT*covMSATi;
  
  int nuTr= primon*((Y<timei)*nTr+(Y>timei)*nTrp)+(1-primon)*nTr;
  lossd=1/((1/lossd)-(1/nuTr));
  
  int lam = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(sIC+sTr+rhoa*sIA+rhou*sIU)/P;
  
  // vaccine effects
  
  int v_1= MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dv-startyear))*effv_1;
  int v_2= MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dv-startyear))*effv_2;
  int v_3= MDAon*(Y>(tm_3-startyear))*(Y<=(tm_3+dv-startyear))*effv_3;
  
  int lam_1 = (1-v_1)*lam;
  int lam_2 = (1-v_2)*lam;
  int lam_3 = (1-v_3)*lam;
  
  int tau = covEDAT;
  
  int fail = ((Y+startyear)<2019)*(percfail2018/100)+((Y+startyear)>=2019)*((Y+startyear)<2020)*(percfail2019/100)+((Y+startyear)>=2020)*(percfail2020/100);

  // set up treatment rate for RCD
  int incm=ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA;
  int propRCD=(1-RCDcoex)*((1+exp(-kRCD*cRCD))*((1/(1+exp(-kRCD*(RCDrad-cRCD))))-(1/(1+exp(kRCD*cRCD)))))+RCDcoex*RCDs;
  int fRCD=exp(-((delayRCD-muRCDw)^2)/(2*sdRCDw));
  int avrad=clustRCDrad/(1+exp(-bRCD*(gRCD-RCDrad)));
  int eqRCDrad=cRCD-((1/kRCD)*log(((1+exp(kRCD*cRCD))/(1+RCDs*exp(kRCD*cRCD)))-1));
  int avcoex=clustRCDcoex/(1+exp(-bRCD*(gRCD-eqRCDrad)));
  int rateRCD=RCDon*covRCD*incm*(propRCD+fRCD*((1-RCDcoex)*(1-eta)*avrad+RCDcoex*avcoex));
  int tauRCD=rateRCD;
  
  
  // MDA and RTS,S rounds
  int m_1= MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dm-startyear))*(-log((1-cm_1))/dm);
  int m_2= MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dm-startyear))*(-log((1-cm_2))/dm);
  int m_3= MDAon*(Y>(tm_3-startyear))*(Y<=(tm_3+dm-startyear))*(-log((1-cm_3))/dm); 
  int m_4=0;
  
  //int treat = ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)+m_1*cmda_1*(IC_0+IA_0+IU_0)+m_2*cmda_2*(IC_1+IA_1+IU_1)+m_3*cmda_3*(IC_2+IA_2+IU_2)+tauRCD*(RCDsensC*sIC+RCDsensA*sIA+RCDsensU*sIU));
    
  muC = (1-MSATon*MSATsensC*covMSAT)*muC;
  muA = (1-MSATon*MSATsensA*covMSAT)*muA;
  muU = (1-MSATon*MSATsensU*covMSAT)*muU;
  
  
  
  
  // rate of change
  int dY = 1;
  
  //dCinc_det = ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)+tauRCD*(RCDsensC*sIC+RCDsensA*sIA+RCDsensU*sIU));                             //3
  int dCinc_det = ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA;                           //3
  int dCinc_tot = ps*lam*sS+pr*lam*sR+pr*lam*sIU+pr*lam*sIA;                                                                                                 //4
  int dS_0 = mu*P-mu_out*S_0+omega*R_0-lam*S_0+lossd*Sm_0-m_1*S_0;                                                                                         //5
  int dIC_0 = muC*P-mu_out*IC_0+ps*(1-tau)*lam*S_0+pr*(1-tau)*lam*R_0+pr*(1-tau)*lam*IU_0+pr*(1-tau)*lam*IA_0-nuC*IC_0-m_1*IC_0-RCDsensC*tauRCD*IC_0;        //6 
  int dIA_0 = muA*P-mu_out*IA_0+(1-ps)*lam*S_0+(1-pr)*lam*R_0+(1-pr)*lam*IU_0-pr*lam*IA_0+nuC*IC_0-nuA*IA_0+fail*nuTr*Tr_0-m_1*IA_0-RCDsensA*tauRCD*IA_0;    //7
  int dIU_0 = muU*P-mu_out*IU_0-lam*IU_0-nuU*IU_0+nuA*IA_0-m_1*IU_0-RCDsensU*tauRCD*IU_0;                                                                    //8
  int dR_0 = -mu_out*R_0-omega*R_0-lam*R_0+nuU*IU_0+lossd*Rm_0-m_1*R_0;                                                                                      //9
  int dTr_0 = -mu_out*Tr_0+ps*tau*lam*S_0+pr*tau*lam*R_0+pr*tau*lam*IU_0+pr*tau*lam*IA_0-nuTr*Tr_0-m_1*Tr_0+tauRCD*(RCDsensC*IC_0+RCDsensA*IA_0+RCDsensU*IU_0); //10
  int dSm_0 = -mu_out*Sm_0+omega*Rm_0-lossd*Sm_0-m_1*Sm_0;                                                                                                   //11
  int dRm_0 = -mu_out*Rm_0-omega*Rm_0+(1-fail)*nuTr*Tr_0-lossd*Rm_0-m_1*Rm_0;                                                                                //12
  
  
  int dS_1 = -mu_out*S_1+omega*R_1-lam_1*S_1+lossd*Sm_1+(1-cmda_1)*m_1*S_0-m_2*S_1;                                                                          //13
  int dIC_1 = -mu_out*IC_1+ps*(1-tau)*lam_1*S_1+pr*(1-tau)*lam_1*R_1+pr*(1-tau)*lam_1*IU_1+pr*(1-tau)*lam_1*IA_1-nuC*IC_1+(1-cmda_1)*m_1*IC_0-m_2*IC_1;      //14
  int dIA_1 = -mu_out*IA_1+(1-ps)*lam_1*S_1+(1-pr)*lam_1*R_1+(1-pr)*lam_1*IU_1-pr*lam_1*IA_1+nuC*IC_1-nuA*IA_1+fail*nuTr*Tr_1+(1-cmda_1)*m_1*IA_0-m_2*IA_1;  //15
  int dIU_1 = -mu_out*IU_1-lam_1*IU_1-nuU*IU_1+nuA*IA_1+(1-cmda_1)*m_1*IU_0-m_2*IU_1;                                                                        //16
  int dR_1 = -mu_out*R_1-omega*R_1-lam_1*R_1+nuU*IU_1 +lossd*Rm_1+(1-cmda_1)*m_1*R_0-m_2*R_1;                                                                //17
  int dTr_1 = -mu_out*Tr_1+ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1-nuTr*Tr_1+m_1*(cmda_1*(IC_0+IA_0+IU_0)+Tr_0)-m_2*Tr_1;      //18
  int dSm_1 = -mu_out*Sm_1+omega*Rm_1-lossd*Sm_1+m_1*(cmda_1*S_0+Sm_0)-m_2*Sm_1;                                                                             //19
  int dRm_1 = -mu_out*Rm_1-omega*Rm_1+(1-fail)*nuTr*Tr_1-lossd*Rm_1+m_1*(cmda_1*R_0+Rm_0)-m_2*Rm_1;                                                          //20
  
  int dS_2 = -mu_out*S_2+omega*R_2-lam_2*S_2+lossd*Sm_2+(1-cmda_2)*m_2*S_1-m_3*S_2;                                                                          //21
  int dIC_2 = -mu_out*IC_2+ps*(1-tau)*lam_2*S_2+pr*(1-tau)*lam_2*R_2+pr*(1-tau)*lam_2*IU_2+pr*(1-tau)*lam_2*IA_2-nuC*IC_2+(1-cmda_2)*m_2*IC_1-m_3*IC_2;      //22
  int dIA_2 = -mu_out*IA_2+(1-ps)*lam_2*S_2+(1-pr)*lam_2*R_2+(1-pr)*lam_2*IU_2-pr*lam_2*IA_2+nuC*IC_2-nuA*IA_2+fail*nuTr*Tr_2+(1-cmda_2)*m_2*IA_1-m_3*IA_2;  //23
  int dIU_2 = -mu_out*IU_2-lam_2*IU_2-nuU*IU_2+nuA*IA_2+(1-cmda_2)*m_2*IU_1-m_3*IU_2;                                                                        //24
  int dR_2 = -mu_out*R_2-omega*R_2-lam_2*R_2+nuU*IU_2 +lossd*Rm_2+(1-cmda_2)*m_2*R_1-m_3*R_2;                                                                //25
  int dTr_2 = -mu_out*Tr_2+ps*tau*lam_2*S_2+pr*tau*lam_2*R_2+pr*tau*lam_2*IU_2+pr*tau*lam_2*IA_2-nuTr*Tr_2+m_2*(cmda_2*(IC_1+IA_1+IU_1)+Tr_1)-m_3*Tr_2;      //26
  int dSm_2 = -mu_out*Sm_2+omega*Rm_2-lossd*Sm_2+m_2*(cmda_2*S_1+Sm_1)-m_3*Sm_2;                                                                             //27
  int dRm_2 = -mu_out*Rm_2-omega*Rm_2+(1-fail)*nuTr*Tr_2-lossd*Rm_2+m_2*(cmda_2*R_1+Rm_1)-m_3*Rm_2;                                                          //28
  
  int dS_3 = -mu_out*S_3+omega*R_3-lam_3*S_3+lossd*Sm_3+(1-cmda_3)*m_3*S_2-m_4*S_3;                                                                          //29
  int dIC_3 = -mu_out*IC_3+ps*(1-tau)*lam_3*S_3+pr*(1-tau)*lam_3*R_3+pr*(1-tau)*lam_3*IU_3+pr*(1-tau)*lam_3*IA_3-nuC*IC_3+(1-cmda_3)*m_3*IC_2-m_4*IC_3;      //30
  int dIA_3 = -mu_out*IA_3+(1-ps)*lam_3*S_3+(1-pr)*lam_3*R_3+(1-pr)*lam_3*IU_3-pr*lam_3*IA_3+nuC*IC_3-nuA*IA_3+fail*nuTr*Tr_3+(1-cmda_3)*m_3*IA_2-m_4*IA_3;  //31
  int dIU_3 = -mu_out*IU_3-lam_3*IU_3-nuU*IU_3+nuA*IA_3+(1-cmda_3)*m_3*IU_2-m_4*IU_3;                                                                        //32
  int dR_3 = -mu_out*R_3-omega*R_3-lam_3*R_3+nuU*IU_3 +lossd*Rm_3+(1-cmda_3)*m_3*R_2-m_4*R_3;                                                                //33
  int dTr_3 = -mu_out*Tr_3+ps*tau*lam_3*S_3+pr*tau*lam_3*R_3+pr*tau*lam_3*IU_3+pr*tau*lam_3*IA_3-nuTr*Tr_3+m_3*(cmda_3*(IC_2+IA_2+IU_2)+Tr_2)-m_4*Tr_3;      //34
  int dSm_3 = -mu_out*Sm_3+omega*Rm_3-lossd*Sm_3+m_3*(cmda_3*S_2+Sm_2)-m_4*Sm_3;                                                                             //35
  int dRm_3 = -mu_out*Rm_3-omega*Rm_3+(1-fail)*nuTr*Tr_3-lossd*Rm_3+m_3*(cmda_3*R_2+Rm_2)-m_4*Rm_3;                                                          //36
  
  // return the rate of change
  List output(36);
  output["dY"]=dY;
  output["dCinc_det"]=dCinc_det;
  output["dCinc_tot"]=dCinc_tot;
  output["dS_0"]=dS_0;
  output["dIC_0"]=dIC_0;
  output["dIA_0"]=dIA_0;
  output["dIU_0"]=dIU_0;
  output["dR_0"]=dR_0;
  output["dTr_0"]=dTr_0;
  output["dSm_0"]=dSm_0;
  output["dRm_0"]=dRm_0;
  output["dS_1"]=dS_1;
  output["dIC_1"]=dIC_1;
  output["dIA_1"]=dIA_1;
  output["dIU_1"]=dIU_1;
  output["dR_1"]=dR_1;
  output["dTr_1"]=dTr_1;
  output["dSm_1"]=dSm_1;
  output["dSm_1"]=dSm_1;
  output["dRm_1"]=dRm_1;
  output["dS_2"]=dS_2;
  output["dIC_2"]=dIC_2;
  output["dIA_2"]=dIA_2;
  output["dIU_2"]=dIU_2;
  output["dR_2"]=dR_2;
  output["dTr_2"]=dTr_2;
  output["dSm_2"]=dSm_2;
  output["dRm_2"]=dRm_2;
  output["dS_3"]=dS_3;
  output["dIC_3"]=dIC_3;
  output["dIA_3"]=dIA_3;
  output["dIU_3"]=dIU_3;
  output["dR_3"]=dR_3;
  output["dTr_3"]=dTr_3;
  output["dSm_3"]=dSm_3;
  output["dRm_3"]=dRm_3;

  return output;
    
}

//out = ode(y = state, times = times, func = modGMS, parms = parameters)