#TEMPLATE FOR ODE FUNCTIONS LAO/RAI ModelS
#shiny version of 
library(deSolve)
library(shiny)
library(TSA)


ui <- fluidPage(
  tabsetPanel(
    id="panels",
    tabPanel(title = strong("Typology Parameters"),
             column(3,
                    sliderInput(inputId="API", label = "baseline API", value = 2.5, min=1, max=100,step=.5),
                    sliderInput(inputId="bh", label = "number of mosquito bites per human per day (average)", value = 4, min=0, max=30,step=1),
                    sliderInput(inputId="eta", label = "% of all infections that are caught outside the village (forest)", value = 50, min=0, max=100),
                    sliderInput(inputId="covEDAT0", label = "baseline % of all clinical cases treated", value = 30, min=0, max=100)
             ),
             column(3,
                    #sliderInput(inputId="nuTr", label = "days of infectiosness after treatment ACT", value = 14, min=0, max=30), #remove
                    #sliderInput(inputId="nuTrp", label = "days of infectiosness after treatment ACT+primaquine", value = 7, min=0, max=30), #remove
                    sliderInput(inputId="covITN0", label = "baseline coverage of ITN (%) ", value = 75, min=0, max=90,step=.5),
                    sliderInput(inputId="covRCD0", label = "baseline coverage of RCD (%) ", value = 0, min=0, max=90),
                    sliderInput(inputId="covIRS0", label = "baseline coverage of IRS (%) ", value = 0, min=0, max=90)
             ),
             column(3,
                    #sliderInput(inputId="amp", label = "relative amplitude seasonality ", value = 0.7, min=0, max=2), #remove
                    #sliderInput(inputId="phi", label = "phase angle seasonality ", value = 0.5, min=0, max=1), #remove
                    sliderInput(inputId="muC", label = "imported clinical cases per 1000 population per year ", value = 1, min=0, max=10),
                    sliderInput(inputId="muA", label = "imported super-microscopic asymtomatic infection per 1000 population per year ", value = 10, min=0, max=100),
                    sliderInput(inputId="muU", label = "imported sub-microscopic asymtomatic infections per 1000 population per year ", value = 10, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="percfail2018", label = "% of cases failing treatment in 2018 and before ", value = 30, min=0, max=100),
                    sliderInput(inputId="percfail2019", label = "% of cases failing treatment in 2019  ", value = 10, min=0, max=100),
                    sliderInput(inputId="percfail2020", label = "% of cases failing treatment in 2020 and after  ", value = 20, min=0, max=100)
             )
    ),
    
    tabPanel(title = strong("Process Indicators"),
             column(3,
                    #sliderInput(inputId="timei", label = "timing of intervention ", value = 2018, min=2017, max=2020),
                    checkboxInput(inputId="EDATon", label = "switch on scale up of EDAT ", value = FALSE),
                    checkboxInput(inputId="primon", label = "ACT+primaquine for EDAT and MDA ", value = FALSE), #under EDAT checkbox
                    sliderInput(inputId="EDATscale", label = "years to scale up EDAT ", value = 1, min=.25, max=3, step=.25),
                    sliderInput(inputId="covEDATi", label = "new % of all clinical cases treated", value = 90, min=0, max=100),
                    #hr(),
                    checkboxInput(inputId="ITNon", label = "switch on scale up of ITN ", value = FALSE),
                    sliderInput(inputId="ITNscale", label = "years to scale up ITN ", value = 0.5, min=.25, max=3, step=.25),
                    sliderInput(inputId="covITNi", label = "new coverage of ITN (%) ", value = 90, min=0, max=90),
                    sliderInput(inputId="effITN", label = "new % of infections averted due to owenership of ITN ", value = 30, min=0, max=50)
                    
             ),
             column(3,
                    checkboxInput(inputId="RCDon", label = "switch on scale up of RCD", value = FALSE),
                    sliderInput(inputId="RCDscale", label = "years to scale up RCD ", value = 2, min=.25, max=3, step=.25), #.25 timesteps
                    sliderInput(inputId="covRCDi", label = "new coverage of RCD (%)", value = 50, min=0, max=100),
                    sliderInput(inputId="effRCD", label = "no. people screened per clinical case", value = 20, min=0, max=1000),
                    sliderInput(inputId="RCDsensC", label = "sensitivity RCD test (clinical) ", value = 95, min=0, max=100),
                    sliderInput(inputId="RCDsensA", label = "sensitivity RCD test (super-micro, asym)", value = 60, min=0, max=100),
                    sliderInput(inputId="RCDsensU", label = "sensitivity RCD test (sub-micro, asym)", value = 0, min=0, max=100)
             ),
             column(3,
                   sliderInput(inputId="clustRCD", label = "% increased likelihood of finding cases with radial search", value = 20, min=0, max=100),
                    checkboxInput(inputId="RCDcoex", label = "change RCD from radial serch to co-exposure search   ", value = FALSE),
                    sliderInput(inputId="clustRCDcoex", label = "% increased likelihood of finding cases with coexposure search", value = 90, min=0, max=100)
                    
             ),
             column(3,
                    checkboxInput(inputId="IRSon", label = "switch on scale up of IRS ", value = FALSE),
                    sliderInput(inputId="IRSscale", label = "years to scale up IRS ", value = 1, min=.25, max=3, step=.25),
                    sliderInput(inputId="covIRSi", label = "new coverage of IRS (%) ", value = 90, min=0, max=90),
                    sliderInput(inputId="effIRS", label = "% reduction in biting rate due to IRS ", value = 15, min=0, max=25)
             )
    ),
    tabPanel(title = strong("Focal MDA Indicators"),
             column(3,
                    checkboxInput(inputId="MDAon", label = "switch on MDA", value = FALSE), #6
                    sliderInput(inputId="cmda_1", label = "effective population coverage of focal MDA in round 1 ", value = 50, min=0, max=100),
                    sliderInput(inputId="cmda_2", label = "effective population coverage of focal MDA in round 2 ", value = 50, min=0, max=100),
                    sliderInput(inputId="cmda_3", label = "effective population coverage of focal MDA in round 3 ", value = 50, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="tm_1", label = "timing of 1st round [2018+ no. of month, 1 means Jan'2018, 13 means Jan'2019]", value = 1, min=1, max=36),
                    sliderInput(inputId="tm_2", label = "timing of 2nd round [2018+ no. of month]", value = 2, min=2, max=36),
                    sliderInput(inputId="tm_3", label = "timing of 3rd round [2018+ no. of month]", value = 3, min=3, max=36),
                    sliderInput(inputId="dm", label = "months to complete each round ", value = 6, min=1, max=24)
             ),
             column(3,
                    sliderInput(inputId="lossd", label = "days prophylaxis provided by the ACT", value = 30, min=7, max=30),
                    sliderInput(inputId="cm_1", label = "% population coverage of 1st MDA round", value = 80, min=0, max=100),
                    sliderInput(inputId="cm_2", label = "% of 1st MDA round population to get 2nd", value = 95, min=0, max=100),
                    sliderInput(inputId="cm_3", label = "% of 2nd MDA round population to get 3rd", value = 95, min=0, max=100)
             )
    ),
    tabPanel(title = strong("Imported malaria MSAT indicators"),
             column(3,
                    checkboxInput(inputId="MSATon", label = "switch on MSAT for imported cases", value = FALSE),
                    sliderInput(inputId="MSATscale", label = "years to scale up MSAT ", value = 2, min=.25, max=3, step=.25), 
                    sliderInput(inputId="covMSATi", label = "new coverage of MSAT (%)", value = 80, min=0, max=100)
              ),
             column(3,
                    sliderInput(inputId="MSATsensC", label = "sensitivity MSAT test (clinical) ", value = 95, min=0, max=100),
                    sliderInput(inputId="MSATsensA", label = "sensitivity MSAT test (super-micro, asym)", value = 60, min=0, max=100),
                    sliderInput(inputId="MSATsensU", label = "sensitivity MSAT test (sub-micro, asym)", value = 0, min=0, max=100)
             )
    ),
    tabPanel(title = strong("Biological Parameters"), #not remove
             column(3,
                    sliderInput(inputId="omega", label = "average duration of immunity (years) ", value = 2, min=0, max=10),
                    sliderInput(inputId="nuC", label = "days of symptoms in the absence of treatment ", value = 9, min=1, max=30),
                    sliderInput(inputId="nuA", label = "days of super-microscopic asymtomatic infection ", value = 60, min=30, max=90)
             ),
             column(3,
                    sliderInput(inputId="nuU", label = "days of sub-microscopic asymtomatic infection ", value = 60, min=30, max=365),
                    sliderInput(inputId="rhoa", label = "relative infectivity of super-microscopic asymptomatic infections compared with clinical infections (%) ", value = 70, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="rhou", label = "relative infectivity of sub-microscopic asymptomatic infections compared with clinical infections (%) ", value = 30, min=0, max=100),
                    sliderInput(inputId="ps", label = "% of all non-immune new infections that are clinical ", value = 90, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="pr", label = "% of all immune new infections that are clinical ", value = 20, min=0, max=100),
                    sliderInput(inputId="mu", label = "life expectancy (years) ", value = 50, min=45, max=95)
             )
    )
  ),
  fluidRow(plotOutput(outputId = "MODEL")),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  hr(),
  fluidRow(h4("          Legend")),
  fluidRow(h4("          Grey solid line: baseline scenario. Blue solid line: elimination strategy scenario.")), 
  fluidRow(h4("          Dark blue solid line: target baseline API. Grey dashed lines: start and end of elimination activities.")),
  fluidRow(h4("          Red dashed line: pre-elimniation threshold (API = 1 per 1000 per year)"))
  
)
#non-reactive parameters
# define the number of weeks to run the model
dt<-1/12
startyear<-2010
stopyear<-2025
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)
tsteps<-length(times)

#non-reactive function
runGMS<-function(initprev, scenario,param) 
{
  #MODEL PARAMETERS
  parameters <- c(scenario,param)
  
  
  
  # MODEL INITIAL CONDITIONS
  # population size
  initP<-10000 
  
  initS_0<-0.5*(1-initprev)*initP
  initIC_0<-0
  initIA_0<-initprev*initP
  initIU_0<-0
  initR_0<-0.5*(1-initprev)*initP
  initTr_0<-0
  
  state <- c(Y = 0, Cinc = 0,  
             S_0 = initS_0, IC_0 = initIC_0, IA_0 = initIA_0, IU_0 = initIU_0, R_0 = initR_0, Tr_0 = initTr_0, Sm_0 = 0, Rm_0 = 0,
             S_1 = 0, IC_1 = 0, IA_1 = 0, IU_1 = 0, R_1 = 0, Tr_1 = 0, Sm_1 = 0, Rm_1 = 0,
             S_2 = 0, IC_2 = 0, IA_2 = 0, IU_2 = 0, R_2 = 0, Tr_2 = 0, Sm_2 = 0, Rm_2 = 0,
             S_3 = 0, IC_3 = 0, IA_3 = 0, IU_3 = 0, R_3 = 0, Tr_3 = 0, Sm_3 = 0, Rm_3 = 0
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
           RCDsensC<-RCDsensC/100
           RCDsensA<-RCDsensA/100
           RCDsensU<-RCDsensU/100
           covMSATi<-covMSATi/100
           covMSAT0<-covMSAT0/100
           MSATsensC<-MSATsensC/100
           MSATsensA<-MSATsensA/100
           MSATsensU<-MSATsensU/100
           
           clustRCD<-clustRCD/100
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
           # convert durations to rates
           lossd<-365/lossd
           omega<-1/omega
           nuC<-365/nuC
           nuA<-365/nuA
           nuU<-365/nuU
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
           seas<-1+amp*cos(2*3.14159*(Y-phi))
           nu <- 1/((1/nuC)+(1/nuA)+(1/nuU))
           # beta<-R0*(mu+nu)*seas
           beta<-seas*b*epsilonh*epsilonm*bh/((bh*epsilonh+deltam)*(gammam/(gammam+deltam)))
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
           
           nuTr<- primon*((Y<3)*nTr+(Y>3)*nTrp)+(1-primon)*nTr
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
           
           #fail <- (Y<2019)*(percfail2018/100)+(Y>=2019)*(Y<2020)*(percfail2019/100)+(Y>=2020)*(percfail2020/100)
           fail <- ((Y+startyear)<2019)*(percfail2018/100)+((Y+startyear)>=2019)*((Y+startyear)<2020)*(percfail2019/100)+((Y+startyear)>=2020)*(percfail2020/100)
           
           
           # set up treatment rate for RCD
           incm<-ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA
           #rateRCD<-((1-eta)*(1+(1-RCDcoex)*clustRCD)+eta*(1+RCDcoex*clustRCDcoex))*(effRCD/P)*incm*covRCD*dRCD
           rateRCD<-((1-eta)*(1+(1-RCDcoex)*clustRCD)+eta*(1+RCDcoex*clustRCDcoex))*(effRCD/P)*incm*covRCD
           tauRCD<-1/((1/rateRCD)+(1/nuTr))
           
           
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
           
           dCinc <-   treat                                                                                                                                       #3
           dS_0 <- mu*P-mu_out*S_0+omega*R_0-lam*S_0+lossd*Sm_0-m_1*S_0                                                                                           #4
           dIC_0 <- muC*P-mu_out*IC_0+ps*(1-tau)*lam*S_0+pr*(1-tau)*lam*R_0+pr*(1-tau)*lam*IU_0+pr*(1-tau)*lam*IA_0-nuC*IC_0-m_1*IC_0-RCDsensC*tauRCD*IC_0        #5 
           dIA_0 <- muA*P-mu_out*IA_0+(1-ps)*lam*S_0+(1-pr)*lam*R_0+(1-pr)*lam*IU_0-pr*lam*IA_0+nuC*IC_0-nuA*IA_0+fail*nuTr*Tr_0-m_1*IA_0-RCDsensA*tauRCD*IA_0    #6
           dIU_0 <- muU*P-mu_out*IU_0-lam*IU_0-nuU*IU_0+nuA*IA_0-m_1*IU_0-RCDsensU*tauRCD*IU_0                                                                    #7
           dR_0 <- -mu_out*R_0-omega*R_0-lam*R_0+nuU*IU_0+lossd*Rm_0-m_1*R_0                                                                                      #8
           dTr_0 <- -mu_out*Tr_0+ps*tau*lam*S_0+pr*tau*lam*R_0+pr*tau*lam*IU_0+pr*tau*lam*IA_0-nuTr*Tr_0-m_1*Tr_0+tauRCD*(RCDsensC*IC_0+RCDsensA*IA_0+RCDsensU*IU_0)                                                 #9
           dSm_0 <- -mu_out*Sm_0+omega*Rm_0-lossd*Sm_0-m_1*Sm_0                                                                                                   #10
           dRm_0 <- -mu_out*Rm_0-omega*Rm_0+(1-fail)*nuTr*Tr_0-lossd*Rm_0-m_1*Rm_0                                                                                #11
           
           
           dS_1 <- -mu_out*S_1+omega*R_1-lam_1*S_1+lossd*Sm_1+(1-cmda_1)*m_1*S_0-m_2*S_1                                                                          #12
           dIC_1 <- -mu_out*IC_1+ps*(1-tau)*lam_1*S_1+pr*(1-tau)*lam_1*R_1+pr*(1-tau)*lam_1*IU_1+pr*(1-tau)*lam_1*IA_1-nuC*IC_1+(1-cmda_1)*m_1*IC_0-m_2*IC_1      #13
           dIA_1 <- -mu_out*IA_1+(1-ps)*lam_1*S_1+(1-pr)*lam_1*R_1+(1-pr)*lam_1*IU_1-pr*lam_1*IA_1+nuC*IC_1-nuA*IA_1+fail*nuTr*Tr_1+(1-cmda_1)*m_1*IA_0-m_2*IA_1  #14
           dIU_1 <- -mu_out*IU_1-lam_1*IU_1-nuU*IU_1+nuA*IA_1+(1-cmda_1)*m_1*IU_0-m_2*IU_1                                                                        #15
           dR_1 <- -mu_out*R_1-omega*R_1-lam_1*R_1+nuU*IU_1 +lossd*Rm_1+(1-cmda_1)*m_1*R_0-m_2*R_1                                                                #16
           dTr_1 <- -mu_out*Tr_1+ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1-nuTr*Tr_1+m_1*(cmda_1*(IC_0+IA_0+IU_0)+Tr_0)-m_2*Tr_1      #17
           dSm_1 <- -mu_out*Sm_1+omega*Rm_1-lossd*Sm_1+m_1*(cmda_1*S_0+Sm_0)-m_2*Sm_1                                                                             #18
           dRm_1 <- -mu_out*Rm_1-omega*Rm_1+(1-fail)*nuTr*Tr_1-lossd*Rm_1+m_1*(cmda_1*R_0+Rm_0)-m_2*Rm_1                                                          #19
           
           dS_2 <- -mu_out*S_2+omega*R_2-lam_2*S_2+lossd*Sm_2+(1-cmda_2)*m_2*S_1-m_3*S_2                                                                          #20
           dIC_2 <- -mu_out*IC_2+ps*(1-tau)*lam_2*S_2+pr*(1-tau)*lam_2*R_2+pr*(1-tau)*lam_2*IU_2+pr*(1-tau)*lam_2*IA_2-nuC*IC_2+(1-cmda_2)*m_2*IC_1-m_3*IC_2      #21
           dIA_2 <- -mu_out*IA_2+(1-ps)*lam_2*S_2+(1-pr)*lam_2*R_2+(1-pr)*lam_2*IU_2-pr*lam_2*IA_2+nuC*IC_2-nuA*IA_2+fail*nuTr*Tr_2+(1-cmda_2)*m_2*IA_1-m_3*IA_2  #22
           dIU_2 <- -mu_out*IU_2-lam_2*IU_2-nuU*IU_2+nuA*IA_2+(1-cmda_2)*m_2*IU_1-m_3*IU_2                                                                        #23
           dR_2 <- -mu_out*R_2-omega*R_2-lam_2*R_2+nuU*IU_2 +lossd*Rm_2+(1-cmda_2)*m_2*R_1-m_3*R_2                                                                #24
           dTr_2 <- -mu_out*Tr_2+ps*tau*lam_2*S_2+pr*tau*lam_2*R_2+pr*tau*lam_2*IU_2+pr*tau*lam_2*IA_2-nuTr*Tr_2+m_2*(cmda_2*(IC_1+IA_1+IU_1)+Tr_1)-m_3*Tr_2      #25
           dSm_2 <- -mu_out*Sm_2+omega*Rm_2-lossd*Sm_2+m_2*(cmda_2*S_1+Sm_1)-m_3*Sm_2                                                                             #26
           dRm_2 <- -mu_out*Rm_2-omega*Rm_2+(1-fail)*nuTr*Tr_2-lossd*Rm_2+m_2*(cmda_2*R_1+Rm_1)-m_3*Rm_2                                                          #27
           
           dS_3 <- -mu_out*S_3+omega*R_3-lam_3*S_3+lossd*Sm_3+(1-cmda_3)*m_3*S_2-m_4*S_3                                                                          #28
           dIC_3 <- -mu_out*IC_3+ps*(1-tau)*lam_3*S_3+pr*(1-tau)*lam_3*R_3+pr*(1-tau)*lam_3*IU_3+pr*(1-tau)*lam_3*IA_3-nuC*IC_3+(1-cmda_3)*m_3*IC_2-m_4*IC_3      #29
           dIA_3 <- -mu_out*IA_3+(1-ps)*lam_3*S_3+(1-pr)*lam_3*R_3+(1-pr)*lam_3*IU_3-pr*lam_3*IA_3+nuC*IC_3-nuA*IA_3+fail*nuTr*Tr_3+(1-cmda_3)*m_3*IA_2-m_4*IA_3  #30
           dIU_3 <- -mu_out*IU_3-lam_3*IU_3-nuU*IU_3+nuA*IA_3+(1-cmda_3)*m_3*IU_2-m_4*IU_3                                                                        #31
           dR_3 <- -mu_out*R_3-omega*R_3-lam_3*R_3+nuU*IU_3 +lossd*Rm_3+(1-cmda_3)*m_3*R_2-m_4*R_3                                                                #32
           dTr_3 <- -mu_out*Tr_3+ps*tau*lam_3*S_3+pr*tau*lam_3*R_3+pr*tau*lam_3*IU_3+pr*tau*lam_3*IA_3-nuTr*Tr_3+m_3*(cmda_3*(IC_2+IA_2+IU_2)+Tr_2)-m_4*Tr_3      #33
           dSm_3 <- -mu_out*Sm_3+omega*Rm_3-lossd*Sm_3+m_3*(cmda_3*S_2+Sm_2)-m_4*Sm_3                                                                             #34
           dRm_3 <- -mu_out*Rm_3-omega*Rm_3+(1-fail)*nuTr*Tr_3-lossd*Rm_3+m_3*(cmda_3*R_2+Rm_2)-m_4*Rm_3                                                          #35
           
           # return the rate of change
           list(c(dY,dCinc, 
                  dS_0, dIC_0, dIA_0, dIU_0, dR_0, dTr_0, dSm_0, dRm_0, 
                  dS_1, dIC_1, dIA_1, dIU_1, dR_1, dTr_1, dSm_1, dRm_1,
                  dS_2, dIC_2, dIA_2, dIU_2, dR_2, dTr_2, dSm_2, dRm_2,
                  dS_3, dIC_3, dIA_3, dIU_3, dR_3, dTr_3, dSm_3, dRm_3
           ))
         }
    ) 
    
  }
  
  out <- ode(y = state, times = times, func = modGMS, parms = parameters)
  
  # MODEL OUTPUTS
  ipop <- 4:35
  iinc <- 3
  iprev <- c(7,8,9,11,15,16,17,19,23,24,25,27,31,32,33,35)
  iprev <- c(5,6,7,9,13,14,15,17,21,22,23,25,29,30,31,33)
  
  # population
  times<-out[,1]+startyear
  pop<-rowSums(out[,ipop])
  
  
  # clinical incidence per 1000 per month
  tci <- out[,iinc]
  clinmonth <- tci
  clinmonth[1] <- 0
  clinmonth[2:length(times)] <- 1000*(tci[2:length(times)] - tci[1:(length(times)-1)])/pop[2:length(times)]
  # % prevalence
  prevalence <- 100*rowSums(out[,iprev])/pop
  
  GMSout<-matrix(NA,nrow=length(times),ncol=3)
  GMSout[,1]<-times
  GMSout[,2]<-clinmonth
  GMSout[,3]<-prevalence
  
  return(GMSout)
}

server <- function(input, output) {
  scenario_0<-c(EDATon = 0,
                ITNon = 0,
                RCDon = 0,
                RCDcoex = 0,
                IRSon = 0,
                MDAon = 0,
                primon = 0,
                MSATon = 0)
  scenario_iR<-reactive(c(EDATon = input$EDATon,
                          ITNon = input$ITNon,
                          RCDon = input$RCDon,
                          RCDcoex = input$RCDcoex,
                          IRSon = input$IRSon,
                          MDAon = input$MDAon,
                          primon = input$primon,
                          MSATon = input$MSATon))
  parametersR <- reactive(c(
    
    # ************** SAI ***********#
    # For shiny app, please see square brackets [a to b] for range of slider from a to b 
    # if [N] then don't include 
    # if [C] then check box
    # ************** SAI ***********#
    
    # process indicators
    # timei = 2018,                # timing of intervention [N] #remove
    #EDATon = scenario[1],        # switch on scale up of EDAT [C]
    # EDATscale = 3,               # years to scale up EDAT [1 to 3]
    # covEDATi = 90,               # new % of all villages covered by VMW [0 to 100]
    #ITNon = scenario[2],         # switch on scale up of ITN [C]
    # ITNscale = 0.5,              # years to scale up ITN [1 to 3]
    # covITNi = 90,                # new coverage of ITN (%) [0 to 90]
    # effITN = 30,                 # % of new infections averted due to owenership of ITN [0 to 50]
    #RCDon = scenario[3],         # switch on scale up of RCD default radial search   [C] 
    # RCDscale = 2,                # years to scale up RCD [1 to 3]
    #RCDcoex = scenario[4],       # Change RCD to co-exposure search   [C] 
    # RCDsensC = 95,               # Sensitivity of RCD test used for clinical infection [0 to 100]
    # RCDsensA = 60,               # Sensitivity of RCD test used for super-microscopic asymtomatic infection [0 to 100]
    # RCDsensU = 0,                # Sensitivity of RCD test used for sub-microscopic asymtomatic infection [0 to 100]
    # covRCDi = 50,                # new coverage of RCD (%) [0 to 100]
    # effRCD = 20,                 # number of people investigated per new clinical index case [0 to 1000] 
    # dRCD = 4,                    # number of weeks for each investigation [1 to 8] #remove
    # clustRCD = 20,               # % increased likelihood of finding cases with radial search given village transmission [0 to 100]
    # clustRCDcoex = 90,             # % increased likelihood of finding cases with coexposure search given outside-village transmission [0 to 100]
    #IRSon = scenario[5],         # switch on scale up of IRS [C]  
    # IRSscale = 1,                # years to scale up IRS [1 to 3]
    # covIRSi = 90,                # new coverage of IRS (%) [0 to 90]
    # effIRS = 15,                 # % reduction in risk provided by IRS [0 to 25]
    #MDAon = scenario[6],         # switch on MDA [C]
    # cmda_1 = 50,                 # effective population coverage of focal MDA in round 1 [0 to 100]
    # cmda_2 = 50,                 # effective population coverage of focal MDA in round 2 [0 to 100]
    # cmda_3 = 50,                 # effective population coverage of focal MDA in round 3 [0 to 100]
    tm_1 = 2018+(input$tm_1/12),          # timing of 1st round [2018 to 2021 - 1 month steps]
    tm_2 = 2018+(input$tm_2/12),          # timing of 2nd round [2018+(1/12) to 2021 - 1 month steps]
    tm_3 = 2018+(input$tm_3/12),          # timing of 3rd round [2018+(2/12) to 2021 - 1 month steps]
    # dm = 6,                      # number of months taken to reach population target coverage in each round [15 to 24]
    # lossd = 30,                  # number of days of profilaxis provided by the ACT [7 to 30]
    # cm_1 = 80,                   # % popultion coverage of 1st MDA round [N] 
    # cm_2 = 95,                   # % of people from 1st MDA round who receieve the 2nd [N] 
    # cm_3 = 95,                   # % of people from 2nd MDA round who receieve the 3rd [N]
    # vacon = input$vacon,         # switch on MDA [C]
    effv_1 = 0,                  # protective efficacy of a single dose of RTS,S [N]
    effv_2 = 0,                 # protective efficacy of two doses of RTS,S [N]
    effv_3 = 0,                 # protective efficacy of three doses of RTS,S [N]
    dv = 1,                      # duration of vaccine protection [N]
    
    #primon = scenario[7],        # ACT+primaquine for EDAT and MDA [C]
    # NB: This is an extremely simplistic model for primaquine
    
    timei = 2018,
    #EDATon = input$EDATon,
    EDATscale = input$EDATscale,
    covEDATi = input$covEDATi,
    #ITNon = input$ITNon,
    ITNscale = input$ITNscale,
    covITNi = input$covITNi,
    effITN = input$effITN,
    #RCDon = input$RCDon,
    RCDscale = input$RCDscale,
    #RCDcoex = input$RCDcoex,
    RCDsensC = input$RCDsensC,
    RCDsensA = input$RCDsensA,
    RCDsensU = input$RCDsensU,
    covRCDi = input$covRCDi,
    effRCD = input$effRCD,
    dRCD = 4,
    clustRCD = input$clustRCD,
    clustRCDcoex = input$clustRCDcoex,
    #IRSon = input$IRSon,
    IRSscale = input$IRSscale,
    covIRSi = input$covIRSi,
    effIRS = input$effIRS,
    #MDAon = input$MDAon,
    cmda_1 = input$cmda_1,
    cmda_2 = input$cmda_2,
    cmda_3 = input$cmda_3,
    #tm_1 = input$tm_1,
    #tm_2 = input$tm_2,
    #tm_3 = input$tm_3,
    dm = input$dm,
    lossd = input$lossd,
    cm_1 = input$cm_1,
    cm_2 = input$cm_2,
    cm_3 = input$cm_3,
    # effv_1 = input$effv_1,
    # effv_2 = input$effv_2,
    # effv_3 = input$effv_3,
    # dv = input$dv,
    #MSATon = scenario[8],
    MSATscale = input$MSATscale,
    covMSATi = input$covMSATi,
    MSATsensC = input$MSATsensC,
    MSATsensA = input$MSATsensA,
    MSATsensU = input$MSATsensU,
    
    # setting typology parameters
    # R0 =  2.20,                  # basic reproduction number
    # eta = 50,                    # % of all infections that are caught in the outside the village (forrest) [0 to 100]
    # covEDAT0 = 30,               # baseline % of all villages with VMW [0 to 100]
    nuTr = 14,                   # days of infectiosness after treatment ACT [N]
    nuTrp = 7,                   # days of infectiosness after treatment ACT+primaquine [N]
    # covITN0 = 60,                # baseline coverage of ITN (%) [0 to 90]
    # covRCD0 = 0,                 # baseline coverage of RCD (%) [0 to 90]
    # covIRS0 = 0,                 # baseline coverage of IRS (%) [0 to 90]
    amp = 0.7,                   # relative amplitude seasonality [N]
    phi = 0.5,                   # phase angle seasonality [N]
    # muC = 5,                     # number of imported clinical cases per 1000 population per year [0 to 10]
    # muA = 50,                    # number of imported super-microscopic asymtomatic infection per 1000 population per year [0 to 100]
    # muU = 50,                    # number of imported sub-microscopic asymtomatic infections per 1000 population per year [0 to 100]
    # percfail2018 = 30,           # % of cases failing treatment in 2018 and before [0 to 100]
    # percfail2019 = 10,           # % of cases failing treatment in 2019  [0 to 100]
    # percfail2020 = 20,           # % of cases failing treatment in 2020 and after  [0 to 100]
    bh = input$bh,                 # bites per human per year
    epsilonh=0.23,                 # per bite probability of an infectious mosquito infecting a human
    epsilonm=0.5,                  # per bite probability of an infectious human infecting a mosquito
    b=365/3,                       # per mosquito rate of biting
    deltam=365/14,                 
    gammam=365/10,
    eta = input$eta,
    covEDAT0 = input$covEDAT0,
    #nuTr = input$nuTr,
    #nuTrp = input$nuTrp,
    covITN0 = input$covITN0,
    covRCD0 = input$covRCD0,
    covIRS0 = input$covIRS0,
    #amp = input$amp,
    #phi = input$phi,
    muC = input$muC,
    muA = input$muA,
    muU = input$muU,
    percfail2018 = input$percfail2018,
    percfail2019 = input$percfail2019,
    percfail2020 = input$percfail2020,
    covMSAT0=0,
    
    # biological parameters
    # omega = 2,                   # average duration of immunity (years) [N]
    # nuC = 9,                     # days of symptoms in the absence of treatment [N]
    # nuA = 60,                    # days of super-microscopic asymtomatic infection [N]
    # nuU = 60,                    # days of sub-microscopic asymtomatic infection [N]
    # rhoa = 70,                   # relative infectivity of super-microscopic asymptomatic infections compared with clinical infections (%) [N]
    # rhou = 30,                   # relative infectivity of sub-microscopic asymptomatic infections compared with clinical infections (%) [N]
    # ps = 90,                     # % of all non-immune new infections that are clinical [N]
    # pr = 20,                     # % of all immune new infections that are clinical [N]
    # mu = 50                      # life expectancy (years) [N]
    
    omega = input$omega,
    nuC = input$nuC,
    nuA = input$nuA,
    nuU = input$nuU,
    rhoa = input$rhoa,
    rhou = input$rhou,
    ps = input$ps,
    pr = input$pr,
    mu = input$mu
  ))
  
  # initial prevalence
  initprevR <- reactive(0.001*input$API)
  
  GMSout0R <- reactive(runGMS(initprevR(), scenario_0,parametersR()))
  
  GMSoutiR <- reactive(runGMS(initprevR(), scenario_iR(),parametersR()))
  
  output$MODEL <- renderPlot({
    GMSout0<-GMSout0R()
    
    GMSouti<-GMSoutiR()
    
    times<-GMSout0[,1]
    clinmonth<-cbind(GMSout0[,2],GMSouti[,2])
    prevalence<-cbind(GMSout0[,3],GMSouti[,3])
    
    runin<-(2016-startyear)/dt
    
    finclin<-max(clinmonth[(runin:length(clinmonth[,1])),])
    finprev<-max(prevalence[(runin:length(prevalence[,1])),])
    
    
    # PLOTTING
    par(mfrow=c(1,2))
    maxy<-max(finclin,input$API/12)
    plot(times[(runin:length(clinmonth[,1]))],clinmonth[runin:length(clinmonth[,1]),1], type='l',lty=1,col="dark grey",xlab = "Time",ylab="incidence per 1000 per month",main="Confirmed cases per month per 1000 population",ylim=c(0,maxy),lwd=2)
    lines(times[(runin:length(clinmonth[,1]))],clinmonth[runin:length(clinmonth[,1]),2], type='l',lty=1,col="blue",xlab = "Time",ylab="incidence per 1000 per month",main="Confirmed cases per month per 1000 population",ylim=c(0,maxy),lwd=2)
    lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    lines(c(2021,2021),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    abline(h=input$API/12,col="dark blue",lty=1,lwd=1)
    abline(h=1/12,col="red",lty=3,lwd=3)
    maxy<-finprev
    plot(times[(runin:length(prevalence[,1]))],prevalence[(runin:length(prevalence[,1])),1], type='l',lty=1,col="dark grey",xlab = "Time",ylab="% prevalence",main="Predicted population prevalence by U-PCR",ylim=c(0,maxy),lwd=2)
    lines(times[(runin:length(prevalence[,1]))],prevalence[(runin:length(prevalence[,1])),2], type='l',lty=1,col="blue",xlab = "Time",ylab="% prevalence",main="Predicted population prevalence by U-PCR",ylim=c(0,maxy),lwd=2)
    lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    lines(c(2021,2021),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    
  })
}

shinyApp(ui = ui, server = server)