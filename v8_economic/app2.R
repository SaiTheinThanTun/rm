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
                    sliderInput(inputId="API", label = "baseline API", value = 2.5, min=1, max=100,step=0.5),
                    sliderInput(inputId="bh", label = "number of mosquito bites per human per day (average)", value = 7, min=0, max=30,step=1),
                    sliderInput(inputId="eta", label = "% of all infections that are caught outside the village (forest)", value = 50, min=0, max=100,step=10),
                    sliderInput(inputId="covEDAT0", label = "baseline % of all clinical cases treated", value = 30, min=0, max=100)
             ),
             column(3,
                    #sliderInput(inputId="nuTr", label = "days of infectiosness after treatment ACT", value = 14, min=0, max=30), #remove
                    #sliderInput(inputId="nuTrp", label = "days of infectiosness after treatment ACT+primaquine", value = 7, min=0, max=30), #remove
                    sliderInput(inputId="covITN0", label = "baseline coverage of ITN (%) ", value = 75, min=0, max=90,step=.5),
                    sliderInput(inputId="effITN", label = "% of infections averted due to owenership of ITN ", value = 30, min=0, max=50),
                    # sliderInput(inputId="covRCD0", label = "baseline coverage of RCD (%) ", value = 0, min=0, max=90),
                    sliderInput(inputId="covIRS0", label = "baseline coverage of IRS (%) ", value = 0, min=0, max=90,step=10),
                    sliderInput(inputId="effIRS", label = "% reduction in biting rate due to IRS ", value = 15, min=0, max=25,step=5)
             ),
             column(3,
                    sliderInput(inputId="muC", label = "imported clinical cases per 1000 population per year ", value = 1, min=0, max=10,step=1),
                    sliderInput(inputId="muA", label = "imported asymptomatic microscopically detectable carriers per 1000 population per year ", value = 1, min=0, max=100,step=1),
                    sliderInput(inputId="muU", label = "imported asymptomatic microscopically undetectable carriers per 1000 population per year ", value = 1, min=0, max=100,step=1)
             ),
             column(3,
                    sliderInput(inputId="percfail2018", label = "% of cases failing treatment in 2018 and before ", value = 30, min=0, max=100,step=5),
                    sliderInput(inputId="percfail2019", label = "% of cases failing treatment in 2019  ", value = 10, min=0, max=100,step=5),
                    sliderInput(inputId="percfail2020", label = "% of cases failing treatment in 2020 and after  ", value = 20, min=0, max=100,step=5)
             )
    ),
    
    tabPanel(title = strong("New Interventions"),
             column(4,
                    wellPanel(
                    h3("Early Diagnosis and Treatment"),
                    #sliderInput(inputId="timei", label = "timing of intervention ", value = 2018, min=2017, max=2020),
                    checkboxInput(inputId="EDATon", label = "switch on scale up of EDAT ", value = FALSE),
                    checkboxInput(inputId="primon", label = "ACT+primaquine for EDAT and MDA ", value = FALSE), #under EDAT checkbox
                    sliderInput(inputId="EDATscale", label = "years to scale up EDAT ", value = 1, min=.25, max=3, step=.25),
                    sliderInput(inputId="covEDATi", label = "new % of all clinical cases treated", value = 90, min=0, max=100,step=5)
                    ), wellPanel(
                    h3("Insecticide Treated Net"),
                    checkboxInput(inputId="ITNon", label = "switch on scale up of ITN ", value = FALSE),
                    sliderInput(inputId="ITNscale", label = "years to scale up ITN ", value = 0.5, min=.25, max=3, step=.25),
                    sliderInput(inputId="covITNi", label = "new coverage of ITN (%) ", value = 90, min=0, max=90,step=5)
                    )
                    
             ),
              column(4, wellPanel(
               h3("Reactive Case Detection"),
               checkboxInput(inputId="RCDon", label = "switch on scale up of RCD", value = FALSE),
               sliderInput(inputId="RCDscale", label = "years to scale up RCD ", value = 2, min=.25, max=3, step=.25), #.25 timesteps
               sliderInput(inputId="covRCDi", label = "new coverage of RCD (%)", value = 50, min=0, max=100,step=10),
               sliderInput(inputId="delayRCD", label = "reaction time (weeks)", value = 4, min=1, max=8,step=1),
               radioButtons(inputId="RCDcoex", label = "RCD Search Type: ", choices = c("Radial search"=0, "Co-exposure search"=1), selected = 0, inline=TRUE),
               sliderInput(inputId="RCDrad", label = "radius for radial search (m)", value = 20, min=5, max=200,step=5),
               sliderInput(inputId="clustRCDrad", label = "added value of radial targetting (%)", value = 40, min=0, max=100,step=10),
               sliderInput(inputId="RCDs", label = "sample size for coexposure search (% of village)", value = 5, min=1, max=100,step=1),
               sliderInput(inputId="clustRCDcoex", label = "added value of co-exposure targetting (%)", value = 50, min=0, max=100,step=10)             )
             ),
             column(4, wellPanel(
               h3("Sensitivity of RCD"),
                    sliderInput(inputId="RCDsensC", label = "sensitivity RCD test (clinical) ", value = 95, min=0, max=100,step=5),
                    sliderInput(inputId="RCDsensA", label = "sensitivity RCD test (micro detectable, asym)", value = 60, min=0, max=100,step=5),
                    sliderInput(inputId="RCDsensU", label = "sensitivity RCD test (micro undetectable, asym)", value = 0, min=0, max=100,step=5)
             ),
             wellPanel(
                       h3("Indoor Residual Spray"),
                    checkboxInput(inputId="IRSon", label = "switch on scale up of IRS ", value = FALSE),
                    sliderInput(inputId="IRSscale", label = "years to scale up IRS ", value = 1, min=.25, max=3, step=.25),
                    sliderInput(inputId="covIRSi", label = "new coverage of IRS (%) ", value = 90, min=0, max=90,step=5)
                    # sliderInput(inputId="effIRS", label = "% reduction in biting rate due to IRS ", value = 15, min=0, max=25)
             )
             )
             
    ),
    tabPanel(title = strong("Focal MDA Indicators"),
             column(3,
                    checkboxInput(inputId="MDAon", label = "switch on MDA", value = FALSE), #6
                    sliderInput(inputId="cmda_1", label = "effective population coverage of focal MDA in round 1 ", value = 50, min=0, max=100,step=10),
                    sliderInput(inputId="cmda_2", label = "effective population coverage of focal MDA in round 2 ", value = 50, min=0, max=100,step=10),
                    sliderInput(inputId="cmda_3", label = "effective population coverage of focal MDA in round 3 ", value = 50, min=0, max=100,step=10)
             ),
             column(3,
                    sliderInput(inputId="tm_1", label = "timing of 1st round [2018+ no. of month, 1 means Jan'2018, 13 means Jan'2019]", value = 1, min=1, max=36,step=1),
                    sliderInput(inputId="tm_2", label = "timing of 2nd round [2018+ no. of month]", value = 2, min=2, max=36,step=1),
                    sliderInput(inputId="tm_3", label = "timing of 3rd round [2018+ no. of month]", value = 3, min=3, max=36,step=1),
                    sliderInput(inputId="dm", label = "months to complete each round ", value = 6, min=1, max=24,step=0.5)
             ),
             column(3,
                    sliderInput(inputId="lossd", label = "days prophylaxis provided by the ACT", value = 30, min=7, max=30,step=1),
                    sliderInput(inputId="cm_1", label = "% population coverage of 1st MDA round", value = 80, min=0, max=100,step=10),
                    sliderInput(inputId="cm_2", label = "% of 1st MDA round population to get 2nd", value = 95, min=0, max=100,step=10),
                    sliderInput(inputId="cm_3", label = "% of 2nd MDA round population to get 3rd", value = 95, min=0, max=100,step=10)
             )
    ),
    tabPanel(title = strong("Imported Malaria MSAT Indicators"),
             column(3,
                    checkboxInput(inputId="MSATon", label = "switch on MSAT for imported cases", value = FALSE),
                    sliderInput(inputId="MSATscale", label = "years to scale up MSAT ", value = 2, min=.25, max=3, step=.25), 
                    sliderInput(inputId="covMSATi", label = "new coverage of MSAT (%)", value = 80, min=0, max=100,step=10)
              ),
             column(3,
                    sliderInput(inputId="MSATsensC", label = "sensitivity MSAT test (clinical) ", value = 95, min=0, max=100,step=5),
                    sliderInput(inputId="MSATsensA", label = "sensitivity MSAT test (micro detectable, asym)", value = 60, min=0, max=100,step=5),
                    sliderInput(inputId="MSATsensU", label = "sensitivity MSAT test (micro undetectable, asym)", value = 0, min=0, max=100,step=5)
             )
    ),
    tabPanel(title= strong("Download"),
             br(),
             downloadButton("downloadTable", "Download current values of parameters"),
             downloadButton("downloadplot","Download high resolution figure")),
    tabPanel(title= strong("Restore your parameters"),
             wellPanel(
               fileInput(inputId = "file", label ="Your input file:", accept = c(".csv"))
             )
             #,
             #tableOutput(outputId = "table")
    ),
    tabPanel(title=strong("User Manual & Help"),
             br(),
             tags$ul(tags$li(strong(a(href="https://www.dropbox.com/s/g9b96n4lki1r2l6/RAI_strategydesigntool_usermanual_01022017.docx?dl=0", "Download User Manual")))),
             strong("Contact the developers for any question and feedback"),
             tags$ul(tags$li(a(href="http://www.tropmedres.ac/researchers/researcher/lisa-white","Professor Lisa White, "), a(href="mailto:lisa@tropmedres.ac","lisa@tropmedres.ac")), 
                             tags$li(a(href="http://www.tropmedres.ac/researchers/researcher/sompob-saralamba","Dr Sompob Saralamba, "),a(href="mailto:sompob@tropmedres.ac","sompob@tropmedres.ac")), 
                                     tags$li(a(href="http://www.tropmedres.ac/sai-thein-than-tun","Dr Sai Thein Than Tun, "), a(href="mailto:sai@tropmedres.ac","sai@tropmedres.ac"))))
    # tabPanel(title = strong("Biological Parameters"), #not remove
    #          column(3,
    #                 sliderInput(inputId="omega", label = "average duration of immunity (years) ", value = 2, min=0, max=10),
    #                 sliderInput(inputId="nuC", label = "days of symptoms in the absence of treatment ", value = 9, min=1, max=30),
    #                 sliderInput(inputId="nuA", label = "days of asymptomatic microscopically detectable carriage ", value = 60, min=30, max=90)
    #          ),
    #          column(3,
    #                 sliderInput(inputId="nuU", label = "days of asymptomatic microscopically undetectable carriage ", value = 60, min=30, max=365),
    #                 sliderInput(inputId="rhoa", label = "relative infectivity of asymptomatic microscopically detectable carriers compared with clinical infections (%) ", value = 70, min=0, max=100)
    #          ),
    #          column(3,
    #                 sliderInput(inputId="rhou", label = "relative infectivity of asymptomatic microscopically undetectable carriers compared with clinical infections (%) ", value = 30, min=0, max=100),
    #                 sliderInput(inputId="ps", label = "% of all non-immune new infections that are clinical ", value = 90, min=0, max=100)
    #          ),
    #          column(3,
    #                 sliderInput(inputId="pr", label = "% of all immune new infections that are clinical ", value = 20, min=0, max=100),
    #                 sliderInput(inputId="mu", label = "life expectancy (years) ", value = 50, min=45, max=95)
    #          )
    # )
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
  fluidRow(h4("          Red dashed line: pre-elimination threshold (API = 1 per 1000 per year)"))
  
)

#non-reactive parameters
# define the number of weeks to run the model
dt<-1/12
startyear<-2007
stopyear<-2025
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)
tsteps<-length(times)

ParLabel <- read.table('ParLabel.csv', sep=",", as.is=TRUE)

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
                  kRCD = 0.017,                
                  cRCD = 105,                
                  bRCD = 0.024,
                  gRCD = 230,
                  muRCDw=4,
                  sdRCDw=1.5,
                  covMSAT0=0,
                  omega = 2,                   # average duration of immunity (years) [N]
                  nuC = 3,                     # days of symptoms in the absence of treatment [N], #change 9 -> 3
                  nuA = 60,                    # days of asymptomatic microscopically detectable carriage [N]
                  nuU = 100,                    # days of asymptomatic microscopically undetectable carriage [N], #change 60 -> 100, Mean duration of a malaria untreated infection: 160 days, 
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
           # nuRCD<- 52/delayRCD
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
           propRCD<-(1-RCDcoex)*((1+exp(-kRCD*cRCD))*((1/(1+exp(-kRCD*(RCDrad-cRCD))))-(1/(1+exp(kRCD*cRCD)))))+RCDcoex*RCDs
           fRCD<-dnorm(delayRCD, mean = muRCDw, sd = sdRCDw, log = FALSE)/dnorm(muRCDw, mean = muRCDw, sd = sdRCDw, log = FALSE)
           avrad<-clustRCDrad/(1+exp(-bRCD*(gRCD-RCDrad)))
           eqRCDrad<-cRCD-((1/kRCD)*log(((1+exp(kRCD*cRCD))/(1+RCDs*exp(kRCD*cRCD)))-1))
           avcoex<-clustRCDcoex/(1+exp(-bRCD*(gRCD-eqRCDrad)))
           rateRCD<-RCDon*covRCD*incm*(propRCD+fRCD*((1-RCDcoex)*(1-eta)*avrad+RCDcoex*avcoex))
           tauRCD<-rateRCD
           
           
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
                  dS_3, dIC_3, dIA_3, dIU_3, dR_3, dTr_3, dSm_3, dRm_3
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
  
  
  # % prevalence
  prevalence <- 100*rowSums(out[,iprev])/pop
  GMSout<-matrix(NA,nrow=length(times),ncol=4)
  GMSout[,1]<-times
  GMSout[,2]<-clinmonth_det
  GMSout[,3]<-clinmonth_tot
  GMSout[,4]<-prevalence
  
  return(GMSout)
}


server <- function(input, output, session) {
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
                          RCDcoex = as.numeric(input$RCDcoex),
                          IRSon = input$IRSon,
                          MDAon = input$MDAon,
                          primon = input$primon,
                          MSATon = input$MSATon))
  
  parametersR <- reactive(c(
    bh = input$bh,                 # bites per human per year
    eta = input$eta,
    covEDAT0 = input$covEDAT0,
    covITN0 = input$covITN0,
    effITN = input$effITN,
    covIRS0 = input$covIRS0,
    effIRS = input$effIRS,
    muC = input$muC,
    muA = input$muA,
    muU = input$muU,
    percfail2018 = input$percfail2018,
    percfail2019 = input$percfail2019,
    percfail2020 = input$percfail2020,
    
    EDATscale = input$EDATscale,
    covEDATi = input$covEDATi,
    ITNscale = input$ITNscale,
    covITNi = input$covITNi,
    RCDscale = input$RCDscale,
    covRCDi = input$covRCDi,
    delayRCD = input$delayRCD,
    #RCDrad = input$RCDrad,
    clustRCDrad = input$clustRCDrad,
    #RCDs = input$RCDs,
    clustRCDcoex = input$clustRCDcoex,
    RCDsensC = input$RCDsensC,
    RCDsensA = input$RCDsensA,
    RCDsensU = input$RCDsensU,
    IRSscale = input$IRSscale,
    covIRSi = input$covIRSi,
    
    cmda_1 = input$cmda_1,
    cmda_2 = input$cmda_2,
    cmda_3 = input$cmda_3,
    tm_1 = input$tm_1,          # timing of 1st round [2018 to 2021 - 1 month steps]
    tm_2 = input$tm_2,          # timing of 2nd round [2018+(1/12) to 2021 - 1 month steps]
    tm_3 = input$tm_3,          # timing of 3rd round [2018+(2/12) to 2021 - 1 month steps]
    dm = input$dm,
    lossd = input$lossd,
    cm_1 = input$cm_1,
    cm_2 = input$cm_2,
    cm_3 = input$cm_3,
    
    MSATscale = input$MSATscale,
    covMSATi = input$covMSATi,
    MSATsensC = input$MSATsensC,
    MSATsensA = input$MSATsensA,
    MSATsensU = input$MSATsensU,
    
    RCDrad = input$RCDrad,
    RCDs = input$RCDs
  ))
  
  #getting back previous parameters
  data <- reactive({read.csv(input$file$datapath)})
  datavalue <- reactive(data()[,2])
  observeEvent(input$file,{
    updateCheckboxInput(session, "EDATon", value = datavalue()[1])
    updateCheckboxInput(session, "ITNon", value = datavalue()[2])
    updateCheckboxInput(session, "RCDon", value = datavalue()[3])
    updateRadioButtons(session, "RCDcoex", selected = datavalue()[4])
    updateCheckboxInput(session, "IRSon", value = datavalue()[5])
    updateCheckboxInput(session, "MDAon", value = datavalue()[6])
    updateCheckboxInput(session, "primon", value = datavalue()[7])
    updateCheckboxInput(session, "MSATon", value = datavalue()[8])
    
    updateSliderInput(session, "API", value = datavalue()[9])
    
    updateSliderInput(session, "bh", value = datavalue()[10])
    updateSliderInput(session, "eta", value = datavalue()[11])
    updateSliderInput(session, "covEDAT0", value = datavalue()[12])
    updateSliderInput(session, "covITN0", value = datavalue()[13])
    updateSliderInput(session, "effITN", value = datavalue()[14])
    updateSliderInput(session, "covIRS0", value = datavalue()[15])
    updateSliderInput(session, "effIRS", value = datavalue()[16])
    updateSliderInput(session, "muC", value = datavalue()[17])
    updateSliderInput(session, "muA", value = datavalue()[18])
    updateSliderInput(session, "muU", value = datavalue()[19])
    updateSliderInput(session, "percfail2018", value = datavalue()[20])
    updateSliderInput(session, "percfail2019", value = datavalue()[21])
    updateSliderInput(session, "percfail2020", value = datavalue()[22])
    updateSliderInput(session, "EDATscale", value = datavalue()[23])
    updateSliderInput(session, "covEDATi", value = datavalue()[24])
    updateSliderInput(session, "ITNscale", value = datavalue()[25])
    updateSliderInput(session, "covITNi", value = datavalue()[26])
    updateSliderInput(session, "RCDscale", value = datavalue()[27])
    updateSliderInput(session, "covRCDi", value = datavalue()[28])
    updateSliderInput(session, "delayRCD", value = datavalue()[29])
    updateSliderInput(session, "clustRCDrad", value = datavalue()[30])
    updateSliderInput(session, "clustRCDcoex", value = datavalue()[31])
    updateSliderInput(session, "RCDsensC", value = datavalue()[32])
    updateSliderInput(session, "RCDsensA", value = datavalue()[33])
    updateSliderInput(session, "RCDsensU", value = datavalue()[34])
    updateSliderInput(session, "IRSscale", value = datavalue()[35])
    updateSliderInput(session, "covIRSi", value = datavalue()[36])
    updateSliderInput(session, "cmda_1", value = datavalue()[37])
    updateSliderInput(session, "cmda_2", value = datavalue()[38])
    updateSliderInput(session, "cmda_3", value = datavalue()[39])
    updateSliderInput(session, "tm_1", value = datavalue()[40])
    updateSliderInput(session, "tm_2", value = datavalue()[41])
    updateSliderInput(session, "tm_3", value = datavalue()[42])
    updateSliderInput(session, "dm", value = datavalue()[43])
    updateSliderInput(session, "lossd", value = datavalue()[44])
    updateSliderInput(session, "cm_1", value = datavalue()[45])
    updateSliderInput(session, "cm_2", value = datavalue()[46])
    updateSliderInput(session, "cm_3", value = datavalue()[47])
    updateSliderInput(session, "MSATscale", value = datavalue()[48])
    updateSliderInput(session, "covMSATi", value = datavalue()[49])
    updateSliderInput(session, "MSATsensC", value = datavalue()[50])
    updateSliderInput(session, "MSATsensA", value = datavalue()[51])
    updateSliderInput(session, "MSATsensU", value = datavalue()[52])
    updateSliderInput(session, "RCDrad", value = datavalue()[53])
    updateSliderInput(session, "RCDs", value = datavalue()[54])
    
  })
  
  #testing
  #output$table <- renderTable(datavalue()[1:8])
  
  # initial prevalence
  initprevR <- reactive(0.001*input$API)
  
  GMSout0R <- reactive(runGMS(initprevR(), scenario_0,parametersR()))
  
  GMSoutiR <- reactive(runGMS(initprevR(), scenario_iR(),parametersR()))
  
  plotR <- function()
  {
    GMSout0<-GMSout0R()
    
    GMSouti<-GMSoutiR()
    
    times<-GMSout0[,1]
    clinmonth_det<-cbind(GMSout0[,2],GMSouti[,2])
    clinmonth_tot<-cbind(GMSout0[,3],GMSouti[,3])
    prevalence<-cbind(GMSout0[,4],GMSouti[,4])
    
    runin<-(2016-startyear)/dt
    
    finclin<-max(clinmonth_tot[(runin:length(clinmonth_det[,1])),])
    finprev<-max(prevalence[(runin:length(prevalence[,1])),])
    
    
    # PLOTTING
    par(mfrow=c(1,2))
    
    maxy<-max(finclin,input$API/12)
    x<-times[(runin:length(clinmonth_det[,1]))]
    y1<-clinmonth_det[runin:length(clinmonth_det[,1]),1]
    y2<-clinmonth_tot[runin:length(clinmonth_tot[,1]),1]
    
    plot(x,y1, type='l',lty=1,col=rgb(0,0,0,alpha=0.1),xlab = "Time",ylab="incidence per 1000 per month",main="Monthly cases per 1000 population",ylim=c(0,maxy),lwd=2)
    lines(x,y2, type='l',lty=1,col=rgb(0,0,0,alpha=0.1),lwd=2)
    
    polygon(c(x,rev(x)),c(y2,rev(y1)),col=rgb(0,0,0,alpha=0.1),border=NA)
    
    y1<-clinmonth_det[runin:length(clinmonth_det[,1]),2]
    y2<-clinmonth_tot[runin:length(clinmonth_tot[,1]),2]
    lines(x,y1, type='l',lty=1,col=rgb(0,0,1,alpha=0.4),lwd=2)
    lines(x,y2, type='l',lty=1,col=rgb(0,0,1,alpha=0.4),lwd=2)
    
    polygon(c(x,rev(x)),c(y2,rev(y1)),col=rgb(0,0,1,alpha=0.4),border=NA)
    
    lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    lines(c(2021,2021),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    abline(h=input$API/12,col="dark blue",lty=1,lwd=1)
    abline(h=1/12,col="red",lty=3,lwd=3)
    maxy<-finprev
    plot(times[(runin:length(prevalence[,1]))],prevalence[(runin:length(prevalence[,1])),1], type='l',lty=1,col=rgb(0,0,0,alpha=0.25),xlab = "Time",ylab="% prevalence",main="Predicted true prevalence",ylim=c(0,maxy),lwd=6)
    lines(times[(runin:length(prevalence[,1]))],prevalence[(runin:length(prevalence[,1])),2], type='l',lty=1,col=rgb(0,0,1,alpha=0.6),xlab = "Time",ylab="% prevalence",main="Predicted true prevalence",ylim=c(0,maxy),lwd=6)
    lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    lines(c(2021,2021),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
  }
  
  output$MODEL <- renderPlot({
    plotR()
  })
  
  output$downloadplot <- downloadHandler(
    filename = function(){paste('MalMod_',gsub("\\:","",Sys.time()),'.png',sep='')},
    content = function(file) {
      png(filename=file, height= 1600, width=4800, units= "px", res=300) #if(...=="png"){png(file)} else if(...=="pdf"){pdf(file)}
      plotR()
      dev.off()
    })
  
  tableContentR <- reactive({
    tmp <- c(scenario_iR(), input$API, parametersR())
    tmp2 <- cbind(ParLabel[,1], tmp, ParLabel[,2], names(tmp))
    colnames(tmp2) <- c("Name","Value","Unit","VarName")
    tmp2
  })
  
  output$downloadTable <- downloadHandler(
    filename = function(){paste('MalMod_',gsub("\\:","",Sys.time()),'.csv',sep='')},
    content = function(file) {
      write.csv(tableContentR(), file, row.names = FALSE)
    })
}

shinyApp(ui = ui, server = server)