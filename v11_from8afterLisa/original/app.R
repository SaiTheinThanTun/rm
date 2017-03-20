#TEMPLATE FOR ODE FUNCTIONS LAO/RAI ModelS
#shiny version of 
library(deSolve)
library(shiny)
library(TSA)
#library(Rcpp)
#sourceCpp("modGMS.cpp")



ui <- fluidPage(
  tabsetPanel(
    id="panels",
    tabPanel(title = strong("Typology Parameters"),
             column(3,
                    sliderInput(inputId="API", label = "baseline API", value = 2.5, min=1, max=100,step=0.5),
                    sliderInput(inputId="bh_max", label = "number of mosquito bites per human per night (peak season)", value = 11, min=0, max=80,step=1), #change range 0-80, Dan's data
                    sliderInput(inputId="eta", label = "% of all infections that are caught outside the village (forest)", value = 50, min=0, max=100,step=10),
                    sliderInput(inputId="covEDAT0", label = "baseline % of all clinical cases treated", value = 30, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="covITN0", label = "baseline coverage of ITN (%) ", value = 75, min=0, max=90,step=.5),
                    sliderInput(inputId="effITN", label = "% of infections averted due to owenership of ITN ", value = 30, min=0, max=50), 
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
             )
             )
             
    ),
    tabPanel(title = strong("Focal MDA Indicators"),
             column(3,
                    checkboxInput(inputId="MDAon", label = "switch on MDA", value = FALSE),
                    sliderInput(inputId="dm", label = "time needed to visit all villages in target population (months) ", value = 3, min=1, max=6,step=1),
                    sliderInput(inputId="lossd", label = "days prophylaxis provided by the ACT", value = 30, min=7, max=30,step=1)
             ),
             column(3,
                    sliderInput(inputId="tmda", label = "Month of the year to start set of MDA rounds", value = 11, min=1, max=12,step=1),
                    sliderInput(inputId="nyMDA", label = "Number of years to deliver sets of MDA rounds", value = 1, min=1, max=3,step=1),
                    sliderInput(inputId="avMDA", label = "Added value of focal targetting", value = 50, min=0, max=80,step=10)
             ),
             
             column(3,
                    sliderInput(inputId="prevT2018", label = "Prevalence threshold for defining hotspots in 2018", value = 30, min=0, max=50,step=10),
                    sliderInput(inputId="prevT2019", label = "Prevalence threshold for defining hotspots in 2019", value = 30, min=0, max=50,step=10),
                    sliderInput(inputId="prevT2020", label = "Prevalence threshold for defining hotspots in 2020", value = 30, min=0, max=50,step=10)
             )

    ),
    tabPanel(title = strong("Mass Vaccination Indicators"),
             column(3,
                    checkboxInput(inputId="Vacon", label = "switch on mass Vaccination", value = FALSE),
                    sliderInput(inputId="lossv", label = "duration of vaccine protective efficacy (years)",value = 1, min=0.25, max=3,step=0.25),
                    sliderInput(inputId="effv", label = "initial level of vaccine protective efficacy",value = 30, min=0, max=100,step=10),
                    radioButtons(inputId="Vactype", label = "Mass Vaccination coverage type: ", choices = c("Mass vaccination"=0, "MVDA"=1), selected = 1, inline=TRUE)
             ),
             column(3,
                    sliderInput(inputId="covvmass", label = "population coverage for mass vaccination option",value = 80, min=0, max=90,step=10),
                    sliderInput(inputId="dcv", label = "time needed to vaccinate residents of all villages in target population (months) ", value = 3, min=1, max=6,step=1)
              ),
             column(3,
                    sliderInput(inputId="tvac", label = "Month of the year to start mass vaccination", value = 11, min=1, max=12,step=1),
                    sliderInput(inputId="nyvac", label = "Number of years to deliver annual mass vaccination", value = 1, min=1, max=3,step=1)
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
                  rmda = 3,
                  cmda1=0.8,
                  cmda2=0.85,
                  cmda3=0.9,
                  kmda = 1,
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
  
  initS<-0.5*(1-initprev)*initP
  initIC<-0
  initIA<-initprev*initP
  initIU<-0
  initR<-0.5*(1-initprev)*initP
  initTr<-0
  
  state <- c(Y = 0, Cinc_det = 0, Cinc_tot = 0, 
             S = initS, IC = initIC, IA = initIA, IU = initIU, R = initR, Tr = initTr, Sm = 0, Rm = 0, V=0
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
  
           avMDA <- avMDA/100
           prevT2018 <- prevT2018/100
           prevT2019 <- prevT2019/100
           prevT2020 <- prevT2020/100
           
           effv <- effv/100
           covvmass <- covvmass/100
           

           # effv_1<-effv_1/100
           # effv_2<-effv_2/100
           # effv_3<-effv_3/100
           rhoa<-rhoa/100
           rhou<-rhou/100
           ps<-ps/100
           pr<-pr/100
           eta<-eta/100
           # convert time scales
           dm<-dm/12
           tmda <- 2018+(tmda/12) 
           dcv<-dcv/12
           tvac <- 2018+(tvac/12) 
           
           # convert durations to rate
           lossd<-365/lossd
           lossv<-1/lossv
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
           
           
           # define variables
           P <- (S+R+IC+IA+IU+Tr+Sm+Rm)
           seas<-1+amp*cos(2*3.14159*(Y-phi))
           nu <- 1/((1/nuC)+(1/nuA)+(1/nuU))
           bh<-bh_max/(1+amp)
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
           
           
           # MDA rounds
           
           prevA<-(IC+Tr+IA+IU)/P
           prevT <- (((Y+startyear)>2018)*((Y+startyear)<=2019)*prevT2018+((Y+startyear)>2019)*((Y+startyear)<=2020)*prevT2019+((Y+startyear)>2020)*prevT2020)
           cc <- (1-avMDA)/(-2+(1+avMDA)*exp(kmda*prevA))
           cmda <- (rmda==1)*cmda1+(rmda==2)*cmda2+(rmda==3)*cmda3
           cemda <- cmda*((Y+startyear)>2018)*(1+cc)/(1+cc*exp(kmda*prevT))
           
           swmda <- (Y>(tmda-startyear))*(Y<=(tmda+dm-startyear))+(nyMDA>=2)*(Y>(tmda+1-startyear))*(Y<=(tmda+1+dm-startyear))+(nyMDA>=3)*(Y>(tmda+2-startyear))*(Y<=(tmda+2+dm-startyear))
           mdarate <- MDAon*swmda*(-log((1-cemda))/(dm+(rmda/12))) 
           
           # vaccine rates
           swvac <- (Y>(tvac-startyear))*(Y<=(tvac+dcv-startyear))+(nyvac>=2)*(Y>(tvac+1-startyear))*(Y<=(tvac+1+dcv-startyear))+(nyvac>=3)*(Y>(tvac+2-startyear))*(Y<=(tvac+2+dcv-startyear))
           
           vacrate <- Vacon*(Vactype*swmda*MDAon*(-log((1-cemda))/(dm+(rmda/12)))+(1-Vactype)*swvac*(-log((1-covvmass))/(dcv+(3/12))) )
           
           lam <- (1-(effv*V/P))*(1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(IC+Tr+rhoa*IA+rhou*IU)/P

           tau <- covEDAT
           
           fail <- ((Y+startyear)<2019)*(percfail2018/100)+((Y+startyear)>=2019)*((Y+startyear)<2020)*(percfail2019/100)+((Y+startyear)>=2020)*(percfail2020/100)
           
           
           # set up treatment rate for RCD
           incm<-ps*tau*lam*S+pr*tau*lam*R+pr*tau*lam*IU+pr*tau*lam*IA
           propRCD<-(1-RCDcoex)*((1+exp(-kRCD*cRCD))*((1/(1+exp(-kRCD*(RCDrad-cRCD))))-(1/(1+exp(kRCD*cRCD)))))+RCDcoex*RCDs
           fRCD<-exp(-((delayRCD-muRCDw)^2)/(2*sdRCDw))
           avrad<-clustRCDrad/(1+exp(-bRCD*(gRCD-RCDrad)))
           eqRCDrad<-cRCD-((1/kRCD)*log(((1+exp(kRCD*cRCD))/(1+RCDs*exp(kRCD*cRCD)))-1))
           avcoex<-clustRCDcoex/(1+exp(-bRCD*(gRCD-eqRCDrad)))
           rateRCD<-RCDon*covRCD*incm*(propRCD+fRCD*((1-RCDcoex)*(1-eta)*avrad+RCDcoex*avcoex))
           tauRCD<-rateRCD
           
           
  

           treat <- ((ps*tau*lam*S+pr*tau*lam*R+pr*tau*lam*IU+pr*tau*lam*IA)
                     +mdarate*(IC+IA+IU)
                      +tauRCD*(RCDsensC*IC+RCDsensA*IA+RCDsensU*IU)
           )
           
           muC <- (1-MSATon*MSATsensC*covMSAT)*muC
           muA <- (1-MSATon*MSATsensA*covMSAT)*muA
           muU <- (1-MSATon*MSATsensU*covMSAT)*muU
           
           # rate of change
           dY <- 1                                                                                                                                         #2                                                                    
           dCinc_det <- ps*tau*lam*S+pr*tau*lam*R+pr*tau*lam*IU+pr*tau*lam*IA                                                                              #3
           dCinc_tot <- ps*lam*S+pr*lam*R+pr*lam*IU+pr*lam*IA                                                                                              #4
           dS <- mu*P-mu_out*S+omega*R-lam*S+lossd*Sm-mdarate*S                                                                                            #5
           dIC <- muC*P-mu_out*IC+ps*(1-tau)*lam*S+pr*(1-tau)*lam*R+pr*(1-tau)*lam*IU+pr*(1-tau)*lam*IA-nuC*IC-mdarate*IC-RCDsensC*tauRCD*IC               #6 
           dIA <- muA*P-mu_out*IA+(1-ps)*lam*S+(1-pr)*lam*R+(1-pr)*lam*IU-pr*lam*IA+nuC*IC-nuA*IA+fail*nuTr*Tr-mdarate*IA-RCDsensA*tauRCD*IA               #7
           dIU <- muU*P-mu_out*IU-lam*IU-nuU*IU+nuA*IA-mdarate*IU-RCDsensU*tauRCD*IU                                                                       #8
           dR <- -mu_out*R-omega*R-lam*R+nuU*IU+lossd*Rm-mdarate*R                                                                                         #9
           dTr <- -mu_out*Tr+ps*tau*lam*S+pr*tau*lam*R+pr*tau*lam*IU+pr*tau*lam*IA-nuTr*Tr+tauRCD*(RCDsensC*IC+RCDsensA*IA+RCDsensU*IU)+mdarate*(IC+IA+IU) #10
           dSm <- -mu_out*Sm+omega*Rm-lossd*Sm+mdarate*S                                                                                                   #11
           dRm <- -mu_out*Rm-omega*Rm+(1-fail)*nuTr*Tr-lossd*Rm+mdarate*R                                                                                  #12
           dV <- -mu_out*V+vacrate*(P-V)-lossv*V                                                                                                           #13
           # dV <- vacrate*(P-V)-lossv*V                                                                                                           #13
           

           # return the rate of change
           list(c(dY,dCinc_det,dCinc_tot, 
                  dS, dIC, dIA, dIU, dR, dTr, dSm, dRm, dV 
            ))
         }
    ) 
    
  }
  
  out <- ode(y = state, times = times, func = modGMS, parms = parameters)
  # WmodGMSrcpp<-function(t,state,parameters){
  #   tmp<-modGMSrcpp(t,state,parameters)
  #   return(list(tmp))
  # }
  # out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = parameters)
  
  # MODEL OUTPUTS
  ipop <- 5:12
  iinc_det <- 3
  iinc_tot <- 4
  iprev <- c(6,  7,  8, 10)
  
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
                Vacon = 0,
                Vactype = 1,
                primon = 0,
                MSATon = 0)
  
  scenario_iR<-reactive(c(EDATon = input$EDATon,
                          ITNon = input$ITNon,
                          RCDon = input$RCDon,
                          RCDcoex = as.numeric(input$RCDcoex),
                          IRSon = input$IRSon,
                          MDAon = input$MDAon,
                          Vacon = input$Vacon,
                          Vactype = as.numeric(input$Vactype),
                          primon = input$primon,
                          MSATon = input$MSATon))
  
  parametersR <- reactive(c(
    bh_max = input$bh_max,                 # bites per human per night
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
    clustRCDrad = input$clustRCDrad,
    clustRCDcoex = input$clustRCDcoex,
    RCDsensC = input$RCDsensC,
    RCDsensA = input$RCDsensA,
    RCDsensU = input$RCDsensU,
    IRSscale = input$IRSscale,
    covIRSi = input$covIRSi,
    
    avMDA = input$avMDA,
    tmda = input$tmda,          
    nyMDA = input$nyMDA,          
    dm = input$dm,
    lossd = input$lossd,
    prevT2018 = input$prevT2018,
    prevT2019 = input$prevT2019,
    prevT2020 = input$prevT2020,
    
    lossv = input$lossv,
    effv = input$effv,
    covvmass = input$covvmass,
    dcv = input$dcv,
    tvac = input$tvac,
    nyvac = input$nyvac,
    
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
    
    updateSliderInput(session, "bh_max", value = datavalue()[10])
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