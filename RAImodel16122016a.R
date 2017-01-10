#############   Simple model for RAI assessment    ###########

#setwd("C:/Users/lisa/Dropbox/0_Backup_Ddrive/Data_Current/arjen/RAIconceptnote2016")

library(deSolve)
library(TSA)

# define the number of weeks to run the model
dt<-1/12
startyear<-2015
stopyear<-2030
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)
tsteps<-length(times)

# initial proportions resistant
presa <- 0.9         # proportion of all infections resistant to artemisin at start of simulation
presp1 <- 0.01       # proportion of all infections resistant to partner drug 1 at start of simulation must be less than presa if double resisatnce
double1 <- 1         # is the partner drug 1 resistance double resistance? y=1, n=0 
presp2 <- 0.00       # proportion of all infections resistant to partner drug 2 at start of simulation
double2 <- 0         # is the partner drug 2 resistance double resistance? y=1, n=0 

# initial prevalence
initprev<-0.2


if (presp1*double1>presa){
  print("ERROR: chose a lower value for presp1")
}
if (presp2*double2>presa){
  print("ERROR: chose a lower value for presp2")
}


runRAI<-function(scenario) 
{
  
  #MODEL PARAMETERS
  parameters <- c(
    # intervention parameters
    timei = scenario[6],         # timing of intervention
    scalei = scenario[7],        # number of years required to reach intervention targets
    EDATon = scenario[1],        # switch on scale up of EDAT
    ITNon = scenario[2],         # switch on scale up of ITN
    IRSon = scenario[3],         # switch on scale up of IRS
    RCDon = scenario[4],         # switch on scale up of RCD    
    ta2ap2 = scenario[5],        # timing of switch to ACT2
    CHWon = scenario[8],         # switch on scale up of CHW    
    covEDATi = 0.9,      # intervention proportion of all clinical cases that receive treatment
    covITNi = 0.8,       # intervention coverage of ITN
    effITN = 0.2,        # efficacy of ITN in reducing force of infection
    durITN = 2,          # duration of efficay of ITN
    covIRSi = 0.3,       # intervention coverage of IRS
    effIRS = 0.2,        # efficacy of ITN in reducing force of infection
    covRCDi = 0.8,       # intervention coverage of RCD
    effRCD = 1.0,        # efficacy of RCD measured as the avereage number of additional cases detected per clinical case reported 
    covCHWi = 0.9,       # intervention coverage of extended role VMWs
    effCHW = 0.9,        # efficacy of f extended role VMWs in reducing fatigue 
    # setting typology parameters
    R0 =  2.7,           # basic reproduction number
    eta = 0.5,           # proportion of all infections that occur in the forrest
    covEDAT0 = 0.3,      # baseline proportion of all clinical cases that receive treatment
    nuT1 = 365/3,        # recovery under successful treatement with ACT #1
    covITN0 = 0.2,       # baseline coverage of ITN
    covIRS0 = 0.0,       # baseline coverage of IRS
    covRCD0 = 0.0,       # baseline coverage of RCD (reactive case detection)
    covCHW0 = 0.5,       # baseline coverage of extended role VMWs
    amp = 0.7,           # relative amplitude seasonality
    phi = 0.5,           # phase angle seasonality
    muC = 0.001,         # rate of in-migration of clincial cases = number of cases per person (in current pop) per year
    muA = 0.01,          # rate of in-migration of super-microscopic asymptomatic infections = number of new people per person (in current pop) per year
    muU = 0.01,          # rate of in-migration of sub-microscopic asymptomatic infections = number of new people per person (in current pop) per year
    kf = 0.9,            # maximum fatigue due to low proportion testing positive (0 to 1)
    # biological parameters
    omega = 1/2,         # rate of loss of immunity = 1/(average duration of immunity)
    nuC = 365/10,        # rate of loss of symptoms in the absence of treatment
    nuA = 365/60,        # 1/(duration of super-microscopic asymtomatic infection)
    nuU = 365/60,        # 1/(duration of sub-microscopic asymtomatic infection)
    rhoa = 0.7,          # relative infectivity of super-microscopic asymptomatic infections compared with clinical infections
    rhou = 0.3,          # relative infectivity of sub-microscopic asymptomatic infections compared with clinical infections
    ps = 1,              # proportion of all non-immune new infections that are clinical
    pr = 0.2,            # proportion of all immune new infections that are clinical
    mu = (1/50),         # birth/death
    incnm = 1,           # incidence of non-malaria febrile illness
    # resistance spread parameters
    fa = 0.1,           # proportion of artemisinin resistant cases that fail
    ka = 5,              # speed of fixation of artemisinin resistance with respect to cumulative exposure
    ha =  1e9,           # number of exposures to artemisinin mono therapy required for a selective sweeep for artemisinin resistance                
    fp1 = 0.3,           # proportion of partner drug 1 resistant cases that fail
    kp1 = 5,             # speed of fixation of partner drug 1 resistance
    hp1 = 1E8,           # number of exposures to ACT 1 required for a selective sweeep for partner drug 1 resistance                
    hap1 = 1E7,          # number of exposures to ACT 1 required for a selective sweeep for double resistance given artemisnin resistance                
    hp1a = 1E8,          # number of exposures to ACT 1 required for a selective sweeep for double resistance given partner drug 1 resistance                
    fap1 = 0.9,          # proportion of ACT1 resistant cases that fail 
    fp2 = 0.3,           # proportion of partner drug 2 resistant cases that fail
    kp2 = 5,             # speed of fixation of partner drug 2 resistance
    hp2 = 1E7,#5,        # position of selective sweeep for fixation of partner drug 2 resistance                
    hap2 = 1E6,#4,       # number of exposures to ACT 2 required for a selective sweeep for double resistance given artemisnin resistance                
    hp2a = 1E8,          # number of exposures to ACT 2 required for a selective sweeep for double resistance given partner drug 2 resistance                
    fap2 = 0.9,          # proportion of ACT2 resistant cases that fail
    ta2ap1 = 2000        # timing of switch from mono to ACT1
    )
  
  
  # MODEL INITIAL CONDITIONS
  # population size
  initP<-10000 
  
  initS<-0.5*(1-initprev)*initP
  initIC<-0
  initIA<-initprev*initP
  initIU<-0
  initR<-0.5*(1-initprev)*initP
  initT1<-0
  
  
  # adjust cumulative exposure to reflect current proportions resistant
  initCa<-0
  if (presa>0){
    initCa<-as.numeric(parameters["ha"])*(1-(log((1-presa)/presa))/as.numeric(parameters["ka"]))
  }
  
  
  initCap1<-0
  if (presp1*(1-double1)>0){
    initCap1<-as.numeric(parameters["hap1"])*(1-(log((1-presp1)/presp1))/as.numeric(parameters["kp1"]))
  }
  if (presp1*double1>0){
    initCap1<-as.numeric(parameters["hp1"])*(1-(log((1-(presp1/presa))/(presp1/presa)))/as.numeric(parameters["kp1"]))
  }
  
  
  initCap2<-0
  if (presp2*(1-double2)>0){
    initCap2<-as.numeric(parameters["hap2"])*(1-(log((1-presp2)/presp2))/as.numeric(parameters["kp2"]))
  }
  if (presp2*double2>0){
    initCap2<-as.numeric(parameters["hp2"])*(1-(log((1-(presp2/presa))/(presp2/presa)))/as.numeric(parameters["kp2"]))
  }
  
  initCf<-0
  
  
  state <- c(S = initS, IC = initIC, IA = initIA, IU = initIU, R = initR, 
             T1 = initT1, Ca = initCa, Cap1 = initCap1, Cap2 = initCap2, Cf = initCf, Y=0)
  
  # set up a function to solve the model
  modRAI<-function(t, state, parameters) 
  {
    with(as.list(c(state, parameters)),
         {
           
           # swtich on interventions
           covEDATi <- EDATon*covEDATi+(1-EDATon)*covEDAT0
           covITNi <- ITNon*covITNi+(1-ITNon)*covITN0
           covIRSi <- IRSon*covIRSi+(1-IRSon)*covIRS0
           covRCDi <- RCDon*covRCDi+(1-RCDon)*covRCD0
           covCHWi <- CHWon*covCHWi+(1-CHWon)*covCHW0
           
           # define variables
           P <- (S+R+IC+IA+IU+T1)
           seas<-1+amp*cos(2*3.14159*(Y-phi))
           nu <- 1/((1/nuC)+(1/nuA)+(1/nuU))
           beta<-R0*(mu+nu)*seas
           mu_out <- mu+muC+muA+muU
           
           
           timei<-timei-startyear
           ta2ap1<-ta2ap1-startyear
           ta2ap2<-ta2ap2-startyear
           
           wsi<-(1-(Y<=timei))*(Y<=(timei+scalei))*((Y-timei)/scalei)+1*(Y>=(timei+scalei))
           
           covEDAT<-(1-wsi)*covEDAT0+wsi*covEDATi
           covITN<-(1-wsi)*covITN0+wsi*covITNi
           covIRS<-(1-wsi)*covIRS0+wsi*covIRSi
           covRCD<-(1-wsi)*covRCD0+wsi*covRCDi
           covCHW<-(1-wsi)*covCHW0+wsi*covCHWi
           
           
           lam <- (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(IC+rhoa*IA+rhou*IU)/P
           
           ptp <- lam*(ps*S+pr*(R+IA+IU))/(incnm*P+lam*(ps*S+pr*(R+IA+IU)))
           fatig<- kf*(1-covCHW*effCHW)*(1-ptp)
           
           tau1 <- min(1,covEDAT*(1-fatig)*(1+(1-eta)*effRCD*covRCD))
           
           
           resa<-1/(1+exp(-ka*(Ca-ha)/ha))
           resp1<-1/(1+exp(-kp1*(Cap1-hp1)/hp1))
           resp2<-1/(1+exp(-kp2*(Cap2-hp2)/hp2))
           
           resap1<-resa/(1+exp(-kp1*(Cap1-hap1)/hap1))+resp1/(1+exp(-ka*(Cap1-hp1a)/hp1a))
           resap2<-resa/(1+exp(-kp2*(Cap2-hap2)/hap2))+resp2/(1+exp(-ka*(Cap2-hp2a)/hp2a))
           
           
           faila <- fa*resa
           failp1 <- fp1*resp1
           failp2 <- fp2*resp2
           
           failap1 <- fap1*resap1
           failap2 <- fap2*resap2
           
           fail <- (Y<ta2ap1)*faila+(Y>=ta2ap1)*(Y<ta2ap2)*max(faila,failp1,failap1)+(Y>=ta2ap2)*max(faila,failp2,failap2)
           
           treat <- (ps*tau1*lam*S+pr*tau1*lam*R+pr*tau1*lam*IU+pr*tau1*lam*IA)
           
           # rate of change
           dS <- mu*P-mu_out*S+omega*R-lam*S                                                                      #2
           dIC <- muC*P-mu_out*IC+ps*(1-tau1)*lam*S+pr*(1-tau1)*lam*R+pr*(1-tau1)*lam*IU+pr*(1-tau1)*lam*IA-nuC*IC     #3
           dIA <- muA*P-mu_out*IA+(1-ps)*lam*S+(1-pr)*lam*R+(1-pr)*lam*IU-pr*lam*IA+nuC*IC-nuA*IA+fail*nuT1*T1         #4
           dIU <- muU*P-mu_out*IU-lam*IU-nuU*IU+nuA*IA                                                                 #5
           dR <- -mu_out*R-omega*R-lam*R+nuU*IU +(1-fail)*nuT1*T1                                                 #6
           dT1 <- -mu_out*T1+ps*tau1*lam*S+pr*tau1*lam*R+pr*tau1*lam*IU+pr*tau1*lam*IA-nuT1*T1                    #7
           
           dCa <-   (Y<ta2ap1)*treat                                                                          #8
           dCap1 <-  (Y>=ta2ap1)*(Y<ta2ap2)*treat                                                             #9
           dCap2 <-  (Y>=ta2ap2)*treat                                                                        #10
           
           dCf <-  fail*nuT1*T1                                                                               #11
           
           dY <- 1
           
           # return the rate of change
           list(c(dS, dIC, dIA, dIU, dR, dT1, dCa, dCap1, dCap2, dCf, dY))
         }
    ) 
    
  }
  
  out <- ode(y = state, times = times, func = modRAI, parms = parameters)
  
  # MODEL OUTPUTS
  # population
  times<-out[,1]+startyear
  pop<-rowSums(out[,2:7])
  # clinical incidence per 1000 per month
  tci <- rowSums(out[,8:10])
  clinmonth <- tci
  clinmonth[1] <- 0
  clinmonth[2:length(times)] <- 1000*(tci[2:length(times)] - tci[1:(length(times)-1)])/pop
  # percentage prevalence
  prevalence <- 100*rowSums(out[,c(3,4,5,7)])/pop
  # treatment failure rates 
  failmonth <- out[,11]
  failmonth[2:length(times)] <- 1000*(out[2:length(times),11] - out[1:(length(times)-1),11])/pop
  percfail <- 100*failmonth/clinmonth
  
  # treated cases
  t_treat<-tci-tci[1]
  
  # ITN distribution 
  covITNi<-scenario[2]*parameters["covITNi"]+(1-scenario[2])*parameters["covITN0"]
  
  # ITN distribution 
  nITN0<-ceiling((parameters["timei"]-startyear)/parameters["durITN"])
  nITNi<-ceiling((stopyear-parameters["timei"])/parameters["durITN"])-1
  totalnets<-as.numeric(nITN0*parameters["covITN0"]*initP+nITNi*covITNi*initP)
  totalnets
  ITNindex<-(1:(nITN0+nITNi))*(1/dt)*parameters["durITN"]
  dITN<-matrix(covITNi*initP,nrow=1,ncol=(nITN0+nITNi))
  dITN[1:nITN0]<-parameters["covITN0"]*initP
  t_ITN<-0*times
  n_ITN<-t_ITN
  n_ITN[ITNindex]<-dITN
  t_ITN<-cumsum(n_ITN)
  
  
  # IRS events - assume annual
  covIRSi<-scenario[3]*parameters["covIRSi"]+(1-scenario[2])*parameters["covIRS0"]
  
  nIRS0<-ceiling(parameters["timei"]-startyear)
  nIRSi<-ceiling(stopyear-parameters["timei"])-1
  totalIRSperson<-as.numeric(nIRS0*parameters["covIRS0"]*initP+nIRSi*covIRSi*initP)
  totalIRSperson
  IRSindex<-(1:(nIRS0+nIRSi))*(1/dt)
  dIRS<-matrix(covIRSi*initP,nrow=1,ncol=(nIRS0+nIRSi))
  dIRS[1:nIRS0]<-parameters["covIRS0"]*initP
  t_IRS<-0*times
  n_IRS<-t_IRS
  n_IRS[IRSindex]<-dIRS
  t_IRS<-cumsum(n_IRS)
  
  # RCD events
  
  
  RAIout<-matrix(NA,nrow=length(times),ncol=7)
  RAIout[,1]<-times
  RAIout[,2]<-clinmonth
  RAIout[,3]<-prevalence
  RAIout[,4]<-percfail
  RAIout[,5]<-t_treat
  RAIout[,6]<-t_ITN
  RAIout[,7]<-t_IRS
  
  return(RAIout)
}

scenario<-c(0,0,0,0,stopyear,2020,3,0)
RAIout0<-runRAI(scenario)
scenario<-c(1,1,0,0,2025,2020,3,1)
RAIouti<-runRAI(scenario)

times<-RAIout0[,1]
clinmonth<-cbind(RAIout0[,2],RAIouti[,2])
prevalence<-cbind(RAIout0[,3],RAIouti[,3])
percfail<-cbind(RAIout0[,4],RAIouti[,4])
t_treat<-cbind(RAIout0[,5],RAIouti[,5])
t_ITN<-cbind(RAIout0[,6],RAIouti[,6])
t_IRS<-cbind(RAIout0[,7],RAIouti[,7])


# PLOTTING
par(mfrow=c(2,3))
maxy<-50
matplot(times,clinmonth, type='l',lty=1,xlab = "Time",ylab="incidence per 1000 per month",main="incidence",ylim=c(0,maxy),lwd=2)
lines(times,maxy*(times-scenario[6])/scenario[7],col="dark grey",lty=3, lwd=2)
lines(c(scenario[5],scenario[5]),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
maxy<-50
matplot(times,prevalence, type='l',lty=1,xlab = "Time",ylab="% prevalence",main="prevalence",ylim=c(0,maxy),lwd=2)
lines(times,maxy*(times-scenario[6])/scenario[7],col="dark grey",lty=3, lwd=2)
lines(c(scenario[5],scenario[5]),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
maxy<-100
matplot(times,percfail, type='l',lty=1,xlab = "Time",ylab="% failure",main="treatment failure",ylim=c(0,maxy),lwd=2)
lines(times,maxy*(times-scenario[6])/scenario[7],col="dark grey",lty=3, lwd=2)
lines(c(scenario[5],scenario[5]),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
maxy<-max(t_treat)
matplot(times,t_treat, type='l',lty=1,xlab = "Time",ylab="cases",main="cumulative treated cases",ylim=c(0,maxy),lwd=2)
lines(times,maxy*(times-scenario[6])/scenario[7],col="dark grey",lty=3, lwd=2)
lines(c(scenario[5],scenario[5]),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
maxy<-max(t_ITN)
matplot(times,t_ITN, type='l',lty=1,xlab = "Time",ylab="ITNs",main="cumulative ITNs distributed",ylim=c(0,maxy),lwd=2)
lines(times,maxy*(times-scenario[6])/scenario[7],col="dark grey",lty=3, lwd=2)
lines(c(scenario[5],scenario[5]),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
maxy<-max(t_IRS)
matplot(times,t_IRS, type='l',lty=1,xlab = "Time",ylab="IRS per person",main="cumulative IRS",ylim=c(0,maxy),lwd=2)
lines(times,maxy*(times-scenario[6])/scenario[7],col="dark grey",lty=3, lwd=2)
lines(c(scenario[5],scenario[5]),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)


# STILL TO DO
# outputs for extension of VMW role (see CHW parameters and variables)
# how to cost RCD and other interventions
# define front end: baseline scenario choice, features of drugs, costs, 
# MSAT and MDA
  # COSTs
  # VMW costs 
    # - installation, training, support, infrastructure
    # - unit costs of tests malaria and non-mal febrile
    # - unit costs of treatments
  # CHW role extension costs
    # - YOEL
  # ITN costs
    # - ditribution costs
    # - unit costs of nets
  # RCD costs
    # - investigation cost per reported case
    # - unit costs for tests around reported case (how many tests per investigation)
    # - unit costs for treatment of positive cases
  # Switching to new ACT cost
    # YOEL






