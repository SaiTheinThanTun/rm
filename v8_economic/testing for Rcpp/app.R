#library(rstan)
library(shiny)
library(Rcpp)
library(deSolve)
#sourceCpp("D:/Dropbox/IBM project_Sai/RAI model/v8_economic/testing for Rcpp/lorenz.cpp")
sourceCpp("lorenz.cpp")
#expose_stan_functions("lorenz.stan")

ui <- fluidPage(
  sliderInput("beta", label="Beta",value = 2.6666, min=0, max=5),
  sliderInput("sigma", label="sigma",value = 10, min=0, max=15),
  sliderInput("rho", label="rho",value = 28, min=0, max=40),
  numericInput("stateA", label="state A", value=-10),
  numericInput("stateB", label="state B", value=-12),
  numericInput("stateC", label="state C", value=30.05),
  plotOutput("plot1")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    parameters1<-c(input$beta,input$sigma,input$rho)
    
    # x=-10, y=-12, z=30.05
    init.state<-c(input$stateA,input$stateB,input$stateC, 0, 0, 0)
    
    nderivs<-function(t,state,parameters){
      tmp<-lorenz(t,state,parameters)
      return(list(tmp))
    }
    
    run1<-ode(y=init.state,times=seq(0,100,0.01), func=nderivs, parms=parameters1)
    plot(run1)
  })
  
}

shinyApp(ui = ui, server = server)


# parms[1]=beta ,[2]=sigma,[3]=rho
