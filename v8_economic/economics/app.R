library(deSolve)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3,
           sliderInput(inputId = "pop",label = "Total population",value = 50000, min = 0, max = 1000000),
           sliderInput(inputId = "No_villages",label = "Number of villages",value = 56, min = 1, max = 50000)
    ),
    column(3,
           sliderInput(inputId = "prop_vilCE",label = "Proportion of villages visited for community engagement",value = 76, min = 1, max = 100),
           sliderInput(inputId = "prop_vilqPCR",label = "Proportion of villages surveyed",value = 80, min = 1, max = 100),
           sliderInput(inputId = "prop_vilMDA",label = "Proportion of surveyed villages offered MDA",value = 20, min = 1, max = 100)
    ), 
    column(3,
           sliderInput(inputId = "TMT_events",label = "Number of TMT events",value = 1, min = 1, max = 5),
           sliderInput(inputId = "rounds",label = "Number of rounds of MDA per TMT events",value = 3, min = 1, max = 3),
           sliderInput(inputId = "TMT_uptake",label = "Mean TMT uptake",value = 3, min = 1, max = 3)
          ),
    column(3,
           sliderInput(inputId = "length",label = "length of MDA programme",value = 2018, min = 2018, max = 2022),
           sliderInput(inputId= "inf",label = "annual inflation rate",value = 3, min = 0, max = 10),
           sliderInput(inputId= "sal_increase",label = "annual salary increment",value = 2, min = 0, max = 10)
           )
    ),
  fluidRow(
        column(12,
           h3("Baseline parameters"),
           mainPanel(tableOutput("value"))
        )
  )
 )

Wdays_per_month       <- 20
No_Days_CE_village    <- 1
No_Days_Travel_CEvil  <- 1/2
No_Days_qPCR_village  <- 1
No_Days_Travel_qPCRvil<- 1/2
No_Days_MDA_village   <- 5
No_Days_Travel_MDAvil <- 1/2
No_Days_ITN_village   <- 1/2
No_Days_Travel_ITNvil <- 1/2

StaffType_Salary <- c(250,120,500,150,8000,5000,3500,1000,200,50)


StaffType_CE   <- c(3,0,0,0,0,0,0,0,0,0)
StaffType_qPCR <- c(2,0,1,0,0,0,0,0,1,0)
StaffType_MDA  <- c(2,1,1,0,0,0,0,0,0,0)
StaffType_ITN  <- c(1,1,0,0,0,0,0,0,0,0)

TotalCost_CEpervil <-(StaffType_Salary/Wdays_per_month)* StaffType_CE* (No_Days_CE_village+No_Days_Travel_CEvil)
TotalCost_qPCRpervil <-(StaffType_Salary/Wdays_per_month)* StaffType_qPCR* (No_Days_qPCR_village+No_Days_Travel_qPCRvil)
TotalCost_MDApervil <-(StaffType_Salary/Wdays_per_month)* StaffType_MDA* (No_Days_MDA_village+No_Days_Travel_MDAvil)
TotalCost_ITNpervil <-(StaffType_Salary/Wdays_per_month)* StaffType_ITN* (No_Days_ITN_village+No_Days_Travel_ITNvil)

sum(TotalCost_CEpervil)
sum(TotalCost_qPCRpervil)
sum(TotalCost_MDApervil)
sum(TotalCost_ITNpervil)


server <- function(input, output) {
  sliderValues <- reactive({
   data.frame(
     Name=c("Population", "Number of villages", "Number of villages for CE", "Number of villages surveyed", "Number of villages for MDA", "Number of rounds of MDA per TMT events"),
     Value=c(pop=input$pop,
      No_villages=input$No_villages,
      prop_vilCE=ceiling(input$No_villages*(input$prop_vilCE/100)),
      prop_vilqPCR = ceiling(input$No_villages*(input$prop_vilqPCR/100)),
      prop_vilMDA=ceiling(input$No_villages*(input$prop_vilMDA/100)),
      MDA_rounds=input$rounds
      )
      )})

  
  
  output$value <- renderTable({
    sliderValues()
    
  })
}

shinyApp(ui = ui, server = server)
