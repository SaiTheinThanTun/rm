#TEMPLATE FOR ODE FUNCTIONS LAO/RAI ModelS
#shiny version of 
library(deSolve)
library(shiny)
library(TSA)


ui <- fluidPage(
  tabsetPanel(
    id="panels",
    tabPanel(title = strong("Intervention parameters tab 1"),
             column(3,
                    numericInput(inputId="timei", label = "timing of intervention [N]", value = 2018, min=NA, max=NA),
                    checkboxInput(inputId="EDATon", label = "switch on scale up of EDAT [C]", value = TRUE),
                    sliderInput(inputId="EDATscale", label = "years to scale up EDAT [1 to 3]", value = 3, min=1, max=3),
                    sliderInput(inputId="covEDATi", label = "new percentage of all villages covered by VMW [0 to 100]", value = 90, min=0, max=100)
                    
             ),
             column(3,
                    
                    checkboxInput(inputId="ITNon", label = "switch on scale up of ITN [C]", value = TRUE),
                    sliderInput(inputId="ITNscale", label = "years to scale up ITN [1 to 3]", value = 0.5, min=1, max=3),
                    sliderInput(inputId="covITNi", label = "new coverage of ITN (%) [0 to 90]", value = 90, min=0, max=90),
                    sliderInput(inputId="effITN", label = "percentage of new infections averted due to owenership of ITN [0 to 50]", value = 30, min=0, max=50)
             ),
             column(3,
                    checkboxInput(inputId="RCDon", label = "switch on scale up of RCD default radial search   [C]", value = TRUE),
                    sliderInput(inputId="RCDscale", label = "years to scale up RCD [1 to 3]", value = 2, min=1, max=3),
                    checkboxInput(inputId="RCDcoex", label = "Change RCD to co-exposure search   [C]", value = TRUE),
                    sliderInput(inputId="covRCDi", label = "new coverage of RCD (%) [0 to 100]", value = 50, min=0, max=100),
                    sliderInput(inputId="effRCD", label = "number of additional clinical cases found for each index case [0 to 2]", value = 1, min=0, max=2)
             ),
             column(3,
                    checkboxInput(inputId="IRSon", label = "switch on scale up of IRS [C]", value = TRUE),
                    sliderInput(inputId="IRSscale", label = "years to scale up IRS [1 to 3]", value = 1, min=1, max=3),
                    sliderInput(inputId="covIRSi", label = "new coverage of IRS (%) [0 to 90]", value = 90, min=0, max=90),
                    sliderInput(inputId="effIRS", label = "% reduction in risk provided by IRS [0 to 25]", value = 15, min=0, max=25)
             )
    ),
    tabPanel(title = strong("Intervention parameters tab 2"),
             column(3,
                    checkboxInput(inputId="MDAon", label = "switch on MDA", value = TRUE), #6
                    sliderInput(inputId="cmda_1", label = "effective population coverage of focal MDA in round 1 [0 to 100]", value = 50, min=0, max=100),
                    sliderInput(inputId="cmda_2", label = "effective population coverage of focal MDA in round 2 [0 to 100]", value = 50, min=0, max=100),
                    sliderInput(inputId="cmda_3", label = "effective population coverage of focal MDA in round 3 [0 to 100]", value = 50, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="tm_1", label = "timing of first round [2018 to 2021 - 1 month steps]", value = 0, min=0, max=36),
                    sliderInput(inputId="tm_2", label = "timing of second round [2018+(1/12) to 2021 - 1 month steps]", value = 1, min=1, max=36),
                    sliderInput(inputId="tm_3", label = "timing of third round [2018+(2/12) to 2021 - 1 month steps]", value = 2, min=2, max=36),
                    sliderInput(inputId="dm", label = "number of months taken to reach population target coverage in each round [15 to 24]", value = 6, min=15, max=24)
             ),
             column(3,
                    sliderInput(inputId="lossd", label = "number of days of profilaxis provided by the ACT [7 to 30]", value = 30, min=7, max=30),
                    numericInput(inputId="cm_1", label = "% popultion coverage of first MDA round [N]", value = 80, min=NA, max=NA),
                    numericInput(inputId="cm_2", label = "% of people from first MDA round who receieve the second [N]", value = 95, min=NA, max=NA),
                    numericInput(inputId="cm_3", label = "% of people from second MDA round who receieve the third [N]", value = 95, min=NA, max=NA)
             ),
             column(3,
                    numericInput(inputId="effv_1", label = ",                  # protective efficacy of a single dose of RTS,S [N]", value = 0, min=NA, max=NA),
                    numericInput(inputId="effv_2", label = "0,                 # protective efficacy of two doses of RTS,S [N]", value = 0, min=NA, max=NA),
                    numericInput(inputId="effv_3", label = "0,                 # protective efficacy of three doses of RTS,S [N]", value = 0, min=NA, max=NA),
                    numericInput(inputId="dv", label = "duration of vaccine protection [N]", value = 1, min=NA, max=NA),
                    
                    checkboxInput(inputId="primon", label = "ACT+primaquine for EDAT and MDA [C]", value = TRUE)
             )
    ),
    tabPanel(title = strong("Typology parameters"),
             column(3,
                    numericInput(inputId="R0", label = "basic reproduction number", value = 2.50, min=NA, max=NA),
                    sliderInput(inputId="eta", label = "percentage of all infections that are caught in the outside the village (forrest) [0 to 100]", value = 50, min=0, max=100),
                    sliderInput(inputId="covEDAT0", label = "baseline percentage of all villages with VMW [0 to 100]", value = 30, min=0, max=100)
             ),
             column(3,
                    numericInput(inputId="nuTr", label = "days of infectiosness after treatment ACT [N]", value = 14, min=NA, max=NA),
                    numericInput(inputId="nuTrp", label = "days of infectiosness after treatment ACT+primaquine [N]", value = 7, min=NA, max=NA),
                    sliderInput(inputId="covITN0", label = "baseline coverage of ITN (%) [0 to 90]", value = 60, min=0, max=90),
                    sliderInput(inputId="covRCD0", label = "baseline coverage of RCD (%) [0 to 90]", value = 0, min=0, max=90),
                    sliderInput(inputId="covIRS0", label = "baseline coverage of IRS (%) [0 to 90]", value = 0, min=0, max=90)
             ),
             column(3,
                    numericInput(inputId="amp", label = "relative amplitude seasonality [N]", value = 0.7, min=NA, max=NA),
                    numericInput(inputId="phi", label = "phase angle seasonality [N]", value = 0.5, min=NA, max=NA),
                    sliderInput(inputId="muC", label = "number of imported clinical cases per 1000 population per year [0 to 10]", value = 5, min=0, max=10),
                    sliderInput(inputId="muA", label = "number of imported super-microscopic asymtomatic infection per 1000 population per year [0 to 100]", value = 50, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="muU", label = "number of imported sub-microscopic asymtomatic infections per 1000 population per year [0 to 100]", value = 50, min=0, max=100),
                    sliderInput(inputId="percfail2018", label = "percentage of cases failing treatment in 2018 and before [0 to 100]", value = 30, min=0, max=100),
                    sliderInput(inputId="percfail2019", label = "percentage of cases failing treatment in 2019  [0 to 100]", value = 10, min=0, max=100),
                    sliderInput(inputId="percfail2020", label = "percentage of cases failing treatment in 2020 and after  [0 to 100]", value = 20, min=0, max=100)
             )
    ),
    tabPanel(title = strong("Biological parameters"),
             column(3,
                    numericInput(inputId="omega", label = "average duration of immunity (years) [N]", value = 2, min=NA, max=NA),
                    numericInput(inputId="nuC", label = "days of symptoms in the absence of treatment [N]", value = 9, min=NA, max=NA),
                    numericInput(inputId="nuA", label = "days of super-microscopic asymtomatic infection [N]", value = 60, min=NA, max=NA)
             ),
             column(3,
                    numericInput(inputId="nuU", label = "days of sub-microscopic asymtomatic infection [N]", value = 60, min=NA, max=NA),
                    numericInput(inputId="rhoa", label = "relative infectivity of super-microscopic asymptomatic infections compared with clinical infections (%) [N]", value = 70, min=NA, max=NA)
             ),
             column(3,
                    numericInput(inputId="rhou", label = "relative infectivity of sub-microscopic asymptomatic infections compared with clinical infections (%) [N]", value = 30, min=NA, max=NA),
                    numericInput(inputId="ps", label = "% of all non-immune new infections that are clinical [N]", value = 90, min=NA, max=NA)
             ),
             column(3,
                    numericInput(inputId="pr", label = "% of all immune new infections that are clinical [N]", value = 20, min=NA, max=NA),
                    numericInput(inputId="mu", label = "life expectancy (years) [N]", value = 50, min=NA, max=NA)
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
  fluidRow(h4("Legend. Black lines: baseline scenario. Red lines: model result with the selected parameter values."))
  
)

server <- function(input, output) {
  output$MODEL <- renderPlot({
    
  })
}

shinyApp(ui = ui, server = server)