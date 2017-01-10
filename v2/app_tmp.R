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
                    wellPanel(
                      
                    )
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             )
    ),
    tabPanel(title = strong("Intervention parameters tab 2"),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             )
    ),
    tabPanel(title = strong("Typology parameters"),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             )
    ),
    tabPanel(title = strong("Biological parameters"),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
             ),
             column(3,
                    
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