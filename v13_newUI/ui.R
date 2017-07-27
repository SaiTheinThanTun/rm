library(shiny)
library(shinythemes)

useShinyjs()

fluidPage(
  theme=shinytheme("cosmo"),
  
  headerPanel(title="MALMOD (beta)"),
  
  sidebarPanel(
    conditionalPanel(condition="input.tabs == 'A'",
                     # language selection
                     radioButtons("language", NULL,
                                  choiceNames = mapply(languages, flags, FUN = function(languages, flagUrl) {
                                    tagList(
                                      tags$img(src=flagUrl, width=20, height=20),
                                      languages
                                    )
                                  }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                                  choiceValues = languages_short,
                                  inline=TRUE
                     )),
    
    conditionalPanel(condition="input.tabs == 'B'",
                     HTML("Provide basic info on the Area.<br>"),
                     uiOutput("area_name_render"),
                     uiOutput("baseline_population_render"),
                     uiOutput("API_render")
    ),
    
    conditionalPanel(condition="input.tabs == 'C'",
                     tags$h3('Calibration'),
                     tags$p("Adjust the “calibration” parameter until the API line (dark blue, flat) passes through the centre of the model prediction for monthly incidence of confirmed cases line (the lower boundary of the blue wavy line)."),
                     uiOutput("bh_max"),
                     actionButton("run_calibration", "Run Calibration", class = "btn-primary")
    ),
    
    
    conditionalPanel(condition="input.tabs == 'R'",
                     HTML('Create and assses a "package" by (1) selecting one or several interventions, (2) providing a name for the package and (3) running the simulation'),
                     tags$h3('Interventions currently available'),
                     
                     # EDAT
                     # selection and access to advanced parameters
                     fixedRow(column(width = 9, uiOutput("EDATon_render")), 
                              column(width = 3, conditionalPanel(
                                condition = "input['EDATon']", uiOutput("edat_advanced_settings")))),
                     fluidRow(conditionalPanel(
                       condition = "input['EDATon']", htmlOutput("edat_parameters"))),
                     
                     
                     # ITN
                     # selection and access to advanced parameters
                     fixedRow(column(width = 9, uiOutput("ITNon_render")), 
                              column(width = 3, conditionalPanel(
                                condition = "input['ITNon']", uiOutput("itn_advanced_settings")))),
                     fluidRow(conditionalPanel(
                       condition = "input['ITNon']", htmlOutput("itn_parameters"))),
                     
                     
                     
                     # RCD
                     # selection and access to advanced parameters
                     fixedRow(column(width = 9, uiOutput("RCDon_render")), 
                              column(width = 3, conditionalPanel(
                                condition = "input['RCDon']", uiOutput("rcd_advanced_settings")))),
                     fluidRow(conditionalPanel(
                       condition = "input['RCDon']", htmlOutput("rcd_parameters"))),
                     
                     
                     
                     # IRS
                     # selection and access to advanced parameters
                     fixedRow(column(width = 9, uiOutput("IRSon_render")), 
                              column(width = 3, conditionalPanel(
                                condition = "input['IRSon']", uiOutput("irs_advanced_settings")))),
                     fluidRow(conditionalPanel(
                       condition = "input['IRSon']", htmlOutput("irs_parameters"))),
                     
                     
                     tags$br(),
                     tags$h3('Interventions under trial'),
                     
                     # MSAT
                     fixedRow(column(width = 9, uiOutput("MSATon_render")), 
                              column(width = 3, conditionalPanel(
                                condition = "input['MSATon']", uiOutput("msat_advanced_settings")))),
                     fluidRow(conditionalPanel(
                       condition = "input['MSATon']", htmlOutput("msat_parameters"))),
                     
                     
                     # MDA
                     fixedRow(column(width = 9, uiOutput("MDAon_render")), 
                              column(width = 3, conditionalPanel(
                                condition = "input['MDAon']", uiOutput("mda_advanced_settings")))),
                     fluidRow(conditionalPanel(
                       condition = "input['MDAon']", htmlOutput("mda_parameters"))),
                     
                     
                     # Button run simulation
                     tags$br(), tags$br(),
                     uiOutput("package_name_render"),
                     uiOutput("run_simul_render")
                     
    )
  ),
  mainPanel(
    tabsetPanel(id="tabs", selected="A",
                tabPanel("About", value="A",
                         conditionalPanel(
                           condition = "input['language'] == 'en'",
                           includeMarkdown('./www/markdown/about_en.md')),
                         conditionalPanel(
                           condition = "input['language'] == 'th'",
                           includeMarkdown('./www/markdown/about_th.md'))
                         
                ),
                tabPanel("Baseline set-up", value="B",
                         fluidRow(
                           column(width=12,
                                  HTML('<br>Set-up baseline parameters for the selected area. Some contextual info will appear on hoover or you can '),
                                  actionLink("baseline_show", "read more info on the parameters below.")
                           ),
                           
                           column(width=4,
                                  tags$h3('→ Vector Control'),
                                  uiOutput("covITN0_render"),
                                  uiOutput("effITN_render"),
                                  uiOutput("covIRS0_render"),
                                  uiOutput("effIRS_render")
                           ),
                           column(width=4,
                                  tags$h3('→ Treatment'),
                                  uiOutput("covEDAT0_render"),
                                  uiOutput("percfail2018_render"),
                                  uiOutput("percfail2019_render"),
                                  uiOutput("percfail2020_render")
                           ),
                           
                           column(width=4,
                                  tags$h3('→ Transmission'),
                                  uiOutput("eta_render"),
                                  uiOutput("muC_render"),
                                  uiOutput("muA_render"),
                                  uiOutput("muU_render")
                           )
                         )
                         
                ),
                tabPanel("Calibration", value="C",
                         plotOutput('plot_calibration'),
                         plotOutput('plot_calibration_sai')
                ),
                tabPanel("Package(s)", value="R",
                         plotOutput('plot_baseline_interventions'),
                         htmlOutput('text_package'),
                         plotOutput('plot_baseline_interventions_sai'),
                         
                         tags$br(), tags$br(), 
                         uiOutput("all_packages_checkbox_render"),
                         uiOutput("remove_packages_render")
                ),
                
                tabPanel("Report/Bookmark", value="RB",
                         fluidRow(
                           column(width=5,
                                  HTML("You can generate a pdf report with the baseline data and all the packages tested during this session:"),
                                  tags$br(),
                                  downloadButton("report", 'Generate Report', class = "btn-primary")),
                           column(width=2, HTML("<br>")),
                           column(width=5,
                                  HTML("You can bookmark parameters"),
                                  actionLink("bookmark_help", "(how does bookmarking work?)"),
                                  HTML("and use them during another session:"),
                                  tags$br(),
                                  bookmarkButton(label="Bookmark Parameters")
                                  
                           )
                         )
                         
                )
    )
  )
)