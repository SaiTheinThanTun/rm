library(shiny)
library(shinythemes)

useShinyjs()

fluidPage(
  theme=shinytheme("simplex"),
  
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
                     uiOutput("baseline_API_render")
    ),
    
    conditionalPanel(condition="input.tabs == 'C'",
                     tags$h3('Calibration'),
                     tags$p("Adjust the “calibration” parameter until the API line (dark blue, flat) passes through the centre of the model prediction for monthly incidence of confirmed cases line (the lower boundary of the blue wavy line)."),
                     uiOutput("baseline_calibration"),
                     actionButton("run_calibration", "Run Calibration", class = "btn-primary")
    ),
    
    
    conditionalPanel(condition="input.tabs == 'R'",
                     HTML('Create and assses a "package" by (1) selecting one or several interventions, (2) providing a name for the package and (3) running the simulation'),
                     tags$h3('Interventions currently available'),
                     
                     # EDAT
                     # selection and access to advanced parameters
                     uiOutput("edat_yes_render"),
                     fluidRow(
                       column(width = 7,
                              conditionalPanel(
                                condition = "input['edat_yes']",
                                htmlOutput("edat_parameters"))),
                       column(width = 2, offset = 2,  
                              conditionalPanel(
                                condition = "input['edat_yes']",
                                uiOutput("edat_advanced_settings")))
                       ),
                     
                     
                     
                     
                     # uiOutput("edat_advanced_render"),
                     
                     
                     # ITN
                     # selection and access to advanced parameters
                     uiOutput("itn_yes_render"),
                     
                     conditionalPanel(
                       condition = "input['itn_yes']",
                       htmlOutput("itn_parameters")
                     ),
                     
                     uiOutput("itn_advanced_render"),
                     
                     # RCD
                     # selection and access to advanced parameters
                     uiOutput("rcd_yes_render"),
                     
                     conditionalPanel(
                       condition = "input['rcd_yes']",
                       htmlOutput("rcd_parameters")
                     ),
                     
                     uiOutput("rcd_advanced_render"),
                     
                     
                     # IRS
                     # selection and access to advanced parameters
                     uiOutput("irs_yes_render"),
                     
                     conditionalPanel(
                       condition = "input['irs_yes']",
                       htmlOutput("irs_parameters")
                     ),
                     
                     uiOutput("irs_advanced_render"),
                     
                     tags$br(),
                     tags$h3('Interventions under trial'),
                     
                     # MSAT
                     uiOutput("msat_yes_render"),
                     
                     conditionalPanel(
                       condition = "input['msat_yes']",
                       htmlOutput("msat_parameters")
                     ),
                     
                     uiOutput("msat_advanced_render"),
                     
                     
                     # MDA
                     uiOutput("mda_yes_render"),
                     
                     conditionalPanel(
                       condition = "input['mda_yes']",
                       htmlOutput("mda_parameters")
                     ),
                     
                     uiOutput("mda_advanced_render"),
                     
                     
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
                                  uiOutput("baseline_cov_ITN_render"),
                                  uiOutput("baseline_avert_ITN_render"),
                                  uiOutput("baseline_cov_IRS_render"),
                                  uiOutput("baseline_avert_IRS_render")
                           ),
                           column(width=4,
                                  tags$h3('→ Treatment'),
                                  uiOutput("baseline_treatment_render"),
                                  uiOutput("baseline_treat_fail_2018_render"),
                                  uiOutput("baseline_treat_fail_2019_render"),
                                  uiOutput("baseline_treat_fail_2020_render")
                           ),
                           
                           column(width=4,
                                  tags$h3('→ Transmission'),
                                  uiOutput("baseline_trans_forest_render"),
                                  uiOutput("baseline_trans_import_render"),
                                  uiOutput("baseline_trans_detect_render"),
                                  uiOutput("baseline_trans_undetect_render")
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