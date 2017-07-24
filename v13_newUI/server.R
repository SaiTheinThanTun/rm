shinyServer(
  function(session, input, output) {

    # Reactive values
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    explanation <- reactive(switch(input$language,
                                   "en" = "input_explanation_en",
                                   "th" = "input_explanation_th"))
    
    values <- reactiveValues(primon = as.logical(meta[meta$input_id=="primon", "default_value"]),
                             EDATscale = as.numeric(meta[meta$input_id=="EDATscale", "default_value"]),
                             covEDATi = as.numeric(meta[meta$input_id=="covEDATi", "default_value"]),
                             ITNscale = as.numeric(meta[meta$input_id=="ITNscale", "default_value"]),
                             covITNi = as.numeric(meta[meta$input_id=="covITNi", "default_value"]),
                             RCDscale = as.numeric(meta[meta$input_id=="RCDscale", "default_value"]),
                             covRCDi = as.numeric(meta[meta$input_id=="covRCDi", "default_value"]),
                             delayRCD = as.numeric(meta[meta$input_id=="delayRCD", "default_value"]),
                             RCDcoex = as.numeric(meta[meta$input_id=="RCDcoex", "default_value"]),
                             RCDrad = as.numeric(meta[meta$input_id=="RCDrad", "default_value"]),
                             clustRCDrad = as.numeric(meta[meta$input_id=="clustRCDrad", "default_value"]),
                             RCDs = as.numeric(meta[meta$input_id=="RCDs", "default_value"]),
                             clustRCDcoex = as.numeric(meta[meta$input_id=="clustRCDcoex", "default_value"]),
                             RCDsensC = as.numeric(meta[meta$input_id=="RCDsensC", "default_value"]),
                             RCDsensA = as.numeric(meta[meta$input_id=="RCDsensA", "default_value"]),
                             RCDsensU = as.numeric(meta[meta$input_id=="RCDsensU", "default_value"]),
                             IRSscale = as.numeric(meta[meta$input_id=="IRSscale", "default_value"]),
                             covIRSi = as.numeric(meta[meta$input_id=="covIRSi", "default_value"]),
                             MSATscale = as.numeric(meta[meta$input_id=="MSATscale", "default_value"]),
                             covMSATi = as.numeric(meta[meta$input_id=="covMSATi", "default_value"]),
                             MSATsensC = as.numeric(meta[meta$input_id=="MSATsensC", "default_value"]),
                             MSATsensA = as.numeric(meta[meta$input_id=="MSATsensA", "default_value"]),
                             MSATsensU = as.numeric(meta[meta$input_id=="MSATsensU", "default_value"]),
                             lossd = as.numeric(meta[meta$input_id=="lossd", "default_value"]),
                             dm = as.numeric(meta[meta$input_id=="dm", "default_value"]),
                             cmda_1 = as.numeric(meta[meta$input_id=="cmda_1", "default_value"]),
                             cmda_2 = as.numeric(meta[meta$input_id=="cmda_2", "default_value"]),
                             cmda_3 = as.numeric(meta[meta$input_id=="cmda_3", "default_value"]),
                             tm_1 = as.numeric(meta[meta$input_id=="tm_1", "default_value"]),
                             tm_2 = as.numeric(meta[meta$input_id=="tm_2", "default_value"]),
                             tm_3 = as.numeric(meta[meta$input_id=="tm_3", "default_value"])
    )
    
    # Baseline set-up section
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # area_name
    output$area_name_render <- renderUI({
      var <- "area_name"
      tagList(
        textInput(var, label=meta[meta$input_id==var, "label"], value=as.numeric(meta[meta$input_id==var, "default_value"])),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body"))
      )
    })
    
    # baseline_population
    output$baseline_population_render <- renderUI({
      var <- "baseline_population"
      tagList(
        numericInput(var, label=meta[meta$input_id==var, "label"], value=as.numeric(meta[meta$input_id==var, "default_value"]), 
                     min=as.numeric(meta[meta$input_id==var, "min"]),
                     max=as.numeric(meta[meta$input_id==var, "max"])),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body"))
      )
    })
    
    # API
    output$API_render <- renderUI({
      var <- "API"
      tagList(
        numericInput(var, label=meta[meta$input_id==var, "label"], value=as.numeric(meta[meta$input_id==var, "default_value"]), 
                     min=as.numeric(meta[meta$input_id==var, "min"]),
                     max=as.numeric(meta[meta$input_id==var, "max"])),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body"))
      )
    })
    
    # markdown document
    observeEvent(input$baseline_show, {
      showModal(modalDialog(
        title = "About", size="l",
        if(input$language=='en'){includeMarkdown('./www/markdown/baseline_en.md')},
        if(input$language=='th'){includeMarkdown('./www/markdown/baseline_th.md')},
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    
    # Vector Control
    output$covITN0_render <- renderUI({
      var <- "covITN0"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    
    output$effITN_render <- renderUI({
      var <- "effITN"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    output$covIRS0_render <- renderUI({
      var <- "covIRS0"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    output$effIRS_render <- renderUI({
      var <- "effIRS"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    # Treatment
    output$covEDAT0_render <- renderUI({
      var <- "covEDAT0"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    output$percfail2018_render <- renderUI({
      var <- "percfail2018"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    output$percfail2019_render <- renderUI({
      var <- "percfail2019"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    output$percfail2020_render <- renderUI({
      var <- "percfail2020"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    
    # Transmission
    output$eta_render <- renderUI({
      var <- "eta"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="left", options = list(container = "body")))
    })
    
    output$muC_render <- renderUI({
      var <- "muC"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="left", options = list(container = "body")))
    })
    
    output$muA_render <- renderUI({
      var <- "muA"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="left", options = list(container = "body")))
    })
    
    output$muU_render <- renderUI({
      var <- "muU"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="left", options = list(container = "body")))
    })
    
    
    # help on bookmarking
    
    observeEvent(input$bookmark_help, {
      showModal(modalDialog(
        title = "About", size="l",
        if(input$language=='en'){includeMarkdown("./www/markdown/help_bookmark.md")},
        if(input$language=='th'){includeMarkdown("./www/markdown/help_bookmark.md")},
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    
    
    
    # Calibration
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # bh_max
    output$bh_max <- renderUI({
      var <- "bh_max"
      tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
    })
    
    
    # Invalidation when baseline parameters change
    observeEvent({
      input$API
      input$baseline_population
      input$covITN0
      input$effITN
      input$covIRS0
      input$effIRS
      input$covEDAT0
      input$percfail2018
      input$percfail2019
      input$percfail2020
      input$eta
      input$muC
      input$muA
      input$muU
    }, 
    ignoreInit=TRUE, 
    {
      output$plot_calibration <- renderPlot({
        plot(1, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='')
        text(1, 1, 'Set the calibration parameter and then "Run calibration".', cex=2)
      })
      
    })
    
    
    # Plot/Text Baseline section
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # text_baseline_params <- reactiveVal(value="hello world")
    
    observeEvent(input$run_calibration,{
      
      output$plot_calibration <- renderPlot({
        if(input$run_calibration == 0) return({
          plot(1, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='')
          text(1, 1, 'Select a parameter value and "Run Calibration"', cex=2)
        })
        
        ggplot(rv$df %>% filter(what=="Baseline"), aes(time, value, color=what)) +
          geom_point() +
          geom_line() +
          ggtitle("Random plot") +
          theme(legend.position = "bottom" )
      })
      
      output$plot_calibration_sai <- renderPlot({
        plot(1)
      })
      
      # output$text_baseline <- renderText({
      #   # text_baseline_params(
      #   paste0(
      #     "<p>Baseline data summary:</p>",
      #     "<ul>",
      #     "<li>", meta$label[3], ": ", input$API, "</li>",
      #     "<li>", meta$label[4], ": ", input$baseline_bites_night, "</li>",
      #     "<li>...</li>",
      #     "</ul>")
      #   # )
      #   # return(text_baseline_params())
      # })
    })
    
    
    # Interventions
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Interventions/EDAT
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # EDAT Yes/No checkbox
    output$EDATon_render <- renderUI({
      checkboxInput("EDATon", label=tags$span(class="label label-default", meta[meta$input_id=="EDATon", "label"]), value=as.logical(meta[meta$input_id=="EDATon", "default_value"]))
    })
    
    # EDAT summary text of parameters values
    output$edat_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", ifelse(values$primon, "With ACT and Primaquine", "Without ACT and Primaquine"), "</li>",
               "<li>", values$EDATscale, " year(s) to scale up EDAT", "</li>",
               "<li>New percentage of all clinical cases treated: ", values$covEDATi, "%", "</li>",
               "</ul>")
      )
    })
    
    # EDAT button to provide access to advanced parameters
    output$edat_advanced_render <- renderUI({
      if (!isTRUE(input$EDATon)) return()
      actionButton("edat_advanced", "EDAT Parameters", icon = icon("external-link"))
    })
    
    # EDAT modal dialogue with access to parameters
    output$edat_advanced_settings <- renderUI({

      var <- "primon"
      tag_1 <- tagList(
        do.call(checkboxInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.logical(meta[meta$input_id==var, "default_value"])
          )),
        bsTooltip(var, title = meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "EDATscale"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsTooltip(var, title = meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "covEDATi"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsTooltip(var, title = meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      dropdownButton(tag_1, tag_2, tag_3,
                     status = "info", icon = icon("gear"), size = "sm", circle = FALSE, right = FALSE, width = "400px")
    })
    
    # update EDAT patameters on update button
    observe({
      # removeModal(session)
      values$primon <- input$primon
      values$EDATscale <- input$EDATscale
      values$covEDATi <- input$covEDATi
    })
    
    
    
    # Interventions/ITN
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # ITN Yes/No checkbox
    output$ITNon_render <- renderUI({
      checkboxInput("ITNon", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="ITNon", "label"]))), value=as.logical(meta[meta$input_id=="ITNon", "default_value"]))
    })
    
    # ITN summary text of parameters values
    output$itn_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", values$ITNscale, " year(s) to scale up ITN", "</li>",
               "<li>New coverage of ITNs: ", values$covITNi, "%", "</li>",
               "</ul>")
      )
    })
    
    # ITN button to provide access to advanced parameters
    output$itn_advanced_render <- renderUI({
      if (!isTRUE(input$ITNon)) return()
      actionButton("itn_advanced", "ITN Parameters", icon = icon("external-link"))
    })
    
    # ITN modal dialogue with access to parameters
    observeEvent(input$itn_advanced, {
      
      var <- "ITNscale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "covITNi"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      showModal(modalDialog(title = "ITN Parameters", size = "s", easyClose = FALSE, footer = modalButton("Cancel"),
                            tag_1,
                            tag_2,
                            actionButton('update_itn', "Update")
      ))
    })
    
    # update ITN parameters on update button
    observeEvent(input$update_itn,{
      removeModal(session)
      values$ITNscale <- input$ITNscale
      values$covITNi <- input$covITNi
    })
    
    
    
    
    
    # Interventions/RCD
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # RCD Yes/No checkbox
    output$RCDon_render <- renderUI({
      checkboxInput("RCDon", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="RCDon", "label"]))), value=as.logical(meta[meta$input_id=="ITNon", "default_value"]))
    })
    
    # RCD summary text of parameters values
    output$rcd_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", values$RCDscale, " year(s) to scale up RCD", "</li>",
               "<li>", "New coverage of RCD: ", values$covRCDi, "%", "</li>",
               "<li>", "Reaction time: ", values$covRCDi, " weeks", "</li>",
               "<li>", ifelse(values$RCDcoex == 0, 'Performed with "radial" search', 'Performed with "co-exposure search"'), "</li>",
               "<li>", "Radius for radial search: ", values$RCDrad, " meters", "</li>",
               "<li>", "Added value of radial targetting: ", values$clustRCDrad, "%", "</li>",
               "<li>", "Sample size for coexposure search: ", values$RCDs, "% of village", "</li>",
               "<li>", "Added value of co-exposure targetting: ", values$clustRCDcoex, "%", "</li>",
               "<li>", "Sensitivity of RDT test (clinical): ", values$RCDsensC, "%", "</li>",
               "<li>", "Sensitivity of RDT test (micro detectable, asym.): ", values$RCDsensA, "%", "</li>",
               "<li>", "Sensitivity of RDT test (micro undetectable, asym.): ", values$RCDsensU, "%", "</li>",
               "</ul>")
      )
    })
    
    # RCD button to provide access to advanced parameters
    output$rcd_advanced_render <- renderUI({
      if (!isTRUE(input$RCDon)) return()
      actionButton("rcd_advanced", "RCD Parameters", icon = icon("external-link"))
    })
    
    # RCD modal dialogue with access to parameters
    observeEvent(input$rcd_advanced, {
      
      var <- "RCDscale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDthresh"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "covRCDi"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "delayRCD"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDcoex"
      tag_4 <- tagList(
        do.call(radioButtons, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          selected=as.numeric(meta[meta$input_id=="RCDcoex", "default_value"]),
          choices=c("Radial search"=0, "Co-exposure search"=1))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDrad"
      tag_5 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "clustRCDrad"
      tag_6 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDs"
      tag_7 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "clustRCDcoex"
      tag_8 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDsensC"
      tag_9 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDsensA"
      tag_10 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "RCDsensU"
      tag_11 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      showModal(modalDialog(title = "RCD Parameters", size = "m", easyClose = FALSE, footer = modalButton("Cancel"),
                            tag_1,
                            tag_2,
                            tag_3,
                            tag_4,
                            tag_5,
                            tag_6,
                            tag_7,
                            tag_8,
                            tag_9,
                            tag_10,
                            tag_11,
                            actionButton('update_rcd', "Update")
      ))
    })
    
    # update RCD parameters on update button
    observeEvent(input$update_rcd,{
      removeModal(session)
      values$RCDscale <- input$RCDscale
      values$covRCDi <- input$covRCDi
      values$delayRCD <- input$delayRCD
      values$RCDcoex <- input$RCDcoex
      values$RCDrad <- input$RCDrad
      values$clustRCDrad <- input$clustRCDrad
      values$RCDs <- input$RCDs
      values$clustRCDcoex <- input$clustRCDcoex
      values$RCDsensC <- input$RCDsensC
      values$RCDsensA <- input$RCDsensA
      values$RCDsensU <- input$RCDsensU
      values$RCDsensU <- input$RCDsensU
    })
    
    
    
    # Interventions/IRS
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # IRS Yes/No checkbox
    output$IRSon_render <- renderUI({
      checkboxInput("IRSon", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="IRSon", "label"]))), value=as.logical(meta[meta$input_id=="IRSon", "default_value"]))
    })
    
    # IRS summary text of parameters values
    output$irs_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", values$IRSscale, " year(s) to scale up IRS", "</li>",
               "<li>New coverage of IRS: ", values$covIRSi, "%", "</li>",
               "</ul>")
      )
    })
    
    # IRS button to provide access to advanced parameters
    output$irs_advanced_render <- renderUI({
      if (!isTRUE(input$IRSon)) return()
      actionButton("irs_advanced", "IRS Parameters", icon = icon("external-link"))
    })
    
    # IRS modal dialogue with access to parameters
    observeEvent(input$irs_advanced, {
      
      var <- "IRSscale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "covIRSi"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      showModal(modalDialog(title = "IRS Parameters", size = "s", easyClose = FALSE, footer = modalButton("Cancel"),
                            tag_1,
                            tag_2,
                            actionButton('update_irs', "Update")
      ))
    })
    
    # update IRS parameters on update button
    observeEvent(input$update_irs,{
      removeModal(session)
      values$IRSscale <- input$IRSscale
      values$covIRSi <- input$covIRSi
    })
    
    
   
    
    # Interventions/MSAT
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # MSAT Yes/No checkbox
    output$MSATon_render <- renderUI({
      checkboxInput("MSATon", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="MSATon", "label"]))), value=as.logical(meta[meta$input_id=="MSATon", "default_value"]))
    })
    
    # MSAT summary text of parameters values
    output$msat_parameters <- renderText({
      input$update_msat
      return(
        paste0("<ul>", "<li>", values$MSATscale, " year(s) to scale up MSAT", "</li>",
               "<li>", "New coverage of MSAT: ", values$covMSATi, "%", "</li>",
               "<li>", "Sensitivity MSAT test (clinical): ", values$MSATsensC, "%", "</li>",
               "<li>", "Sensitivity MSAT test (micro detectable, asym): ", values$MSATsensA, "%", "</li>",
               "<li>", "Sensitivity MSAT test (micro undetectable, asym): ", values$MSATsensU, "%", "</li>",
               "</ul>")
      )
    })
    
    # MSAT button to provide access to advanced parameters
    output$msat_advanced_render <- renderUI({
      if (!isTRUE(input$MSATon)) return()
      actionButton("msat_advanced", "MSAT Parameters", icon = icon("external-link"))
    })
    
    # MSAT modal dialogue with access to parameters
    observeEvent(input$msat_advanced, {
      
      var <- "MSATscale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "covMSATi"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "MSATsensC"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "MSATsensA"
      tag_4 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "MSATsensU"
      tag_5 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      
      showModal(modalDialog(title = "MSAT Parameters", size = "s", easyClose = FALSE, footer = modalButton("Cancel"),
                            tag_1,
                            tag_2,
                            tag_3,
                            tag_4,
                            tag_5,
                            actionButton('update_msat', "Update")
      ))
    })
    
    # update MSAT patameters on update button
    observeEvent(input$update_msat,{
      removeModal(session)
      values$MSATscale <- input$MSATscale
      values$covMSATi <- input$covMSATi
      values$MSATsensC <- input$MSATsensC
      values$MSATsensA <- input$MSATsensA
      values$MSATsensU <- input$MSATsensU
    })
    
    
    # Interventions/MDA
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # MDA Yes/No checkbox
    output$MDAon_render <- renderUI({
      checkboxInput("MDAon", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="MDAon", "label"]))), value=as.logical(meta[meta$input_id=="MDAon", "default_value"]))
    })
    
    # MDA summary text of parameters values
    output$mda_parameters <- renderText({
      input$update_mda
      return(
        paste0("<ul>", "<li>", values$lossd, " days prophylaxis provided by the ACT", "</li>",
               "<li>", values$dm, " months to complete each round", "</li>",
               "<li>", "Effective population coverage of focal MDA in round 1: ", values$cmda_1, "%", "</li>",
               "<li>", "Effective population coverage of focal MDA in round 2: ", values$cmda_2, "%", "</li>",
               "<li>", "Effective population coverage of focal MDA in round 3: ", values$cmda_3, "%", "</li>",
               "<li>", "Start of 1st round: ", format(as.Date("2017-12-15")+(values$tm_1*30.5), "%B %Y"), "</li>",
               "<li>", "Start of 2nd round: ", format(as.Date("2017-12-15")+(values$tm_2*30.5), "%B %Y"), "</li>",
               "<li>", "Start of 3rd round: ", format(as.Date("2017-12-01")+(values$tm_3*30.5), "%B %Y"), "</li>",
               "</ul>")
      )
    })
    
    # MDA button to provide access to advanced parameters
    output$mda_advanced_render <- renderUI({
      if (!isTRUE(input$MDAon)) return()
      actionButton("mda_advanced", "MDA Parameters", icon = icon("external-link"))
    })
    
    # MDA modal dialogue with access to parameters
    observeEvent(input$mda_advanced, {
      
      var <- "lossd"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "dm"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "cmda_1"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "cmda_2"
      tag_4 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "cmda_3"
      tag_5 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "tm_1"
      tag_6 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "tm_2"
      tag_7 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "tm_3"
      tag_8 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      showModal(modalDialog(title = "MDA Parameters", size = "s", easyClose = FALSE, footer = modalButton("Cancel"),
                            tag_1,
                            tag_2,
                            tag_3,
                            tag_4,
                            tag_5,
                            tag_6,
                            tag_7,
                            tag_8,
                            actionButton('update_mda', "Update")
      ))
    })
    
    # update MDA patameters on update button
    observeEvent(input$update_mda,{
      removeModal(session)
      values$lossd <- input$lossd
      values$dm <- input$dm
      values$cmda_1 <- input$cmda_1
      values$cmda_2 <- input$cmda_2
      values$cmda_3 <- input$cmda_3
      values$tm_1 <- input$tm_1
      values$tm_2 <- input$tm_2
      values$tm_3 <- input$tm_3
    })
    
    
    
    # package Name and Run button
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$package_name_render <- renderUI({
      if(is.null(input$EDATon) | is.null(input$ITNon) | is.null(input$RCDon) | is.null(input$IRSon) | is.null(input$MSATon) | is.null(input$MDAon)) return()
      if ((input$EDATon+input$ITNon+input$RCDon+input$IRSon+input$MSATon+input$MDAon)==0) return() 
      textInput('package_name', label="Package name:", placeholder=paste0("package ", compteur_simul()+1))
    })
    
    output$run_simul_render <- renderUI({
      if(is.null(input$EDATon) | is.null(input$ITNon) | is.null(input$RCDon) | is.null(input$IRSon) | is.null(input$MSATon) | is.null(input$MDAon)) return()
      if ((input$EDATon+input$ITNon+input$RCDon+input$IRSon+input$MSATon+input$MDAon)==0) return() 
      actionButton("run_simul", "Simul package", class = "btn-primary")
    })
    
    
    
    # List and data of interventions
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # initialisation of df - run one time at the start of the app, baseline unknown
    rv <- reactiveValues(df=data_frame(what="Baseline", time=1:20, value=rep(NA, 20)),
                         interv=data_frame(what="Baseline", explain="will appear after first calibration."))
    
    # update df when baseline go button is pressed: should suppress all interventions and update baseline only
    observeEvent(input$run_calibration, {
      rv$df <- data_frame(what="Baseline", time=1:20, value=c(rnorm(5, mean=5), rep(NA, 15)))
      rv$interv <- data_frame(what="Baseline", explain='as per the parameters in the "Baseline set-up" and "Calibration" sections.<br>')
    })
    
    # when modifying the selected interventions, action button value returns to 0 or the latest one?!
    # compteur press run_simul button
    compteur_simul <- reactiveVal(value=0)
    
    # every time there is a new intervention, add the result to the dataframe
    observeEvent(input$run_simul,{
      
      # update compteur
      i <- compteur_simul()+1
      compteur_simul(i)
      
      # add simulation to dataframe
      rv$df <- bind_rows(rv$df, 
                         data_frame(what=ifelse(input$package_name=="", paste0('package ', compteur_simul()), input$package_name),
                                    time=6:20, value=rnorm(15, mean=5)))
      
      
      # Describe the package
      rv$interv <- bind_rows(rv$interv, 
                             data_frame(what=ifelse(input$package_name=="", paste0('package ', compteur_simul()), input$package_name), 
                                        explain=paste0(
                                          # "<p>package was generated with the following parameters:</p>",
                                          "<ul>",
                                          ifelse(input$EDATon, paste0("<li>", "includes EDAT: ", 
                                                                        paste0("<ul>", "<li>", ifelse(values$primon, "With ACT and Primaquine", "Without ACT and Primaquine"), "</li>",
                                                                               "<li>", values$EDATscale, " year(s) to scale up EDAT", "</li>",
                                                                               "<li>New percentage of all clinical cases treated: ", values$covEDATi, "%", "</li>",
                                                                               "</ul>"),
                                                                        "</li>"), ""),
                                          ifelse(input$ITNon, paste0("<li>", "includes ITN: ", 
                                                                       paste0("<ul>", "<li>", values$ITNscale, " year(s) to scale up ITN", "</li>",
                                                                              "<li>New coverage of ITNs: ", values$covITNi, "%", "</li>",
                                                                              "</ul>"), 
                                                                       "</li>"), ""),
                                          ifelse(input$RCDon, paste0("<li>", "includes RCD: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          ifelse(input$IRSon, paste0("<li>", "includes IRS: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          ifelse(input$MSATon, paste0("<li>", "includes MSAT: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          ifelse(input$MDAon, paste0("<li>", "includes MDA: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          "</ul>"
                                        )
                             )
      )
    })
    
    output$text_package <- renderText({
      # if (nrow(rv$interv==1)) return()
      paste0("<strong>", rv$interv$what, ":</strong> ", rv$interv$explain, "<br>")
    })
    
    # packages to remove
    output$all_packages_checkbox_render <- renderUI({
      if(length(rv$interv$what)==1) return()
      checkboxGroupInput("all_packages_checkbox", "Select package(s) to Remove:", choices=rv$interv$what[-1], inline=TRUE)
    })
    
    # when remove button is hit
    observeEvent(input$remove_packages, {
      # remove from dataframe
      rv$df <- rv$df %>% filter(!(what %in% input$all_packages_checkbox))
      
      # remove from list of interventions
      rv$interv <- rv$interv %>% filter(!(what %in% input$all_packages_checkbox))
    })
    
    output$remove_packages_render <- renderUI({
      if(length(rv$interv$what)==1) return()
      actionButton("remove_packages", "Remove")
    })
    
    
    
    # Plot Baseline+packages section
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$plot_baseline_interventions <- renderPlot({
      
      if(input$run_calibration == 0) return({
        plot(1, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='')
        text(1, 1, 'Proceed with "calibration" first', cex=2)
      })
      
      ggplot(rv$df, aes(time, value, color=what)) +
        geom_point() +
        geom_line() +
        ggtitle("Random plot") +
        theme(legend.position = "bottom" )
      
    })
    
    output$plot_baseline_interventions_sai <- renderPlot({
      
      plot(1)
      
    })
    
    # Save Baseline parameters
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    observeEvent(input$save_param,{
      file_name <- paste0("Params Baseline_", Sys.time())
      df <- data.frame(noms=c('a', 'b'), valeurs=c(3, 4))
      write.csv(df, file='file_name', row.names=F)
    })
    
    
    # Generate report
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("./www/report/report.Rmd", tempReport, overwrite = TRUE)
        file.copy("./www/report/interpret_prevalence.png", file.path(tempdir(), "interpret_prevalence.png"), overwrite = TRUE)
        file.copy("./www/report/interpret_results.png", file.path(tempdir(), "interpret_results.png"), overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(area_name = input$area_name)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_format='pdf_document', output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    
  })