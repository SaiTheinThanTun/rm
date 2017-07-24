shinyServer(
  function(session, input, output) {

    # Reactive values
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    explanation <- reactive(switch(input$language,
                                   "en" = "input_explanation_en",
                                   "th" = "input_explanation_th"))
    
    values <- reactiveValues(edat_act = as.logical(meta[meta$input_id=="edat_act", "default_value"]),
                             edat_scale = as.numeric(meta[meta$input_id=="edat_scale", "default_value"]),
                             edat_treatment = as.numeric(meta[meta$input_id=="edat_treatment", "default_value"]),
                             itn_scale = as.numeric(meta[meta$input_id=="itn_scale", "default_value"]),
                             itn_cov = as.numeric(meta[meta$input_id=="itn_cov", "default_value"]),
                             rcd_scale = as.numeric(meta[meta$input_id=="rcd_scale", "default_value"]),
                             rcd_cov = as.numeric(meta[meta$input_id=="rcd_cov", "default_value"]),
                             rcd_delay = as.numeric(meta[meta$input_id=="rcd_delay", "default_value"]),
                             rcd_search = as.numeric(meta[meta$input_id=="rcd_search", "default_value"]),
                             rcd_value = as.numeric(meta[meta$input_id=="rcd_value", "default_value"]),
                             rcd_radial_value = as.numeric(meta[meta$input_id=="rcd_radial_value", "default_value"]),
                             rcd_samplesize = as.numeric(meta[meta$input_id=="rcd_samplesize", "default_value"]),
                             rcd_value_coexp = as.numeric(meta[meta$input_id=="rcd_value_coexp", "default_value"]),
                             rcd_sens_clin = as.numeric(meta[meta$input_id=="rcd_sens_clin", "default_value"]),
                             rcd_sens_detect = as.numeric(meta[meta$input_id=="rcd_sens_detect", "default_value"]),
                             rcd_sens_undetect = as.numeric(meta[meta$input_id=="rcd_sens_undetect", "default_value"]),
                             irs_scale = as.numeric(meta[meta$input_id=="irs_scale", "default_value"]),
                             irs_cov = as.numeric(meta[meta$input_id=="irs_cov", "default_value"]),
                             msat_scale = as.numeric(meta[meta$input_id=="msat_scale", "default_value"]),
                             msat_cov = as.numeric(meta[meta$input_id=="msat_cov", "default_value"]),
                             msat_sens_clinical = as.numeric(meta[meta$input_id=="msat_sens_clinical", "default_value"]),
                             msat_sens_detect = as.numeric(meta[meta$input_id=="msat_sens_detect", "default_value"]),
                             msat_sens_undetect = as.numeric(meta[meta$input_id=="msat_sens_undetect", "default_value"]),
                             mda_prophylaxys = as.numeric(meta[meta$input_id=="mda_prophylaxys", "default_value"]),
                             mda_months = as.numeric(meta[meta$input_id=="mda_months", "default_value"]),
                             mda_cov_1 = as.numeric(meta[meta$input_id=="mda_cov_1", "default_value"]),
                             mda_cov_2 = as.numeric(meta[meta$input_id=="mda_cov_2", "default_value"]),
                             mda_cov_3 = as.numeric(meta[meta$input_id=="mda_cov_3", "default_value"]),
                             mda_timing_1 = as.numeric(meta[meta$input_id=="mda_timing_1", "default_value"]),
                             mda_timing_2 = as.numeric(meta[meta$input_id=="mda_timing_2", "default_value"]),
                             mda_timing_3 = as.numeric(meta[meta$input_id=="mda_timing_3", "default_value"])
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
    
    # baseline_API
    output$baseline_API_render <- renderUI({
      var <- "baseline_API"
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
    output$baseline_cov_ITN_render <- renderUI({
      var <- "baseline_cov_ITN"
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
    
    
    output$baseline_avert_ITN_render <- renderUI({
      var <- "baseline_avert_ITN"
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
    
    output$baseline_cov_IRS_render <- renderUI({
      var <- "baseline_cov_IRS"
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
    
    output$baseline_avert_IRS_render <- renderUI({
      var <- "baseline_avert_IRS"
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
    output$baseline_treatment_render <- renderUI({
      var <- "baseline_treatment"
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
    
    output$baseline_treat_fail_2018_render <- renderUI({
      var <- "baseline_treat_fail_2018"
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
    
    output$baseline_treat_fail_2019_render <- renderUI({
      var <- "baseline_treat_fail_2019"
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
    
    output$baseline_treat_fail_2020_render <- renderUI({
      var <- "baseline_treat_fail_2020"
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
    output$baseline_trans_forest_render <- renderUI({
      var <- "baseline_trans_forest"
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
    
    output$baseline_trans_import_render <- renderUI({
      var <- "baseline_trans_import"
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
    
    output$baseline_trans_detect_render <- renderUI({
      var <- "baseline_trans_detect"
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
    
    output$baseline_trans_undetect_render <- renderUI({
      var <- "baseline_trans_undetect"
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
    
    # baseline_calibration
    output$baseline_calibration <- renderUI({
      var <- "baseline_calibration"
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
      input$baseline_API
      input$baseline_population
      input$baseline_cov_ITN
      input$baseline_avert_ITN
      input$baseline_cov_IRS
      input$baseline_avert_IRS
      input$baseline_treatment
      input$baseline_treat_fail_2018
      input$baseline_treat_fail_2019
      input$baseline_treat_fail_2020
      input$baseline_trans_forest
      input$baseline_trans_import
      input$baseline_trans_detect
      input$baseline_trans_undetect
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
      #     "<li>", meta$label[3], ": ", input$baseline_API, "</li>",
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
    output$edat_yes_render <- renderUI({
      checkboxInput("edat_yes", label=tags$span(class="label label-default", meta[meta$input_id=="edat_yes", "label"]), value=as.logical(meta[meta$input_id=="edat_yes", "default_value"]))
    })
    
    # EDAT summary text of parameters values
    output$edat_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", ifelse(values$edat_act, "With ACT and Primaquine", "Without ACT and Primaquine"), "</li>",
               "<li>", values$edat_scale, " year(s) to scale up EDAT", "</li>",
               "<li>New percentage of all clinical cases treated: ", values$edat_treatment, "%", "</li>",
               "</ul>")
      )
    })
    
    # EDAT button to provide access to advanced parameters
    output$edat_advanced_render <- renderUI({
      if (!isTRUE(input$edat_yes)) return()
      actionButton("edat_advanced", "EDAT Parameters", icon = icon("external-link"))
    })
    
    # EDAT modal dialogue with access to parameters
    output$edat_advanced_settings <- renderUI({

      var <- "edat_act"
      tag_1 <- tagList(
        do.call(checkboxInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.logical(meta[meta$input_id==var, "default_value"])
          )),
        bsTooltip(var, title = meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "edat_scale"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsTooltip(var, title = meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "edat_treatment"
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
      values$edat_act <- input$edat_act
      values$edat_scale <- input$edat_scale
      values$edat_treatment <- input$edat_treatment
    })
    
    
    
    # Interventions/ITN
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # ITN Yes/No checkbox
    output$itn_yes_render <- renderUI({
      checkboxInput("itn_yes", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="itn_yes", "label"]))), value=as.logical(meta[meta$input_id=="itn_yes", "default_value"]))
    })
    
    # ITN summary text of parameters values
    output$itn_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", values$itn_scale, " year(s) to scale up ITN", "</li>",
               "<li>New coverage of ITNs: ", values$itn_cov, "%", "</li>",
               "</ul>")
      )
    })
    
    # ITN button to provide access to advanced parameters
    output$itn_advanced_render <- renderUI({
      if (!isTRUE(input$itn_yes)) return()
      actionButton("itn_advanced", "ITN Parameters", icon = icon("external-link"))
    })
    
    # ITN modal dialogue with access to parameters
    observeEvent(input$itn_advanced, {
      
      var <- "itn_scale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "itn_cov"
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
      values$itn_scale <- input$itn_scale
      values$itn_cov <- input$itn_cov
    })
    
    
    
    
    
    # Interventions/RCD
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # RCD Yes/No checkbox
    output$rcd_yes_render <- renderUI({
      checkboxInput("rcd_yes", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="rcd_yes", "label"]))), value=as.logical(meta[meta$input_id=="itn_yes", "default_value"]))
    })
    
    # RCD summary text of parameters values
    output$rcd_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", values$rcd_scale, " year(s) to scale up RCD", "</li>",
               "<li>", "New coverage of RCD: ", values$rcd_cov, "%", "</li>",
               "<li>", "Reaction time: ", values$rcd_cov, " weeks", "</li>",
               "<li>", ifelse(values$rcd_search == 0, 'Performed with "radial" search', 'Performed with "co-exposure search"'), "</li>",
               "<li>", "Radius for radial search: ", values$rcd_value, " meters", "</li>",
               "<li>", "Added value of radial targetting: ", values$rcd_radial_value, "%", "</li>",
               "<li>", "Sample size for coexposure search: ", values$rcd_samplesize, "% of village", "</li>",
               "<li>", "Added value of co-exposure targetting: ", values$rcd_value_coexp, "%", "</li>",
               "<li>", "Sensitivity of RDT test (clinical): ", values$rcd_sens_clin, "%", "</li>",
               "<li>", "Sensitivity of RDT test (micro detectable, asym.): ", values$rcd_sens_detect, "%", "</li>",
               "<li>", "Sensitivity of RDT test (micro undetectable, asym.): ", values$rcd_sens_undetect, "%", "</li>",
               "</ul>")
      )
    })
    
    # RCD button to provide access to advanced parameters
    output$rcd_advanced_render <- renderUI({
      if (!isTRUE(input$rcd_yes)) return()
      actionButton("rcd_advanced", "RCD Parameters", icon = icon("external-link"))
    })
    
    # RCD modal dialogue with access to parameters
    observeEvent(input$rcd_advanced, {
      
      var <- "rcd_scale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_cov"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_delay"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_search"
      tag_4 <- tagList(
        do.call(radioButtons, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          selected=as.numeric(meta[meta$input_id=="rcd_search", "default_value"]),
          choices=c("Radial search"=0, "Co-exposure search"=1))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_value"
      tag_5 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_radial_value"
      tag_6 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_samplesize"
      tag_7 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_value_coexp"
      tag_8 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_sens_clin"
      tag_9 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_sens_detect"
      tag_10 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "rcd_sens_undetect"
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
      values$rcd_scale <- input$rcd_scale
      values$rcd_cov <- input$rcd_cov
      values$rcd_delay <- input$rcd_delay
      values$rcd_search <- input$rcd_search
      values$rcd_value <- input$rcd_value
      values$rcd_radial_value <- input$rcd_radial_value
      values$rcd_samplesize <- input$rcd_samplesize
      values$rcd_value_coexp <- input$rcd_value_coexp
      values$rcd_sens_clin <- input$rcd_sens_clin
      values$rcd_sens_detect <- input$rcd_sens_detect
      values$rcd_sens_undetect <- input$rcd_sens_undetect
      values$rcd_sens_undetect <- input$rcd_sens_undetect
    })
    
    
    
    # Interventions/IRS
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # IRS Yes/No checkbox
    output$irs_yes_render <- renderUI({
      checkboxInput("irs_yes", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="irs_yes", "label"]))), value=as.logical(meta[meta$input_id=="irs_yes", "default_value"]))
    })
    
    # IRS summary text of parameters values
    output$irs_parameters <- renderText({
      input$update
      return(
        paste0("<ul>", "<li>", values$irs_scale, " year(s) to scale up IRS", "</li>",
               "<li>New coverage of IRS: ", values$irs_cov, "%", "</li>",
               "</ul>")
      )
    })
    
    # IRS button to provide access to advanced parameters
    output$irs_advanced_render <- renderUI({
      if (!isTRUE(input$irs_yes)) return()
      actionButton("irs_advanced", "IRS Parameters", icon = icon("external-link"))
    })
    
    # IRS modal dialogue with access to parameters
    observeEvent(input$irs_advanced, {
      
      var <- "irs_scale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "irs_cov"
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
      values$irs_scale <- input$irs_scale
      values$irs_cov <- input$irs_cov
    })
    
    
   
    
    # Interventions/MSAT
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # MSAT Yes/No checkbox
    output$msat_yes_render <- renderUI({
      checkboxInput("msat_yes", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="msat_yes", "label"]))), value=as.logical(meta[meta$input_id=="msat_yes", "default_value"]))
    })
    
    # MSAT summary text of parameters values
    output$msat_parameters <- renderText({
      input$update_msat
      return(
        paste0("<ul>", "<li>", values$msat_scale, " year(s) to scale up MSAT", "</li>",
               "<li>", "New coverage of MSAT: ", values$msat_cov, "%", "</li>",
               "<li>", "Sensitivity MSAT test (clinical): ", values$msat_sens_clinical, "%", "</li>",
               "<li>", "Sensitivity MSAT test (micro detectable, asym): ", values$msat_sens_detect, "%", "</li>",
               "<li>", "Sensitivity MSAT test (micro undetectable, asym): ", values$msat_sens_undetect, "%", "</li>",
               "</ul>")
      )
    })
    
    # MSAT button to provide access to advanced parameters
    output$msat_advanced_render <- renderUI({
      if (!isTRUE(input$msat_yes)) return()
      actionButton("msat_advanced", "MSAT Parameters", icon = icon("external-link"))
    })
    
    # MSAT modal dialogue with access to parameters
    observeEvent(input$msat_advanced, {
      
      var <- "msat_scale"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "msat_cov"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "msat_sens_clinical"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "msat_sens_detect"
      tag_4 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "msat_sens_undetect"
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
      values$msat_scale <- input$msat_scale
      values$msat_cov <- input$msat_cov
      values$msat_sens_clinical <- input$msat_sens_clinical
      values$msat_sens_detect <- input$msat_sens_detect
      values$msat_sens_undetect <- input$msat_sens_undetect
    })
    
    
    # Interventions/MDA
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # MDA Yes/No checkbox
    output$mda_yes_render <- renderUI({
      checkboxInput("mda_yes", label=tags$span(style="color:#228B22", tags$u(tags$strong(meta[meta$input_id=="mda_yes", "label"]))), value=as.logical(meta[meta$input_id=="mda_yes", "default_value"]))
    })
    
    # MDA summary text of parameters values
    output$mda_parameters <- renderText({
      input$update_mda
      return(
        paste0("<ul>", "<li>", values$mda_prophylaxys, " days prophylaxis provided by the ACT", "</li>",
               "<li>", values$mda_months, " months to complete each round", "</li>",
               "<li>", "Effective population coverage of focal MDA in round 1: ", values$mda_cov_1, "%", "</li>",
               "<li>", "Effective population coverage of focal MDA in round 2: ", values$mda_cov_2, "%", "</li>",
               "<li>", "Effective population coverage of focal MDA in round 3: ", values$mda_cov_3, "%", "</li>",
               "<li>", "Start of 1st round: ", format(as.Date("2017-12-15")+(values$mda_timing_1*30.5), "%B %Y"), "</li>",
               "<li>", "Start of 2nd round: ", format(as.Date("2017-12-15")+(values$mda_timing_2*30.5), "%B %Y"), "</li>",
               "<li>", "Start of 3rd round: ", format(as.Date("2017-12-01")+(values$mda_timing_3*30.5), "%B %Y"), "</li>",
               "</ul>")
      )
    })
    
    # MDA button to provide access to advanced parameters
    output$mda_advanced_render <- renderUI({
      if (!isTRUE(input$mda_yes)) return()
      actionButton("mda_advanced", "MDA Parameters", icon = icon("external-link"))
    })
    
    # MDA modal dialogue with access to parameters
    observeEvent(input$mda_advanced, {
      
      var <- "mda_prophylaxys"
      tag_1 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_months"
      tag_2 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_cov_1"
      tag_3 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_cov_2"
      tag_4 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_cov_3"
      tag_5 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_timing_1"
      tag_6 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_timing_2"
      tag_7 <- tagList(
        do.call(sliderInput, list(
          inputId = var, 
          label = meta[meta$input_id==var, "label"], 
          value = as.numeric(meta[meta$input_id==var, "default_value"]),
          min = as.numeric(meta[meta$input_id==var, "min"]),
          max = as.numeric(meta[meta$input_id==var, "max"]),
          step = as.numeric(meta[meta$input_id==var, "step"]))),
        bsPopover(var, title = NULL, content=meta[meta$input_id==var, explanation()], placement="right", options = list(container = "body")))
      
      var <- "mda_timing_3"
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
      values$mda_prophylaxys <- input$mda_prophylaxys
      values$mda_months <- input$mda_months
      values$mda_cov_1 <- input$mda_cov_1
      values$mda_cov_2 <- input$mda_cov_2
      values$mda_cov_3 <- input$mda_cov_3
      values$mda_timing_1 <- input$mda_timing_1
      values$mda_timing_2 <- input$mda_timing_2
      values$mda_timing_3 <- input$mda_timing_3
    })
    
    
    
    # package Name and Run button
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$package_name_render <- renderUI({
      if(is.null(input$edat_yes) | is.null(input$itn_yes) | is.null(input$rcd_yes) | is.null(input$irs_yes) | is.null(input$msat_yes) | is.null(input$mda_yes)) return()
      if ((input$edat_yes+input$itn_yes+input$rcd_yes+input$irs_yes+input$msat_yes+input$mda_yes)==0) return() 
      textInput('package_name', label="Package name:", placeholder=paste0("package ", compteur_simul()+1))
    })
    
    output$run_simul_render <- renderUI({
      if(is.null(input$edat_yes) | is.null(input$itn_yes) | is.null(input$rcd_yes) | is.null(input$irs_yes) | is.null(input$msat_yes) | is.null(input$mda_yes)) return()
      if ((input$edat_yes+input$itn_yes+input$rcd_yes+input$irs_yes+input$msat_yes+input$mda_yes)==0) return() 
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
                                          ifelse(input$edat_yes, paste0("<li>", "includes EDAT: ", 
                                                                        paste0("<ul>", "<li>", ifelse(values$edat_act, "With ACT and Primaquine", "Without ACT and Primaquine"), "</li>",
                                                                               "<li>", values$edat_scale, " year(s) to scale up EDAT", "</li>",
                                                                               "<li>New percentage of all clinical cases treated: ", values$edat_treatment, "%", "</li>",
                                                                               "</ul>"),
                                                                        "</li>"), ""),
                                          ifelse(input$itn_yes, paste0("<li>", "includes ITN: ", 
                                                                       paste0("<ul>", "<li>", values$itn_scale, " year(s) to scale up ITN", "</li>",
                                                                              "<li>New coverage of ITNs: ", values$itn_cov, "%", "</li>",
                                                                              "</ul>"), 
                                                                       "</li>"), ""),
                                          ifelse(input$rcd_yes, paste0("<li>", "includes RCD: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          ifelse(input$irs_yes, paste0("<li>", "includes IRS: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          ifelse(input$msat_yes, paste0("<li>", "includes MSAT: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
                                          ifelse(input$mda_yes, paste0("<li>", "includes MDA: ", ": <em>parameters to be detailed here</em>", "</li>"), ""),
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