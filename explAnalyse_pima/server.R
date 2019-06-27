function(input, output) {
  
  # Tab Raw Data
  output$view <- renderTable({
    if (input$all == "All") {pima}
    else if (input$all == "None") {pima[input$obs,]}}, rownames = TRUE)
  
  # Tab Variable Exploration
  currentVariable <- reactive(pima[,input$var])
  output$summaryPlot <- renderPlot(nice(currentVariable(), input$var ), height = 500)
  output$summaryStatistics <- renderPrint({if (input$var_statistic == "Yes") {summary(currentVariable())}})
  output$Boxplot <- renderPlot({if (input$var_boxplot == "Yes") {boxplot_variable(currentVariable(), input$var )}})
  output$summaryPlot_transform <- renderPlot({
    if (input$var_transform == "Logarithmic") {logarithm_variable(currentVariable(), input$var )}
    else if (input$var_transform == "Normalized") {normalized_variable(currentVariable(), input$var )}
    else if (input$var_transform == "Polynomial_square") {polynomial_variable(currentVariable(), input$var )}
    })
  
  # Tab Scatterplot
  var_scatter <- reactive({ c(input$bp_scatter, input$skin, input$bmi_scatter,
                              input$ped_scatter, input$age_scatter, input$type_scatter, input$npreg_scatter, input$glu_scatter) })
  
  variable_scatter_df <- reactive({
    df_scatter <- data.frame(row.names = rownames(pima))
    df_scatter <- add_transformed_columns(names(pima), var_scatter(), df_scatter, pima)
    return(df_scatter)
  })
  
  
  output$correlated_vars <- renderTable(cor_threshold_vars(variable_scatter_df(), input$cor_scatter))
  
  output$scatterplot <- renderPlot( pairs({variable_scatter_df()},
                                          lower.panel = panel.smooth, upper.panel = panel.cor,
                                          gap=0, row1attop=TRUE), width = 750, height = 750 )
  # output$scatterplot <- renderPlot( pairs(
  #   {if (input$all_scatter == "All") {pima}
  #     else {pima[input$var_scatter]}
  #   },
  #   lower.panel = panel.smooth, upper.panel = panel.cor,
  #   gap=0, row1attop=TRUE), width = 750, height = 750 )
  
  # output$scatterplot <- renderPlot( pairs(pima, lower.panel = panel.smooth, upper.panel = panel.cor,
  #                                         gap=0, row1attop=TRUE), width = 750, height = 750 )  # Change width and height
  # plot(pima) liefert scatterplot ohne korrelationskoeffizienten 
  
  # Hier wird regressionsmodell schrittweise gebaut
  # wenn man reaktiven wert weiter verwendet muss man ihn später wieder reaktiv gestalten 
  # https://stackoverflow.com/questions/26454609/r-shiny-reactive-error
  
  var_transform <- reactive({ c(input$glc_input, input$npreg_input, input$bp_input, input$skin_input,
                                input$bmi_input, input$ped_input, input$age_input, input$type_input) })
  
  variable_work_df <- reactive({
    df <- data.frame(row.names = rownames(pima))
    df <- add_transformed_columns(names(pima), var_transform(), df, pima)
    return(df)
  })
  
  variables <- reactive({ paste( names(variable_work_df()[-which(names(variable_work_df())=="glu")]), sep = " " , collapse = '+')})
  
  # output model
  leveragePoints <- reactive({ input$selectedLeveragePoints }) # leverage points in var gespeichert 
  #swissNoLeverage <- reactive({ swiss2[-which(rownames_swiss2 %in% leveragePoints() ),] }) # aus swiss entfert und neuer datensatz erstellt 
  
  myModel <-  reactive( 
    # wenn leverage points ausgewählt werden 
    if(!is.null(variable_work_df()[2,2]) 
       && !is.null(input$selectedLeveragePoints) 
       && (input$adjustedModel == TRUE)){
      noLeverageformula <- reactive({ paste("glu ~ ", variables() )}) # modell formel
      currentLinearModel <- reactive( {lm(noLeverageformula(), data = variable_work_df()[-which(rownames(variable_work_df()) %in% leveragePoints() ),] )} ) # modell
      return(currentLinearModel() )
    }
    # wenn keine leverage points ausgewähl werden 
    else if( !is.null(variable_work_df()[2,2])) {  # wenn variablen ausgesucht wurden
      myformula <- reactive({ paste("glu  ~ ", variables() )})
      currentLinearModel <- reactive( {lm(myformula(), data = variable_work_df())} )
      return(currentLinearModel() )}
  )
  
  
  output$linModelPlot <- renderPlot({
    if( !is.null(variable_work_df()[2,2])) {
      layout(matrix(c(1,2,3,4), 2,2, byrow = TRUE), respect = T)
      plot(myModel() ) }
  }, width = 900, height = 900)
  
  output$summary_linearModel <- renderPrint({
    if(is.null(variable_work_df()[2,2])) {
      print("Please select a model")
    } else {
      summary(myModel())}
  })
  
  # AIC modellvergleich output 
  output$outStepwiseAIC <- renderPrint({
    if( input$inStepwiseAIC == TRUE) {
      step(myModel())
    }
  })

  
  
  # non linear model tab
  var_transform_nl <- reactive({ c(input$glc_input_nl, input$npreg_input_nl, input$bp_input_nl, input$skin_input_nl,
                                input$bmi_input_nl, input$ped_input_nl, input$age_input_nl, input$type_input_nl) })
  
  variable_work_df_nl <- reactive({
    df_nl <- data.frame(row.names = rownames(pima))
    df_nl <- add_transformed_columns(names(pima), var_transform_nl(), df_nl, pima)
    return(df_nl)
  })

  nonLinearVariables <- reactive({ paste( names(variable_work_df_nl()[-which(names(variable_work_df_nl())=="type")]), sep = " " , collapse = '+')})
  nonLinearLeveragePoints <- reactive({ input$nonLinearSelectedLeveragePoints }) # leverage points in var gespeichert 
  #swissNoLeverage <- reactive({ pima[-which(rownames_pima %in% leveragePoints() ),] }) # aus swiss entfert und neuer datensatz erstellt 
  
  nonLinearMyModel <-  reactive( 
    # wenn keine leverage points ausgewähl werden 
    
    if( !is.null(variable_work_df_nl()[2,2]) # wenn variablen ausgesucht wurden 
        && (input$nonLinearAdjustedModel == FALSE) ) {
      nonLinearMyformula <- reactive({  paste("type  ~ ", nonLinearVariables()  ) } )
      nonLinearCurrentLinearModel <- reactive( {glm(nonLinearMyformula(), data = variable_work_df_nl(), family = binomial(link = "logit"))} )
      return(nonLinearCurrentLinearModel() )} 
    
    # wenn leverage points ausgewähl werden 
    else if( !is.null(variable_work_df_nl()[2,2]) 
            && !is.null(input$nonLinearSelectedLeveragePoints) 
            && (input$nonLinearAdjustedModel == TRUE)){
      nonLinearNoLeverageformula <- reactive({  paste("type ~ ", nonLinearVariables()) }) # modell formel
      nonLinearCurrentLinearModel <- reactive( {glm(nonLinearNoLeverageformula(), data = variable_work_df_nl()[-which(rownames(variable_work_df_nl()) %in% nonLinearLeveragePoints() ),], family = binomial(link = "logit") )} ) # modell
      return(nonLinearCurrentLinearModel() )
    } )
  
  
  output$nonLinearModelPlot <- renderPlot({
    if( !is.null(variable_work_df_nl()[2,2]) ) {
      layout(matrix(c(1,2,3,4), 2,2, byrow = TRUE), respect = T)
      plot(nonLinearMyModel() ) }
  }, width = 750, height = 750)
  
  output$summary_nonLinearModel <- renderPrint({
    if( is.null(variable_work_df_nl()[2,2]) ) {
      print("Please select a model")
    } else {
      summary(nonLinearMyModel())}
  })
  
  # AIC modellvergleich output 
  output$nonLinearOutStepwiseAIC <- renderPrint({
    if( input$nonLinearInStepwiseAIC == TRUE) {
      step(nonLinearMyModel())
    }
  })
  
}