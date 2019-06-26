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
  output$correlated_vars <- renderTable(cor_threshold_vars(pima, input$cor_scatter))
  
  output$scatterplot <- renderPlot( pairs(
    {if (input$all_scatter == "All") {pima}
      else {pima[input$var_scatter]}
    },
    lower.panel = panel.smooth, upper.panel = panel.cor,
    gap=0, row1attop=TRUE), width = 750, height = 750 )
  
  # output$scatterplot <- renderPlot( pairs(pima, lower.panel = panel.smooth, upper.panel = panel.cor,
  #                                         gap=0, row1attop=TRUE), width = 750, height = 750 )  # Change width and height
  # plot(pima) liefert scatterplot ohne korrelationskoeffizienten 
  
  # Hier wird regressionsmodell schrittweise gebaut
  # wenn man reaktiven wert weiter verwendet muss man ihn später wieder reaktiv gestalten 
  # https://stackoverflow.com/questions/26454609/r-shiny-reactive-error
  
  variables <- reactive({ paste( input$var_4_linearModel, sep = " " , collapse = '+') })
  
  # output model
  leveragePoints <- reactive({ input$selectedLeveragePoints }) # leverage points in var gespeichert 
  #swissNoLeverage <- reactive({ pima[-which(rownames_pima %in% leveragePoints() ),] }) # aus swiss entfert und neuer datensatz erstellt 
  
  myModel <-  reactive( 
    # wenn keine leverage points ausgewähl werden 
    if( !is.null(input$var_4_linearModel) # wenn variablen ausgesucht wurden 
        && (input$adjustedModel == FALSE) ) {
      myformula <- reactive({  paste("pima$glu  ~ ", variables()  ) } )
      currentLinearModel <- reactive( {lm(myformula(), data = pima)} )
      return(currentLinearModel() )} 
    
    # wenn keine leverage points ausgewähl werden 
    else if(!is.null(input$var_4_linearModel) 
              && !is.null(input$selectedLeveragePoints) 
              && (input$adjustedModel == TRUE)){
      noLeverageformula <- reactive({  paste("glu ~ ", variables()) }) # modell formel
      currentLinearModel <- reactive( {lm(noLeverageformula(), data = pima[-which(rownames_pima %in% leveragePoints() ),] )} ) # modell
      return(currentLinearModel() )
    } )
  
  
  output$linModelPlot <- renderPlot({
    if( !is.null(input$var_4_linearModel ) ) {
      layout(matrix(c(1,2,3,4), 2,2, byrow = TRUE), respect = T)
      plot(myModel() ) }
  }, width = 750, height = 750)
  
  output$summary_linearModel <- renderPrint({
    if(is.null(input$var_4_linearModel)) {
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

  
  
  # output model
  nonLinearVariables <- reactive({ paste( input$var_4_nonLinearModel, sep = " " , collapse = '+') })
  nonLinearLeveragePoints <- reactive({ input$nonLinearSelectedLeveragePoints }) # leverage points in var gespeichert 
  #swissNoLeverage <- reactive({ pima[-which(rownames_pima %in% leveragePoints() ),] }) # aus swiss entfert und neuer datensatz erstellt 
  
  nonLinearMyModel <-  reactive( 
    # wenn keine leverage points ausgewähl werden 
    if( !is.null(input$var_4_nonLinearModel) # wenn variablen ausgesucht wurden 
        && (input$nonLinearAdjustedModel == FALSE) ) {
      nonLinearMyformula <- reactive({  paste("pima$type  ~ ", nonLinearVariables()  ) } )
      nonLinearCurrentLinearModel <- reactive( {glm(nonLinearMyformula(), data = pima, family = binomial(link = "logit"))} )
      return(nonLinearCurrentLinearModel() )} 
    
    # wenn keine leverage points ausgewähl werden 
    else if(!is.null(input$var_4_nonlinearModel) 
            && !is.null(input$nonLinearSelectedLeveragePoints) 
            && (input$nonLinearAdjustedModel == TRUE)){
      nonLinearNoLeverageformula <- reactive({  paste("type ~ ", nonLinearVariables()) }) # modell formel
      nonLinearCurrentLinearModel <- reactive( {glm(nonLinearNoLeverageformula(), data = pima[-which(rownames_pima %in% nonLinearLeveragePoints() ),], family = binomial(link = "logit") )} ) # modell
      return(nonLinearCurrentLinearModel() )
    } )
  
  
  output$nonLinearModelPlot <- renderPlot({
    if( !is.null(input$var_4_nonLinearModel ) ) {
      layout(matrix(c(1,2,3,4), 2,2, byrow = TRUE), respect = T)
      plot(nonLinearMyModel() ) }
  }, width = 750, height = 750)
  
  output$summary_nonLinearModel <- renderPrint({
    if(is.null(input$var_4_nonLinearModel)) {
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