function(input, output) {
  
  # Tab Raw Data
  output$view <- renderTable({
    if (input$all == "All") {swiss2}
    else if (input$all == "None") {swiss2[input$obs,]}}, rownames = TRUE)
  
  # Tab Variable Exploration
  currentVariable <- reactive(swiss2[,input$var])
  output$summaryPlot <- renderPlot(nice(currentVariable(), input$var ), height = 450)
  output$summaryStatistics <- renderPrint({if (input$var_statistic == "Yes") {summary(currentVariable())}})
  output$Boxplot <- renderPlot({if (input$var_boxplot == "Yes") {boxplot_variable(currentVariable(), input$var )}}, height = 450)
  output$summaryPlot_transform <- renderPlot({
    if (input$var_transform == "Logarithmic") {logarithm_variable(currentVariable(), input$var )}
    else if (input$var_transform == "Normalized") {normalized_variable(currentVariable(), input$var )}
    else if (input$var_transform == "Polynomial_square") {polynomial_variable(currentVariable(), input$var )}
    }, height = 450)
  
  # Tab Scatterplot
  output$correlated_vars <- renderTable(cor_threshold_vars(swiss2, input$cor_scatter))
  
  output$scatterplot <- renderPlot( pairs(
    {if (input$all_scatter == "All") {swiss2}
      else {swiss2[input$var_scatter]}
    },
    lower.panel = panel.smooth, upper.panel = panel.cor,
    gap=0, row1attop=TRUE), width = 750, height = 750 )
  
  # output$scatterplot <- renderPlot( pairs(swiss2, lower.panel = panel.smooth, upper.panel = panel.cor,
  #                                         gap=0, row1attop=TRUE), width = 750, height = 750 )  # Change width and height
  # plot(swiss2) liefert scatterplot ohne korrelationskoeffizienten 
  
  # Hier wird regressionsmodell schrittweise gebaut
  # wenn man reaktiven wert weiter verwendet muss man ihn später wieder reaktiv gestalten 
  # https://stackoverflow.com/questions/26454609/r-shiny-reactive-error
  
  
  
  # variable_input_vector <- reactive({ c(input$fertility_input, input$agriculture_input, input$examination_input,
  #                                       input$education_input, input$catholic_input, input$infant_mortality_input)})
  
  var_transform <- reactive({ c(input$fertility_input, input$agriculture_input, input$examination_input,
                                input$education_input, input$catholic_input, input$infant_mortality_input) })
  
  variable_work_df <- reactive({
    df <- data.frame(row.names = rownames(swiss2))
    df <- add_transformed_columns(names(swiss2), var_transform(), df, swiss2)
    return(df)
    })

  variables <- reactive({ paste( names(variable_work_df()[-which(names(variable_work_df())=="Education")]), sep = " " , collapse = '+')})
  
  # output model
  leveragePoints <- reactive({ input$selectedLeveragePoints }) # leverage points in var gespeichert 
  #swissNoLeverage <- reactive({ swiss2[-which(rownames_swiss2 %in% leveragePoints() ),] }) # aus swiss entfert und neuer datensatz erstellt 
  
  myModel <-  reactive( 
    # wenn leverage points ausgewählt werden 
    if(!is.null(variable_work_df()[2,2]) 
            && !is.null(input$selectedLeveragePoints) 
            && (input$adjustedModel == TRUE)){
      noLeverageformula <- reactive({ paste("Education ~ ", variables() )}) # modell formel
      currentLinearModel <- reactive( {lm(noLeverageformula(), data = variable_work_df()[-which(rownames(variable_work_df()) %in% leveragePoints() ),] )} ) # modell
      return(currentLinearModel() )
    }
    # wenn keine leverage points ausgewähl werden 
    else if( !is.null(variable_work_df()[2,2])) {  # wenn variablen ausgesucht wurden
      myformula <- reactive({ paste("Education  ~ ", variables() )})
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

}
