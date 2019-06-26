function(input, output) {
  
  # Tab Raw Data
  output$view <- renderTable({swiss2[input$obs,]}, rownames = TRUE)
  
  # Tab Variable Exploration
  currentVariable <- reactive(swiss2[,input$var])
  output$summaryPlot <- renderPlot(nice(currentVariable(), input$var ), height = 500)
  output$summaryStatistics <- renderPrint({if (input$var_statistic == "Yes") {summary(currentVariable())}})
  output$Boxplot <- renderPlot({if (input$var_boxplot == "Yes") {boxplot_variable(currentVariable(), input$var )}})
  output$summaryPlot_transform <- renderPlot({if (input$var_transform == "Logarithmic") {nice(log(currentVariable()), input$var )}})
  
  # Tab Scatterplot
  output$scatterplot <- renderPlot( pairs(swiss2, lower.panel = panel.smooth, upper.panel = panel.cor,
                                          gap=0, row1attop=TRUE), width = 750, height = 750 )  # Change width and height
  # plot(swiss2) liefert scatterplot ohne korrelationskoeffizienten 
  
  # Hier wird regressionsmodell schrittweise gebaut
  # wenn man reaktiven wert weiter verwendet muss man ihn später wieder reaktiv gestalten 
  # https://stackoverflow.com/questions/26454609/r-shiny-reactive-error
  
  variables <- reactive({ paste( input$var_4_linearModel, sep = " " , collapse = '+') })
  
  # output model
  myModel <-  reactive( 
    # wenn keine leverage points ausgewähl werden 
    if( !is.null(input$var_4_linearModel) # wenn variablen ausgesucht wurden 
        && (input$adjustedModel == FALSE) ) {
      myformula <- reactive({  paste("swiss2$Education  ~ ", variables()  ) } )
      currentLinearModel <- reactive( {lm(myformula(), data = swiss2)} )
      return(currentLinearModel() )} 
    
    # wenn keine leverage points ausgewähl werden 
    else if(!is.null(input$var_4_linearModel) 
              && !is.null(input$selectedLeveragePoints) 
              && (input$adjustedModel == TRUE)){
      leveragePoints <- reactive({ input$selectedLeveragePoints }) # leverage points in var gespeichert 
      swissNoLeverage <- reactive({ swiss2[-which(rownames_swiss2 %in% leveragePoints() ),] }) # aus swiss entfert und neuer datensatz erstellt 
      noLeverageformula <- reactive({  paste("Education ~ ", variables()) }) # modell formel
      currentLinearModel <- reactive( {lm(noLeverageformula(), data = swissNoLeverage() )} ) # modell
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

}