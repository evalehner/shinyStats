fluidPage(

  # Titel
  titlePanel(title = "Explorative Datenanalyse von Datensatz Pima"), 
  br(),
  helpText("Write sth. to explain what to do"),
  
  tabsetPanel(
    tabPanel("View Raw Data", 
             sidebarPanel(
               selectInput("all", label = "Select observations", choices= c("All", "None"), selected = "None"),
               checkboxGroupInput("obs", "Choose which provinces to show:", choices = rownames_pima
             )), 
             mainPanel(
               tableOutput("view"))
    ), 
    ## Plots and Input aligned horizontally, bad scaling?
    # tabPanel("Explore one Variable",
    #          helpText("Explain Purpose"),
    #          sidebarLayout(
    #            sidebarPanel(selectInput(inputId = "var", label = "Choose a variable",
    #                                     choices = names(pima))),
    #            mainPanel(plotOutput("summaryPlot"),
    #                      verbatimTextOutput("summaryStatistics")))
    #          ),
    # Plots and input aligned vertically
    tabPanel("Explore one Variable",
             helpText("Explain Purpose"),
             verticalLayout(
               selectInput(inputId = "var", label = "Choose a variable",
                           choices = names(pima)),
               selectInput(inputId ="var_statistic", label = "Choose if you want to see statistic of variable", choices = c("Yes", "No"), selected = "No"),
               plotOutput("summaryPlot", height = "500px"),
               verbatimTextOutput("summaryStatistics"),
               selectInput(inputId ="var_boxplot", label = "Choose if you want to see boxplot in addition", choices = c("Yes", "No"), selected = "No"),
               plotOutput("Boxplot", height = "500px")),
             selectInput(inputId ="var_transform", label = "Choose if you want to transform the variable", choices = list( "No", "Yes" =c("Logarithmic", "Normalized", "Polynomial_square")), selected = "No"),
             plotOutput("summaryPlot_transform", height = "500px")
    ),
    
    tabPanel("View Scatterplot",
             sidebarPanel(
               selectInput("all_scatter", label = "Select Variables to show", choices= c("All", "Select"), selected = "All"),
               checkboxGroupInput("var_scatter", "", choices = colnames(pima)),
               sliderInput("cor_scatter", label = "Choose a correlation threshold", min = 0, max = 1, value = 0.6),
               tableOutput("correlated_vars")
               ),
             mainPanel(
               plotOutput("scatterplot"))
    ),
  #   tabPanel("View Linear Model to Explain Education", 
  #            sidebarLayout(
  #              sidebarPanel(checkboxGroupInput(inputId = "var_4_linearModel", label = "Choose a variable", 
  #                                              choices = names(pima[c(1,2,3,5,6)]), width = '100%'), 
  #                           verbatimTextOutput(outputId = "summary_linearModel", placeholder = TRUE), 
  #                           # checkbox um modellselektion über stepwise AIC zu sehen 
  #                           checkboxInput(inputId = "inStepwiseSelection_AIC", 
  #                                         label = "View stepwise AIC selection", value = FALSE)), 
  #            
  #              mainPanel(plotOutput(outputId = "linModelPlot"), 
  #                        verbatimTextOutput(outputId = "outStepwiseSelection_AIC"))
  #            )
  #   )
  # )
  tabPanel("View Linear Model to Explain Glucose", 
           # layout von sidebarlayout ouf fluidRow + column layout geändert. 
           # I.e. Wir bestimmen manuell wo was anfängt / wir groß es ist etc. 
           # Grund für Änderung: plot und AIC output sind übereinder angezeigt worden 
           
           # 1. Reihe mit Selektionsknöpfen, summary und plots 
           fluidRow(
           column(5, 
                  # selektionsknöpfe, wellPanel sorgt dafür das dieses column grau hinterlegt ist
                  wellPanel(checkboxGroupInput(inputId = "var_4_linearModel", label = "Choose a variable", 
                                             choices = names(pima[c(1,3,4,5,6,7,8)]), width = '100%'), 
                          verbatimTextOutput(outputId = "summary_linearModel", placeholder = TRUE), 
      
                       # select and remove leverage points
                       selectInput(inputId="selectedLeveragePoints", label = "select leverage points", choices=rownames(pima), multiple = TRUE),
                       checkboxInput(inputId="adjustedModel", label = "remove leverage points", value = FALSE),
                       
                       # checkbox um modellselektion über stepwise AIC zu sehen 
                       # AIC ein Wert zum Modellvergleich. je kleiner AIC desto besser
                       checkboxInput(inputId = "inStepwiseAIC", 
                                     label = "View stepwise AIC selection", value = FALSE)
                       )),
            
           column(5, 
                  # residual plots 
                  plotOutput(outputId = "linModelPlot"))
           ),
           
           # 2. Reihe mit AIC output 
           fluidRow(
           column(5,verbatimTextOutput(outputId = "outStepwiseAIC") )
           )
                  
           
  ),


tabPanel("View Non-Linear Model to Explain Type", 
         # layout von sidebarlayout ouf fluidRow + column layout geändert. 
         # I.e. Wir bestimmen manuell wo was anfängt / wir groß es ist etc. 
         # Grund für Änderung: plot und AIC output sind übereinder angezeigt worden 
         
         # 1. Reihe mit Selektionsknöpfen, summary und plots 
         fluidRow(
           column(5, 
                  # selektionsknöpfe, wellPanel sorgt dafür das dieses column grau hinterlegt ist
                  wellPanel(checkboxGroupInput(inputId = "var_4_nonLinearModel", label = "Choose a variable", 
                                               choices = names(pima[c(1,2,3,4,5,6,7)]), width = '100%'), 
                            verbatimTextOutput(outputId = "summary_nonLinearModel", placeholder = TRUE), 
                            
                            # select and remove leverage points
                            selectInput(inputId="nonLinearSelectedLeveragePoints", label = "select leverage points", choices=rownames(pima), multiple = TRUE),
                            checkboxInput(inputId="nonLinearAdjustedModel", label = "remove leverage points", value = FALSE),
                            
                            # checkbox um modellselektion über stepwise AIC zu sehen 
                            # AIC ein Wert zum Modellvergleich. je kleiner AIC desto besser
                            checkboxInput(inputId = "nonLinearInStepwiseAIC", 
                                          label = "View stepwise AIC selection", value = FALSE)
                  )),
           
           column(5, 
                  # residual plots 
                  plotOutput(outputId = "nonLinearModelPlot"))
         ),
         
         # 2. Reihe mit AIC output 
         fluidRow(
           column(5,verbatimTextOutput(outputId = "nonLinearOutStepwiseAIC") )
         )
         
         
  )
)
)