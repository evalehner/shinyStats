fluidPage(

  # Titel
  titlePanel(title = "Explorative Datenanalyse von Datensatz Swiss"), 
  br(),
  helpText("Write sth. to explain what to do"),
  
  tabsetPanel(
    tabPanel("View Raw Data", 
             sidebarPanel(
               selectInput("all", label = "Select provinces", choices= c("All", "None"), selected = "None"),
               checkboxGroupInput("obs", "Choose which provinces to show:", choices = rownames_swiss2
             )), 
             mainPanel(
               tableOutput("view"))
    ), 
    ## Plots and Input aligned horizontally, bad scaling?
    # tabPanel("Explore one Variable",
    #          helpText("Explain Purpose"),
    #          sidebarLayout(
    #            sidebarPanel(selectInput(inputId = "var", label = "Choose a variable",
    #                                     choices = names(swiss2))),
    #            mainPanel(plotOutput("summaryPlot"),
    #                      verbatimTextOutput("summaryStatistics")))
    #          ),
    # Plots and input aligned vertically
    tabPanel("Explore one Variable",
               verticalLayout(
               fluidRow(
                 column(2,
                        wellPanel(
                          selectInput(inputId = "var", label = "Choose a variable", choices = names(swiss2)),
                          selectInput(inputId ="var_statistic", label = "View 5 number summary", choices = c("Yes", "No"), selected = "No"),
                          
                          selectInput(inputId ="var_boxplot", label = "Choose if you want to see boxplot in addition", choices = c("Yes", "No"), selected = "No"),
                          selectInput(inputId ="var_transform", label = "Choose if you want to transform the variable", choices = list( "No", "Yes" =c("Logarithmic", "Normalized", "Polynomial_square")), selected = "No")
                        )),
                 column(6,
                        plotOutput("summaryPlot", height = "450px", width = "auto"),
                        verbatimTextOutput("summaryStatistics")
                        ),
                 column(3,
                        plotOutput("Boxplot", height = "450px", width = "auto")
                        )
                 ),
               fluidRow(
                 column(2),
                 column(6,
                        plotOutput("summaryPlot_transform", height = "450px", width = "auto")
                        )
               )

    )),
    
    tabPanel("View Scatterplot",
             sidebarPanel(
               selectInput("all_scatter", label = "Select Variables to show", choices= c("All", "Select"), selected = "All"),
               checkboxGroupInput("var_scatter", "", choices = colnames(swiss2)),
               sliderInput("cor_scatter", label = "Choose a correlation threshold", min = 0, max = 1, value = 0.6),
               tableOutput("correlated_vars")
               ),
             mainPanel(
               plotOutput("scatterplot"))
    ),
  #   tabPanel("View Linear Model to Explain Education", 
  #            sidebarLayout(
  #              sidebarPanel(checkboxGroupInput(inputId = "var_4_linearModel", label = "Choose a variable", 
  #                                              choices = names(swiss2[c(1,2,3,5,6)]), width = '100%'), 
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
  tabPanel("View Linear Model to Explain Education", 
           # layout von sidebarlayout ouf fluidRow + column layout geändert. 
           # I.e. Wir bestimmen manuell wo was anfängt / wir groß es ist etc. 
           # Grund für Änderung: plot und AIC output sind übereinder angezeigt worden 
           
           # 1. Reihe mit Selektionsknöpfen, summary und plots 
           fluidRow(
           column(4, 
                  # selektionsknöpfe, wellPanel sorgt dafür das dieses column grau hinterlegt ist
                  wellPanel(radioButtons(inputId = "education_input", label = "Choose if you want to transform Education", 
                                               choices = c("Untransformed", "log", "normalized", "polynomial"), width = '100%', inline = TRUE),
                            radioButtons(inputId = "fertility_input", label = "Fertility", 
                                         choices = c("Not included", "Untransformed", "log", "normalized", "polynomial"), width = '100%', inline = TRUE),
                            radioButtons(inputId = "agriculture_input", label = "Agriculture", 
                                         choices = c("Not included", "Untransformed", "log", "normalized", "polynomial"), width = '100%', inline = TRUE),
                            radioButtons(inputId = "examination_input", label = "Examination", 
                                         choices = c("Not included", "Untransformed", "log", "normalized", "polynomial"), width = '100%', inline = TRUE),
                            radioButtons(inputId = "catholic_input", label = "Catholic", 
                                         choices = c("Not included", "Untransformed", "log", "normalized", "polynomial"), width = '100%', inline = TRUE),
                            radioButtons(inputId = "infant_mortality_input", label = "Infant Mortality", 
                                         choices = c("Not included", "Untransformed", "log", "normalized", "polynomial"), width = '100%', inline = TRUE),

                          verbatimTextOutput(outputId = "summary_linearModel", placeholder = TRUE), 
      
                       # select and remove leverage points
                       selectInput(inputId="selectedLeveragePoints", label = "select leverage points", choices=rownames(swiss2), multiple = TRUE),
                       checkboxInput(inputId="adjustedModel", label = "remove leverage points", value = FALSE),
                       
                       # checkbox um modellselektion über stepwise AIC zu sehen 
                       # AIC ein Wert zum Modellvergleich. je kleiner AIC desto besser
                       checkboxInput(inputId = "inStepwiseAIC", 
                                     label = "View stepwise AIC selection", value = FALSE)
                       )),
            
           column(8, 
                  # residual plots 
                  plotOutput(outputId = "linModelPlot"))
           ),
           
           # 2. Reihe mit AIC output 
           fluidRow(
           column(4,verbatimTextOutput(outputId = "outStepwiseAIC") )
           )
                  
           
  )
  )
)
