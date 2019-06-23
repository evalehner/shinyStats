ui <- fluidPage(
  
  # Titel
  titlePanel(title = "Explorative Datenanalyse von Datensatz Swiss"), 
  br(),
  helpText("Write sth. to explain what to do"),
  
  tabsetPanel(
    tabPanel("View Raw Data", 
             sidebarPanel(
               checkboxGroupInput("obs", "Choose which provinces to show:", choices = rownames_swiss2)
             ), 
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
             helpText("Explain Purpose"),
             verticalLayout(
               selectInput(inputId = "var", label = "Choose a variable",
                           choices = names(swiss2)),
               selectInput(inputId ="var_statistic", label = "Choose if you want to see statistic of variable", choices = c("Yes", "No")),
               selectInput(inputId ="var_boxplot", label = "Choose if you want to see boxplot in addition", choices = c("Yes", "No")),
               plotOutput("summaryPlot", height = "500px"),
               verbatimTextOutput("summaryStatistics"),
               plotOutput("Boxplot", height = "500px"))
    ),
    
    tabPanel("View Scatterplot", plotOutput("scatterplot")
    ),
    tabPanel("View Linear Model to Explain Education", 
             sidebarLayout(
               sidebarPanel(checkboxGroupInput(inputId = "var_4_linearModel", label = "Choose a variable", 
                                               choices = names(swiss2[c(1,2,3,5,6)]), width = '100%'), 
                            verbatimTextOutput(outputId = "summary_linearModel", placeholder = TRUE)), 
               mainPanel(plotOutput(outputId = "linModelPlot"))
             )
             
             
    )
    
  )
)