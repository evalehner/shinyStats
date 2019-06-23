library(shiny)

# Code der nur 1x laufen muss außerhalb ver server Funktion um performance zu verbessern 
# Achtung: app läuft nur wenn man swiss2 manuell 'einließt'
data("swiss")
swiss2 <- swiss

rownames_swiss2 <- rownames(swiss2)

#Function for plotting Histogram and QQPlot
nice <- function(data_values, var_name){
  #layout settings
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)
  
  #plots
  hist(data_values, main = paste("Plot of ", var_name), xlab = paste("% of ", var_name),  freq = F)
  lines(density(data_values))
  lines(density(data_values, adjust = 2), col = 2)
  qqnorm(data_values)
  qqline(data_values, col=2)
  
  #change layout settings back to default
  par(def.par)
}

#Funktion for Boxplot (optional)
boxplot_variable <- function(data_values, var_name){
  #layout settings
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1),1,1, byrow = FALSE), respect = T)
  
  #plots
  boxplot(data_values, horizontal = T)

  #change layout settings back to default
  #par(def.par)
}


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# Definition von User Interface 

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

server <- function(input, output) {
  # Tab Raw Data
  output$view <- renderTable({swiss2[input$obs,]}, rownames = TRUE)
 
  # Tab Variable Exploration
  currentVariable <- reactive(swiss2[,input$var])
  output$summaryPlot <- renderPlot(nice(currentVariable(), input$var ), height = 500)
  output$summaryStatistics <- renderPrint({if (input$var_statistic == "Yes") {summary(currentVariable())}})
  output$Boxplot <- renderPlot({if (input$var_boxplot == "Yes") {boxplot_variable(currentVariable(), input$var )}})
  
  # Tab Scatterplot
  output$scatterplot <- renderPlot( pairs(swiss2, lower.panel = panel.smooth, upper.panel = panel.cor,
                                          gap=0, row1attop=TRUE), width = 750, height = 750 )  # Change width and height
                                          # plot(swiss2) liefert scatterplot ohne korrelationskoeffizienten 
  
  # Hier wird regressionsmodell schrittweise gebaut
  # wenn man reaktiven wert weiter verwendet muss man ihn später wieder reaktiv gestalten 
  # https://stackoverflow.com/questions/26454609/r-shiny-reactive-error
  
  variables <- reactive({ paste( input$var_4_linearModel, sep = " " , collapse = '+') })
  form <- reactive({  paste("swiss2$Education  ~ ", variables()  ) } )
  currentLinearModel <- reactive( {lm(form(), data = swiss2)} )
  output$summary_linearModel <- renderPrint({ if( !is.null(input$var_4_linearModel ) ) {
    summary(currentLinearModel())
  } else {
    paste("Please choose a variable!")
  }})
  
  output$linModelPlot <- renderPlot({
    if( !is.null(input$var_4_linearModel ) ) {
      layout(matrix(c(1,2,3,4), 2,2, byrow = TRUE), respect = T)
      plot(currentLinearModel() ) }
    }, width = 750, height = 750)
    
  
}

shinyApp(ui = ui, server = server)
