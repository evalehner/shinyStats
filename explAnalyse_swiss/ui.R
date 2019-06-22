library(shiny)
library(datasets)

# Code der nur 1x laufen muss außerhalb ver server Funktion um performance zu verbessern 
# Achtung: app läuft nur wenn man swiss2 manuell 'einließt'
swiss2 <- data.frame(swiss$Fertility, swiss$Agriculture, swiss$Education, swiss$Catholic,swiss$Infant.Mortality) 
colnames(swiss2) <- c("Fertility", "Agriculture", "Education" , "Catholic",  "Infant.Mortality")


nice <- function(data_values, var_name){
  #layout settings
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)
  
  #plots
  hist(data_values, main = paste("Plot of ", var_name), xlab = paste("better axix names", var_name),  freq = F)
  lines(density(data_values))
  lines(density(data_values, adjust = 2), col = 2)
  boxplot(data_values, horizontal = T)
  qqnorm(data_values)
  qqline(data_values, col=2)
  
  #change layout settings back to default
  par(def.par)
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
               numericInput("obs", "Number of observations to view:", 10, min = 1, max = 47)
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
               plotOutput("summaryPlot", height = "500px"),
                         verbatimTextOutput("summaryStatistics"))
             ),

    tabPanel("View Scatterplot", plotOutput("scatterplot")
             ),
    tabPanel("View Linear Model to Explain Education", 
             sidebarLayout(
               sidebarPanel(checkboxGroupInput(inputId = "var_4_linearModel", label = "Choose a variable", 
                                          choices = names(swiss2), width = '100%')), 
               mainPanel(plotOutput(outputId = "linModelPlot"), 
                         verbatimTextOutput(outputId = "summary_linearModel", placeholder = TRUE))
             )
            
      
    )
  
    )
      
) 


server <- function(input, output) {
  # Tab Raw Data
  output$view <- renderTable({head(swiss2, n = input$obs)})
 
  # Tab Variable Exploration
  currentVariable <- reactive(swiss2[,input$var])  
  output$summaryPlot <- renderPlot(nice(currentVariable(), input$var ), height = 500)
  output$summaryStatistics <- renderPrint(summary(currentVariable() ))  
  
  # Tab Scatterplot
  output$scatterplot <- renderPlot( pairs(swiss2, lower.panel = panel.smooth, upper.panel = panel.cor,
                                          gap=0, row1attop=TRUE), width = 750, height = 750 )  # Change width and height
                                          # plot(swiss2) liefert scatterplot ohne korrelationskoeffizienten 
  
  # Hier wird regressionsmodell schrittweise gebaut
  # wenn man reaktiven wert weiter verwendet muss man ihn später wieder reaktiv gestalten 
  # https://stackoverflow.com/questions/26454609/r-shiny-reactive-error
  
  # Achtung: education ist noch drinne!
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
    })
    
  
}
shinyApp(ui = ui, server = server)
