library(shiny)
library(datasets)

# Code der nur 1x laufen muss außerhalb ver server Funktion um performance zu verbessern  
swiss2 <- data.frame(swiss$Fertility, swiss$Agriculture, swiss$Education, swiss$Catholic,swiss$Infant.Mortality) 
colnames(swiss2) <- c("Fertility", "Agriculture", "Education" , "Catholic",  "Infant.Mortality")

nice <- function(data_values){
  #layout settings
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)
  
  #plots
  hist(data_values, freq = F)
  lines(density(data_values))
  lines(density(data_values, adjust = 2), col = 2)
  boxplot(data_values, horizontal = T)
  qqnorm(data_values)
  qqline(data_values, col=2)
  
  #change layout settings back to default
  par(def.par)
}

# Definition von User Interface 

ui <- fluidPage(
  
  # Titel
  titlePanel(title = "Explorative Datenanalyse von Datensatz Swiss"), 
  br(),
  helpText("Write sth. to explain what to do"),
  
  tabsetPanel(
    tabPanel("View Raw Data", 
             tableOutput("view")), 
    tabPanel("Explore one Variable", 
             sidebarPanel(selectInput(inputId = "var", label = "Choose a variable", 
                                      choices = names(swiss2))),
             plotOutput("summaryPlot"), 
             verbatimTextOutput("summaryStatisitcs")), 
    tabPanel("View Scatterplot", plotOutput("scatterplot"))
  )
      
) 


server <- function(input, output) {
  
  output$view <- renderTable(head(swiss2))
 
  currentVariable <- reactive(swiss2[,input$var])  
  output$summaryPlot <- renderPlot(nice(currentVariable()))
  output$summaryStatisitcs <- renderPrint(summary(currentVariable()))  
  
  output$scatterplot <- renderPlot( plot(swiss2) )

  
}
shinyApp(ui = ui, server = server)