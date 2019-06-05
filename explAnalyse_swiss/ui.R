library(shiny)
library(datasets)


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
  
  
  # Auswahl zwischen Datenexploration der einzelnen Variablen und dem Scatter plot 
  # input function:  
  # output function: 
  
  # Auswahl zwischen seperaten für ausgabe von histogram / boxplot / qqplot + summary statistics 
  # input function 
  #http://rstudio.github.io/shiny/tutorial/#inputs-and-outputs
  sidebarPanel(selectInput(inputId = "var", label = "Choose a variable", 
              choices = names(swiss2))),
  # output function 
  plotOutput("summaryPlot"), 
  verbatimTextOutput("summaryStatisitcs")
  
  # scatterplot 
  # input function 
  # output function 
  

)


server <- function(input, output) {
  currentVariable <- reactive(swiss2[,input$var])
  output$summaryPlot <- renderPlot(nice(currentVariable()))
  output$summaryStatisitcs <- renderPrint(summary(currentVariable()))  
}
shinyApp(ui = ui, server = server)