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
  layout(matrix(1,1,1, byrow = FALSE), respect = T)
  
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

#Function for logarithmic plots
logarithm_variable <- function(data_values, var_name)
  {logVariable <- log(data_values)
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)
  
  #plots
  hist(logVariable, main = paste("Plot of ", var_name), xlab = paste("% (log) of ", var_name),  freq = F)
  lines(density(logVariable))
  lines(density(logVariable, adjust = 2), col = 2)
  qqnorm(logVariable)
  qqline(logVariable, col=2)
  
  #change layout settings back to default
  par(def.par)
}

#Function for normalisation
normalized_variable <- function(data_values, var_name)
{nomVariable <- (data_values-mean(data_values))/sd(data_values)
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)

#plots
hist(nomVariable, main = paste("Plot of ", var_name), xlab = paste("% (normalized) of ", var_name),  freq = F)
lines(density(nomVariable))
lines(density(nomVariable, adjust = 2), col = 2)
qqnorm(nomVariable)
qqline(nomVariable, col=2)

#change layout settings back to default
par(def.par)
}

#Function for polynomial
polynomial_variable <- function(data_values, var_name)
{polyVariable <- (data_values)^2
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)

#plots
hist(polyVariable, main = paste("Plot of ", var_name), xlab = paste("% (square) of ", var_name),  freq = F)
lines(density(polyVariable))
lines(density(polyVariable, adjust = 2), col = 2)
qqnorm(polyVariable)
qqline(polyVariable, col=2)

#change layout settings back to default
par(def.par)
}
