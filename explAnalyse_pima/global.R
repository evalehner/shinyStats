library(shiny)

# Code der nur 1x laufen muss außerhalb ver server Funktion um performance zu verbessern 
# Achtung: app läuft nur wenn man pima manuell 'einließt'
pimaTe <- MASS::Pima.te
pimaTr <- MASS::Pima.tr
pima <- rbind(pimaTe, pimaTr)
pima <- cbind(pima[,-8], type = as.numeric(pima$type)-1)

rownames_pima <- rownames(pima)

#Function for plotting Histogram and QQPlot
nice <- function(data_values, var_name){
  #layout settings
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)
  
  #plots
  hist(data_values, main = paste("Plot of ", var_name), xlab = paste("[unit]", var_name),  freq = F)
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
  par(def.par)
}


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * abs(r))
}

# Funktion für correlation threshold
cor_threshold_vars <- function(data, cor_threshold) {
  cor_var_vector = vector()
  cor_index_df = data.frame(i = 0, j = 0)
  for ( i in 1:length(data)) {
    for ( j in 1:length(data)) {
      cor_var <- cor(data[i], data[j])
      if (i != j & abs(cor_var) > cor_threshold) {
        add_to_df <- TRUE
        for ( row in 1:length(cor_index_df[,1])) {
            if (i == cor_index_df[row,2] & j == cor_index_df[row,1]){
            add_to_df <- FALSE
            }
        }
        if (add_to_df == TRUE) {
          cor_index_df <- rbind(cor_index_df, c(i, j))
          cor_var_vector <- append(cor_var_vector, paste(rownames(cor_var), " - ", colnames(cor_var)))
        }
      }
    }
  }
  return(data.frame(Correlations = cor_var_vector))
}



#Function for logarithmic plots
logarithm_variable <- function(data_values, var_name)
  {{ if (var_name == "npreg") {
  logVariable <- log(data_values + 1)
  }
  else {logVariable <- log(data_values)}}
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2,3),1,3, byrow = FALSE), respect = T)
  
  #plots
  hist(logVariable, main = paste("Plot of ", var_name), xlab = paste("log units", var_name),  freq = F)
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
hist(nomVariable, main = paste("Plot of ", var_name), xlab = paste("normalized units", var_name),  freq = F)
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
hist(polyVariable, main = paste("Plot of ", var_name), xlab = paste("squared unit", var_name),  freq = F)
lines(density(polyVariable))
lines(density(polyVariable, adjust = 2), col = 2)
qqnorm(polyVariable)
qqline(polyVariable, col=2)

#change layout settings back to default
par(def.par)
}

# Funktion für transformieren der Daten für Modell
add_transformed_columns <- function(var_names, transform, df_to_append, df_to_extract) {
  number_of_rows <- length(df_to_extract[,1])
  for (i in 1:length(var_names)) {
    if (transform[i] == "Not included") {
      next
    }
    else if (transform[i] == "Untransformed") {
      df_to_append <- cbind(df_to_append, df_to_extract[which(names(df_to_extract)==var_names[i])])
    }
    else if (transform[i] == "log") {
      df_to_append <- cbind(df_to_append, log(df_to_extract[which(names(df_to_extract)==var_names[i])]))
    }
    else if (transform[i] == "normalized") {
      data_to_norm <- df_to_extract[1:number_of_rows, which(names(df_to_extract)==var_names[i])]
      data_normalized <- data.frame((data_to_norm-mean(data_to_norm))/sd(data_to_norm))
      names(data_normalized) <- var_names[i]
      df_to_append <- cbind(df_to_append, data_normalized)
    }
    else if (transform[i] == "polynomial") {
      data_to_pol <- df_to_extract[1:number_of_rows, which(names(df_to_extract)==var_names[i])]
      data_polynom <- data.frame((data_to_pol)^2)
      names(data_polynom) <- var_names[i]
      df_to_append <- cbind(df_to_append, data_polynom)
    }
  }
  return(df_to_append)
}

