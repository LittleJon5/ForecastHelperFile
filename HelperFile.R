# FRED transformation functions
# October 11, 2012 Revised March 19, 2015
# Author: Ray Nelson
###############################################################################
require(magrittr)

# ---------------------------------- Change
chg <- function(tsData) {
  tsData %>% 
    diff %>% 
    na.omit
}

# --------------------------------- Year over year change
ch1 <- function(tsData, n_obs_per_year) {
  tsData %>% 
    diff(lag = n_obs_per_year) %>% 
    na.omit
}

# --------------------------------- Percentage change
pch <- function(tsData) {
  (tsData/lag(tsData, 1)-1) * 100 %>% 
    na.omit
}

# ---------------------------------- Year over year percentage change
pc1 <- function(tsData, n_obs_per_year) {
  (tsData/lag(tsData, n_obs_per_year)-1) * 100 %>% 
    na.omit
}

# ---------------------------------- Compounded annual change
pca <- function(tsData, n_obs_per_year) {
  na.omit(((tsData/lag(tsData, 1))^n_obs_per_year-1) * 100)
}

# ----------------------------- Continously compounded percentage change
cch <- function(tsData) {
  na.omit((log(tsData) - log(lag(tsData, 1))) * 100)
}

# ----------------------------- Continuously compounded annual change
cca <- function(tsData, n_obs_per_year) {
  na.omit((log(tsData) - log(lag(tsData, 1))) * n_obs_per_year * 100)
}
 


# future data frame assembly function 
# this version of the function works best for displaying
# the data in a table format
# the next function is exactly the same except that
# as.character portion of the first item in the data frame is left off
###########################

forecast.frame <- function(ets.data){
  framed <- data.frame(as.character(as.Date(time(ets.data$mean))),
                      ets.data$mean,
                      ets.data$lower[,2],
                      ets.data$lower[, 1],
                      ets.data$upper[, 1],
                      ets.data$upper[, 2])
  names(framed) <- c('time', 'forecast', 'lower95', 'lower80', 
                     'upper80', 'upper95')
  return(framed)
}

###################
    # This creates a table that combines both the ets and arima
    # forecast into one table
    ##################

combinedTable <- function(arima.data, ets.data){
  
  framed <- data.frame(as.character(as.Date(time(ets.data$mean))),
                       ets.data$mean,
                       arima.data$mean)
  
  names(framed) <- c('Date', "ETS", "ARIMA")
  
  return(framed)
  
}

#################################
# Function for creating a forecast plot
# in ggplot2
##########################

forecastPlotData <- function(etsForecast, fredData){
  
  forecasts <- etsForecast %$%
    cbind(.$mean, .$lower, .$upper)
  
  plotData <- cbind(as.ts(fredData), forecasts)
  
  plotData <- data.frame(as.Date(time(plotData)), plotData)
  
  colnames(plotData) <- c("Date", "Indicator", "Forecast", "Lower80", "Lower95",
                          "Upper80", "Upper95")
  
  return(plotData)
  
}

#################################
# Function creates a ggplot of forecast data
# Use the forecastPlotData to product the
# apropriate data for the forecast
##########################

forecastPlot <- function(plotData, span.val, input.date) {
  
  load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
  recessions <- subset(recessions, Start >= plotData$Date[1])
  
  ggplot(data = plotData) +
    geom_ribbon(aes(x = Date, ymin = Lower95, ymax = Upper95), fill = "lightblue") +
    geom_ribbon(aes(x = Date, ymin = Lower80, ymax = Upper80), fill = "yellow") +
    geom_line(aes(x = Date, y = Indicator), color = "blue") +
    geom_line(aes(x = Date, y = Forecast), color = "red") +
    geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                          ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
    geom_smooth ( aes ( x = Date, y = Indicator ) , method = "loess" , span = span.val,
                  size = .65 , color = "black" , fill = "springgreen4" ) +
    labs(y = "")
  
}

# Function for extracting and renaming model parameters -------------------

modelParameters <- function(forecast){
  modelPar <- forecast$model$par[names(retail.forecast$model$par) %in% c("alpha", "beta", "phi")]
 
  modelPar <- modelPar[names(modelPar) != "b"]
  
  names(modelPar) <- gsub(pattern = "alpha", replacement = "alpha (level)", x = names(modelPar))
  
  names(modelPar) <- gsub(pattern = "beta", replacement = "beta (slope)", x = names(modelPar))
  
  nrow.val <- nrow(forecast$model$states)
  
  final.states <-  forecast$model$states[nrow.val, ]
  
  c(modelPar, final.states) %>% return
  
}

########################################
        ## Function for assembling the graph for the seasonal index of the months
        ## This is a function called in the seasonalPlot function
        #########################

barChartData <- function(stl.forecast){
  
  mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")  # vector used for assignement
  
  season <- stl.forecast$seasonal
  
  time <- as.Date(time(season))
  
  value <- data.frame(season)
  
  months <- factor(mon[month(time)], levels = mon)
  
  posNeg <- ifelse(value > 0, T, F)
  
  plot.data <- cbind(time, value, months, posNeg = posNeg)
  
  names(plot.data) <- c("date", "season", "months", "posNeg")
  
  return(plot.data)
}


# Seasonal Plot Function --------------------------------------------------
#############
  ### uses the barChartData function to fuel the plot
  #############

seasonalPlot <- function(stlForecast){
  
  barData <- barChartData(stlForecast)
  
  ggplot(data = barData, aes(x = months, y = season, fill = posNeg)) +
    geom_bar(stat = "identity") +
    labs(x = " \n Month", y = "") +
    theme(legend.position = "none")
  
}

# ForWay component Fucntion -----------------------------------------------
##############################
# Componenet function of the trendPlots graphing fucntion
############################

fourWayFrame <- function(stlModel, stlForecast) {
  
  stlPlotData <- stlModel$time.series %>% as.data.frame
  
  stlPlotData$date <- stlModel$time.series %>% time %>% as.Date
  
  stlPlotData$indicator <- stlForecast$x %>% as.numeric
  
  stlPlotData$posNeg <- ifelse(stlPlotData$remainder > 0, T, F)
  
  return(stlPlotData)
}

# trendPlots function -----------------------------------------------------
##################################
# Plot for creating the different trends
# plotType calls are "seasonal", "trend", "remainder", "indicator"
# the one that's different is "remainder" it produces a barchart instead
# of a line graph
##############################

trendPlots <- function(stlModel, stlForecast, plotType){
  
  plotInfo <- fourWayFrame(stlModel, stlForecast)
  
  startDate <- plotInfo$date[1] %>% as.Date %>% as.character
  endDate <- plotInfo$date[nrow(plotInfo)] %>% as.Date
  
  load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
  recessions <- subset(recessions, Start >= startDate)
  
  if(plotType == "remainder"){
    ggplot(data = plotInfo) +
      geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                            ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
      geom_bar(aes(y = remainder, x = date, fill = posNeg), stat = "identity" ) +
      theme(legend.position = "none") +
      labs(x = "Date", y = "Value \n") +
      scale_x_date( "" , limits = c( as.Date(startDate) , as.Date(endDate) ) )
  } else {
    ggplot(data = plotInfo) +
      geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                            ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
      geom_point(aes_string(y = plotType, x = "date"), color = "red") +
      geom_line(aes_string(y = plotType, x = "date"), color = "blue") +
      labs(x = "Date", y = "") +
      scale_x_date( "" , limits = c( as.Date(startDate) , as.Date(endDate) ) )
  }
}

# Function for extracting stl seasonal information ------------------------

modelSeasons <- function(stl.forecast){
  season <- stl.forecast$seasonal
  
  seasons <- season %>% as.numeric
  names(seasons) <- season %>% time %>% as.Date %>% month(label = TRUE)
  orderValue <- grep("Jan", names(seasons))
  
  seasons <- c(seasons[orderValue:length(seasons)], seasons[1:orderValue -1])
  
  return(seasons)
}


# Function for extracting and renaming model parameters -------------------

modelParameters <- function(forecast){
  modelPar <- forecast$model$par[names(retail.forecast$model$par) %in% c("alpha", "beta")]
  
  names(modelPar) <- gsub(pattern = "alpha", replacement = "alpha (level)", x = names(modelPar))
  
  names(modelPar) <- gsub(pattern = "beta", replacement = "beta (slope)", x = names(modelPar))
  
  nrow.val <- nrow(retail.forecast$model$states)
  
  final.states <-  retail.forecast$model$states[nrow.val, ]
  
  c(modelPar, final.states) %>% return
  
}


# function for stl forecast that combines seasonal and model param --------

modelParametersSTL <- function(StlForecast){
  first <- modelParameters(StlForecast)
  second <- modelSeasons(StlForecast)
  
  c(first, second) %>% return
}


