#' calculateMeansOverTime
#'
#' This function is designed to accept raw data and 
#' divide the data into time segments.    
#' 
#' For each time segment the outputs are:
#' 
#'    mean - mean of each time segment
#'    stdDev - standard deviation of each time segment
#'    count - count of each time segment
#'    max - max of each time segment
#'    min - min of each time segment
#'    mode - mode of each time segment
#'             
#' 
#' @param dataframe the data frame for wind data
#' @param timeColumnName the name of the time variable
#' @param valueColumnName the value of the dataframe to perform statistics
#' @param timeIntervalType is set to "ceiling" or "floor" depending on where to mark the samples
#' @param timeIntervalValue the value which describes the length of the time segment  
#' @param timeIntervalUnits the units for the length of the time segment
#' @keywords 
#' @examples
#'  
#'    calculateMeansOverTime()
#' @export

calculateMeansOverTime <- function(dataframe, 
                                   timeColumnName = "time",
                                   valueColumnName = NULL,
                                   timeIntervalType = "ceiling",
                                   timeIntervalValue = 15, 
                                   timeIntervalUnits = "minutes"
                                   
){
  
  require(plyr)
  timeIntervalType <- tolower(timeIntervalType)

  if (timeIntervalType == "ceiling"){
    dataframe$timeInterval <-  timeCeiling(dataframe[[timeColumnName]],
                                                    timeIntervalValue,
                                                    timeIntervalUnits)
  }else{
    dataframe$timeInterval <-  timeFloor(dataframe[[timeColumnName]],
                                                    timeIntervalValue,
                                                    timeIntervalUnits)
  }
  
  outputDataframe <- ddply(dataframe, .variables = .(timeInterval), 
                           .fun = function(xx){return(
                             data.frame(
                               mean = mean(xx[[valueColumnName]]),
                               stdDev = sd(xx[[valueColumnName]]),
                               count = length(xx[[valueColumnName]]),
                               max = max(xx[[valueColumnName]]),
                               min = min(xx[[valueColumnName]]),
                               mode = mode(xx[[valueColumnName]]) 
                             ))})
  
  
  return(outputDataframe)
}

