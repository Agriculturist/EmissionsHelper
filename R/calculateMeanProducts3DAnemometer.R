#' calculateMeanProducts3DAnemometer
#'
#' This function is designed to accept raw data from a 3D anemometer and 
#' divide the data into time segments.  This data should consist of a 
#' time, a verticle and two cardinal horizontal wind directions.  
#' 
#' For each time segment the products of the data are averaged.   
#' 
#' @param windDataframe the data frame for wind data
#' @param timeVariableName the name of the time variable
#' @param UVariableName the column name of wind in the X direction 
#' @param VVariableName the column name of wind in the Y direction  
#' @param WVariableName the column name of wind in the Z direction 
#' @param SonicTemperatureVariableName the column name of the sonic temperature 
#' @param timeIntervalType is set to "ceiling" or "floor" depending on where to mark the samples
#' @param timeIntervalValue the value which describes the length of the time segment  
#' @param timeIntervalUnits the units for the length of the time segment
#' @keywords time 3DAnemometer
#' @examples
#'  
#'    calculateCovariances3DAnemometer()
#' @export

calculateMeanProducts3DAnemometer <- function(windDataframe, 
                                             timeVariableName = "time",
                                             UVariableName = "U",
                                             VVariableName = "V",
                                             WVariableName = "W",
                                             SonicTemperatureVariableName = "T", 
                                             timeIntervalType = "ceiling",
                                             timeIntervalValue = 15, 
                                             timeIntervalUnits = "minutes"
                                             
                                            ){
  
  require(plyr)
  timeIntervalType <- tolower(timeIntervalType)

  if (timeIntervalType == "ceiling"){
    windDataframe$timeInterval <-  timeCeiling(windDataframe[[timeVariableName]],
                                                    timeIntervalValue,
                                                    timeIntervalUnits)
  }else{
    windDataframe$timeInterval <-  timeFloor(windDataframe[[timeVariableName]],
                                                    timeIntervalValue,
                                                    timeIntervalUnits)
  }
  
  outputDataframe <- ddply(windDataframe, .variables = .(timeInterval), 
                           .fun = function(xx){return(
                             data.frame(
                               U = mean(xx[[UVariableName]]),
                               V = mean(xx[[VVariableName]]),
                               W = mean(xx[[WVariableName]]),
                               T = mean(xx[[SonicTemperatureVariableName]]),
                               UU = mean(xx[[UVariableName]] * 
                                           xx[[UVariableName]]),
                               UV = mean(xx[[UVariableName]] * 
                                           xx[[VVariableName]]),
                               UW = mean(xx[[UVariableName]] * 
                                           xx[[WVariableName]]),
                               UT = mean(xx[[UVariableName]] * 
                                           xx[[SonicTemperatureVariableName]]),
                               VV = mean(xx[[VVariableName]] * 
                                           xx[[VVariableName]]),
                               VW = mean(xx[[VVariableName]] * 
                                           xx[[WVariableName]]),
                               VT = mean(xx[[VVariableName]] * 
                                           xx[[SonicTemperatureVariableName]]),
                               WW = mean(xx[[WVariableName]] * 
                                           xx[[WVariableName]]),
                               WT = mean(xx[[WVariableName]] * 
                                           xx[[SonicTemperatureVariableName]]),
                               TT = mean(xx[[SonicTemperatureVariableName]] * 
                                           xx[[SonicTemperatureVariableName]])
                             ))})
  
  
  return(outputDataframe)
}

