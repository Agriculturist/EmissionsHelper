#' timeCeiling
#'
#' This function acts like a ceiling function for time.
#'
#' @param timeValue typically a column of time Values to take the floor function
#' @param timeIntervalValue the value of the time interval
#' @param timeIntervalUnits the units of the time interval
#' @keywords time
#' @examples
#'
#' @export

timeCeiling <- function(timeValue, timeIntervalValue = 15, timeIntervalUnits = "minutes"){
  
  timeIntervalInSeconds <- switch(timeIntervalUnits,
                                  "seconds" = 1,
                                  "second" = 1,
                                  "sec" = 1,
                                  "s" = 1,
                                  "minutes" = 60,
                                  "minute" = 60,
                                  "min" = 60,
                                  "m" = 60,
                                  "hours" = 3600,
                                  "hour" = 3600,
                                  "hr" = 3600,
                                  "h" = 3600)
  
  
  if(!is.null(timeValue)){
  return(
    as.POSIXct(
      floor(as.numeric(as.POSIXct(timeValue)) / 
              (timeIntervalValue * timeIntervalInSeconds)) * 
        (timeIntervalValue * timeIntervalInSeconds) 
      + timeIntervalValue * timeIntervalInSeconds, 
      origin='1970-01-01'))
  }else{
    warning("Check parameters in timeCeiling function ... timeValue is NULL")
  }
  
  
}
