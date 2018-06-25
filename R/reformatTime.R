#' reformatTime
#'
#' This function is a wrapper for the as.POSIXct function. This function is 
#' designed require less effort.   
#' 
#' The format parameter can be identical to the as.POSIXct function parameter
#' or it can be simplified to simply "american", "british", or "international". 
#' 
#' The function identifies the format from knowledge of the order.
#' 
#' The user identifies which format order.  This code will issue a warning 
#' if an obvious issue occurs with the format.  If the dataset is large
#' getting the format is critical as testing is limited to the first 1000 datapoints. 
#'   
#' format = "american"      ... date order is (1) mm (2) day (3) year
#' format = "british"       ... date order is (1) day (2) mm (3) year
#' format = "international" ... date order is (1) year (2) mm (3) day
#' 
#' The time of day may not be part of the character string included with the
#' date.  If it is not then the timeIfSeparateFromDate paramter can include 
#' a text string with the time. 
#' 
#' This function does not work if the data is already converted to a 
#' POSIXct time format.  
#' 
#' @param dateToReformat the date and or time an event occured
#' @param timeIfSeparateFromDate the time of an event as a character array 
#' @param format format string to output results
#' @param tz time zone to use
#' @keywords time
#' @examples
#'  
#'    reformatTime()
#' @export

reformatTime <- function(dateToReformat, 
                         timeIfSeparateFromDate = NULL, 
                         dateFormat = "american", 
                         tz = "Etc/GMT+6",
                         ...
                         ){
  
  # check if input is already a date
  if (is(dateToReformat, "POSIXct") && !is.factor(dateToReformat) && 
      !is.character(dateToReformat)){
    stop(paste0(deparse(substitute(dateToReformat)), 
                " is already in time format.  No reformatting done."))
  }
  
  # guards to check input parameters
  dateToReformat <- as.character(dateToReformat)
  dateFormat <- tolower(as.character(dateFormat))
  

  dateInfo      <- testCharacterDate (dateToReformat, ...)
  dateseparator <- dateInfo$dateSeparator
  dateTimeSeparator <- dateInfo$dateTimeSeparator
  formatStyle   <- tolower(dateInfo$formatStyle)
  isFullYear    <- dateInfo$yearFormat == "Full"
  isDateTime    <- dateInfo$timePresent
 
    
  # assemble the format expression for the as.POSIXct function
  formatForPOSIXct <- function(whichFormat, isDateTime, isFullYear){
    
    dateOrderPOSIXct <- list(
      american = c("%m", "%d", "%y"),
      british = c("%d", "%m", "%y"),
      international = c("%y", "%m", "%d")
    )
    
    if(isFullYear){
      dateOrderPOSIXct <- lapply(dateOrderPOSIXct, 
                                 function(x){c(gsub("y", "Y",x))})}
    
    returnFormat <- paste0(dateOrderPOSIXct[[whichFormat]][1], dateseparator,
                           dateOrderPOSIXct[[whichFormat]][2], dateseparator, 
                           dateOrderPOSIXct[[whichFormat]][3])
    
    if(isDateTime){
      returnFormat <-  paste0(returnFormat, dateTimeSeparator, "%H:%M:%S")
    } 
    
    return(returnFormat)
  }
  
  
  if (formatStyle != "unknown" && !is.null(dateFormat)){
    modifiedFormat <- formatForPOSIXct(formatStyle, isDateTime, isFullYear)
  }else{
    modifiedFormat <- dateFormat
  }

  return(as.POSIXct(strptime(as.character(dateToReformat), 
                             format = modifiedFormat)))
  
}

