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
#' getting the format is critical as testing is limited to the first 100 datapoints. 
#' 
#' testChararacterDate parameters may be entered into the function
#' 
#' If dateformat is blank the system will guess at the date format.
#'   
#' Because the American and British styles can be confused it may be necissary
#' to specify the format.
#'
#' dateformat = "American"      ... date order is (1) mm (2) day (3) year
#' dateformat = "British"       ... date order is (1) day (2) mm (3) year
#' dateformat = "International" ... date order is (1) year (2) mm (3) day
#' 
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
#' @param dateformat either ("american", "british", "international") or the strptime format parameter. 
#' @param tz time zone to use
#' @keywords time
#' @examples
#'  
#'    reformatTime("1/31/2012T00:00:00")
#'    
#'    reformatTime("2018-01-05 13:45:01")
#'    
#'    # This will generate an error because it could be either 
#'    #     American or British format
#'    reformatTime("01-01-2010 12:00:00")
#'    
#'    reformatTime("01-01-2010 12:00:00", dateFormat = "American")
#'    
#' @export

reformatTime <- function(dateToReformat, 
                         timeIfSeparateFromDate = NULL, 
                         dateFormat = NULL, 
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
  

  if (length(dateFormat) == 0 ||  
      isTRUE(tolower(dateFormat) %in% 
             c("american", "british", "international"))){
    
    
    dateInfo      <- testCharacterDate (dateToReformat, ...)
    dateseparator <- dateInfo$dateSeparator
    dateTimeSeparator <- dateInfo$dateTimeSeparator
    if (length(dateFormat) == 0){
      formatStyle <- tolower(dateInfo$formatStyle)
    }else{
      formatStyle <- dateFormat
    }    
    isFullYear    <- dateInfo$yearFormat == "Full"
    isDateTime    <- dateInfo$timePresent

    
  }
    
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
    if (length(dateFormat) == 0){
      stop(paste("Date format is indeterminant.",
                    "Try setting the dateformat parameter."))
    }else{
      modifiedFormat <- dateFormat
    }
  }

  return(as.POSIXct(strptime(as.character(dateToReformat), 
                             format = modifiedFormat)))
  
}

