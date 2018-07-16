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
#' @param dateFormat either ("american", "british", "international") or the strptime format parameter. 
#' @param defaultStyle if an indeterminate date is found then the default style is used 
#' (either american or british, international dates are usually not indeterminant because of the leading year)
#' @param tz time zone to use
#' @keywords time
#' @examples
#'  
#'    reformatTime("1/31/2012T00:00:00")
#'    
#'    reformatTime("2018-01-05 13:45:01")
#'    
#'    # This will generate a warning because it could be either 
#'    #     American or British format
#'    reformatTime("01-01-2010 12:00:00")
#'    
#'    reformatTime("01-01-2010 12:00:00", dateFormat = "American")
#'    
#'    reformatTime("4/30/2018 15:07", dateFormat = "American")
#'    
#' @export

reformatTime <- function(dateToReformat, 
                         timeIfSeparateFromDate = NULL, 
                         dateFormat = NULL, 
                         tz = "Etc/GMT+6",
                         defaultStyle = "american",
                         ...
){
  
  
  dateInfo <- list(
    value = dateToReformat,
    isPOSIXct = is(dateToReformat, "POSIXct"),
    isFactor = is.factor(dateToReformat),
    isCharacter = is.character(dateToReformat),
    charValue = as.character(dateToReformat),
    isEmpty = length(dateToReformat) == 0,
    paramValue = deparse(substitute(dateToReformat))
  )
  
  # check 1: Is input is already a date?
  if (dateInfo$isPOSIXct){
    stop(paste0(dateInfo$paramValue, 
                " is already in time format.  No reformatting done."))
  }
  
  # check 2: Are there elements in the date? 
  if (dateInfo$isEmpty) {
    stop(paste0(dateInfo$paramValue, " has 0 rows.  No reformatting done."))
  }  
  
  formatInfo <- list(
    lower = tolower(as.character(dateFormat)),
    STYLES = c("american", "british", "international"),
    isEmpty = length(dateFormat) == 0
  )
  
  formatInfo$isUserStyle <- isTRUE(formatInfo$lower %in% formatInfo$STYLES)

  formatInfo$userInputStyle <- if(formatInfo$isUserStyle){
    formatInfo$lower}else{"none"}

  formatInfo$autoFormat = formatInfo$isUserStyle || formatInfo$isEmpty

  if(formatInfo$autoFormat){
    formatInfo$chardateInfo <- testCharacterDate(dateInfo$charValue) #, ...)
    formatInfo$isFullYear <- formatInfo$chardateInfo$yearFormat == "Full"
    formatInfo$isDateTime <- formatInfo$chardateInfo$timePresent
    formatInfo$secondsPresent <- formatInfo$chardateInfo$secondsPresent
    formatInfo$style <- if (formatInfo$isEmpty){
      tolower(formatInfo$chardateInfo$formatStyle)
    }else{
      formatInfo$userInputStyle
    }
    
    if(!isTRUE(formatInfo$style %in% formatInfo$STYLES)){
      warning("Time format style is indeterminate, setting default")
      formatInfo$style <- defaultStyle
    }
    
    formatInfo$dateSeparator <- formatInfo$chardateInfo$dateSeparator
    formatInfo$dateTimeSeparator <- formatInfo$chardateInfo$dateTimeSeparator
  }

  assembleFormat <- function(formatInformation){
    
    DATEORDER <- list(american = c("%m", "%d", "%y"),
                      british = c("%d", "%m", "%y"),
                      international = c("%y", "%m", "%d"))
    
    if(formatInformation$isFullYear){
      DATEORDER <- lapply(DATEORDER, function(x){c(gsub("y", "Y",x))})
    }
    
    returnFormat <- paste0(DATEORDER[[formatInformation$style]][1], 
                           formatInformation$dateSeparator,
                           DATEORDER[[formatInformation$style]][2], 
                           formatInformation$dateSeparator, 
                           DATEORDER[[formatInformation$style]][3])
    
    if(formatInformation$isDateTime){
      returnFormat <- paste0(returnFormat, 
                             formatInformation$dateTimeSeparator, 
                             if(formatInformation$secondsPresent){
                               "%H:%M:%S"
                             }else{
                               "%H:%M"
                             })
    } 
    return(returnFormat)
  }
  
  if (formatInfo$autoFormat){
    modifiedFormat <- assembleFormat(formatInfo)
  }else{
    modifiedFormat <- dateFormat
  }
  
  return(as.POSIXct(strptime(dateInfo$charValue, 
                             format = modifiedFormat)))
}
