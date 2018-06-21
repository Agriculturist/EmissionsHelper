#' reformatTime
#'
#' This function is a wrapper for the as.POSIXct function. This function is 
#' designed require less effort.   
#' 
#' The format parameter can be identical to the as.POSIXct function parameter
#' or it can be simplified to simply "American", "British", or "International". 
#' 
#' The function identifies the format from knowledge of the order.
#' 
#' The user identifies which format order.  This code will issue a warning 
#' if an obvious issue occurs with the format.  If the dataset is large
#' getting the format is critical as testing is limited to the first 1000 datapoints. 
#'   
#' format = "American"      ... date order is (1) month (2) day (3) year
#' format = "British"       ... date order is (1) day (2) month (3) year
#' format = "International" ... date order is (1) year (2) month (3) day
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
                         dateFormat = "American", 
                         tz = "Etc/GMT+6",
                         dateTimeDelimiter = "\\s"){
  
  if (!is(dateToReformat, "POSIXct")){
    warning ("dataToReformat parameter is already in time format.  No reformatting done.")
    
  }else{
    lowerFormat <- tolower(as.character(dateFormat))
    
    if(lowerFormat %in% c("american","british","international")){
      
      separator <- if(grepl("[/]", dateToReformat[1]) > 0){"/"}else{
        if(grepl("[/.]", dateToReformat[1]) > 0){"."}else{
          if(grepl("[-]", dateToReformat[1]) > 0){"-"}else{" "}
        }
      }
      
      sepRegEx <- "[\\/\\.\\-]"
      dayRegEx <- "[0-3]?[0-9]"
      monthRegEx <- "[0-1]?[0-9]"
      yearRegEx <- "[0-2]?[0-9]?[0-9][0-9]"
      fullyearRegEx <- "[0-2][0-9][0-9][0-9]"
      HHMMSSRegEx <- "[0-2]?[0-9]:[0-6]?[0-9]:[0-6]?[0-9]"
      
      testDates <- function(testDateData, ...) {
        if (length(testDateData) < 1000){ # if dataset is big it only tests with first 1000 points
          return(all(as.logical(sapply(testDateData, function(x){
            grepl(paste0(...), x)}))))
        }else{
          return(all(as.logical(sapply(testDateData[1:999], function(x){
            grepl(paste0(...), x)}))))    
        }
      }
      
      isAmericanDateTime <- testDates(
        dateToReformat, 
        "^", monthRegEx, sepRegEx, dayRegEx, sepRegEx, yearRegEx, 
        dateTimeDelimiter, HHMMSSRegEx, "$")
      
      isAmericanDate <- testDates(
        dateToReformat,
        "^", monthRegEx, sepRegEx, dayRegEx, sepRegEx, yearRegEx, 
        "$")
      
      
      isBritishDateTime <- testDates(
        dateToReformat, 
        "^", dayRegEx, sepRegEx, monthRegEx, sepRegEx, yearRegEx, 
        dateTimeDelimiter, HHMMSSRegEx, "$")
      
      isBritishDate <- testDates(
        dateToReformat, 
        "^", dayRegEx, sepRegEx, monthRegEx, sepRegEx, yearRegEx, 
        "$")
      
      isInternationalDateTime <- testDates(
        dateToReformat, 
        "^", yearRegEx, sepRegEx, monthRegEx, sepRegEx, dayRegEx, 
        dateTimeDelimiter, HHMMSSRegEx, "$")
      
      isInternationalDate <- testDates(
        dateToReformat, 
        "^", yearRegEx, sepRegEx, monthRegEx, sepRegEx, dayRegEx, 
        "$")
      
      if(lowerFormat %in% "american"){
        
        if (!(isAmericanDateTime || isAmericanDate)){
          warning("The date does not appear to be formatted as an American date.")
        }  
        
        if (isAmericanDateTime){
          if (grepl(paste0("^", monthRegEx, sepRegEx, dayRegEx, sepRegEx, 
                           fullyearRegEx, dateTimeDelimiter, HHMMSSRegEx, "$"), dateToReformat)){
            modifiedFormat = paste0( "%m", separator,  "%d" , separator, "%Y",  
                                     dateTimeDelimiter, "%H:%M:%S")           
          }else{
            modifiedFormat = paste0( "%m", separator,  "%d" , separator, "%y",  
                                     dateTimeDelimiter, "%H:%M:%S")             
          }
        }
        
        if(isAmericanDate){
          if (grepl(paste0("^", monthRegEx, sepRegEx, dayRegEx, sepRegEx, 
                           fullyearRegEx, "$"), dateToReformat)){
            modifiedFormat = paste0( "%m", separator,  "%d" , separator, "%Y")           
          }else{
            modifiedFormat = paste0( "%m", separator,  "%d" , separator, "%y")            
          }
        }
      }
      
      if(lowerFormat %in% "british"){
        if (!(isBritishDateTime || isBritishDate )){
          warning("The date does not appear to be formatted as a British date.")
        }  
        
        if (isBritishDateTime){
          if (grepl(paste0("^", dayRegEx, sepRegEx, monthRegEx, sepRegEx,  
                           fullyearRegEx, dateTimeDelimiter, HHMMSSRegEx, "$"), dateToReformat)){
            modifiedFormat = paste0( "%d", separator,  "%m" , separator, "%Y",  
                                     dateTimeDelimiter, "%H:%M:%S")
          }else{
            modifiedFormat = paste0( "%d", separator,  "%m" , separator, "%y",  
                                     dateTimeDelimiter, "%H:%M:%S")
          }
        } 
        
        if(isBritishDate){
          if (grepl(paste0("^", dayRegEx, sepRegEx, monthRegEx, sepRegEx,  
                           fullyearRegEx, "$"), dateToReformat)){
            modifiedFormat = paste0( "%d", separator,  "%m" , separator, "%Y")         
          }else{
            modifiedFormat = paste0( "%d", separator,  "%m" , separator, "%Y")            
          }
        }      
        
        
        
      }    
      
      if(lowerFormat %in% "international"){
        if (!(isInternationalDateTime || isInternationalDate)){
          warning("The date does not appear to be formatted as an International date.")
        }       
        
        if (isInternationalDateTime){
          if (grepl(paste0("^", fullyearRegEx, sepRegEx, monthRegEx, sepRegEx,
                           dayRegEx, dateTimeDelimiter, HHMMSSRegEx, "$"), dateToReformat)){
            modifiedFormat = paste0( "%Y", separator,  "%m" , separator, "%d",  
                                     dateTimeDelimiter, "%H:%M:%S")         
          }else{
            modifiedFormat = paste0( "%y", separator,  "%m" , separator, "%d",  
                                     dateTimeDelimiter, "%H:%M:%S")             
          }
        } 
        
        if(isInternationalDate){
          if (grepl(paste0("^", fullyearRegEx, sepRegEx, monthRegEx, sepRegEx,
                           dayRegEx, "$"), dateToReformat)){
            modifiedFormat = paste0( "%Y", separator,  "%m" , separator, "%d")           
          }else{
            modifiedFormat = paste0( "%y", separator,  "%m" , separator, "%d")             
          }
        }     
        
      }     
    }else{
      modifiedFormat <- dateFormat
    }
    
    
    
  }
  
  
  return(as.POSIXct(strptime(as.character(dateToReformat), format = modifiedFormat)))
  
}

