#' testCharacterDate
#'
#' This function is intended to test a characters date that was 
#' recently converted from a text file.
#' 
#' It can extract features about the date using regular expressions
#' 
#'  This function assumes the dateSeparator is "-", ".", or "/"
#'   
#'   
#' format = "american"      ... date order is (1) mm (2) day (3) year
#' format = "british"       ... date order is (1) day (2) mm (3) year
#' format = "international" ... date order is (1) year (2) mm (3) day
#' 
#' 
#' This function outputs a list of named parameters
#'  
#'   dateSeparator     -  character used to separate dates
#'   dateTimeSeparator -  chacters used to separate date and time
#'   american          -  is this american formatted date 
#'   british           -  is this a British formatted date
#'   international     -  is this an internationl formatted date 
#'   fullYear          -  is a full year given
#'   partialYear       -  is a partial year given
#'   withTime          -  is the time given
#'   withoutTime       -  is time not given
#'   formatStyle       -  American, British, International, or Unknown
#'   yearFormat        -  Full or Partial
#'   timePresent       -  is the time given
#' 
#' @param characterDate the date and or time an event occured
#' @param testUpperLimit number of tests to perform to validate
#' @param testPercentPositive percent positive to confirm one date type
#' @keywords time
#' @examples
#'  
#'    reformatTime()
#' @export

testCharacterDate <- function(characterDate, 
                              testUpperLimit = 100, 
                              testPercentPositive = 50) {
  
  out <- list(dateSeparator = NULL,
              dateTimeSeparator = NULL,
              american = FALSE,
              british = FALSE,
              international = FALSE,
              fullYear = FALSE,
              partialYear = FALSE,
              withTime = FALSE,
              withoutTime = FALSE,
              formatStyle = NULL,
              yearFormat = NULL,
              timePresent = NULL
  )
  
  if(length(characterDate) < testUpperLimit){
    testUpperLimit <- length(characterDate)
  }
  if (testUpperLimit < 1){testUpperLimit <- 1}
  if (testPercentPositive > 100){testPercentPositive <- 100}
  if (testPercentPositive < 0){testPercentPositive <- 0}
  
  
  # identify dateSeparators
  out$dateSeparator <- if(grepl("[/]", characterDate[1])){
    "/"
  }else if(grepl("[/.]", characterDate[1])) {
    "."
  }else if(grepl("[-]", characterDate[1])){
    "-"
  }else{
    " "
  }
  
  # Regular Expression Parts
  
  sepRegEx <- "[\\/\\.\\-]"
  ddRegEx <- "[0-3]?[0-9]"
  mmRegEx <- "[0-1]?[0-9]"
  yyRegEx <- "[0-9][0-9]"
  YYRegEx <- "[0-2][0-9][0-9][0-9]"
  HHMMSSRegEx <- "[0-2]?[0-9]:[0-6]?[0-9]:[0-6]?[0-9]"
  
  # Regular Expression Order
  
  dateTypesRegEx <- list(partialYear =
                           list(american = c(mmRegEx, ddRegEx, yyRegEx),
                                british = c(ddRegEx, mmRegEx, yyRegEx),
                                international = c(yyRegEx, mmRegEx, ddRegEx)),
                         fullYear =
                           list(american = c(mmRegEx, ddRegEx, YYRegEx),
                                british = c(ddRegEx, mmRegEx, YYRegEx),
                                international = c(YYRegEx, mmRegEx, ddRegEx)))
  
  
  for(yearType in c("partialYear", "fullYear")){
    for(formatType in c("american", "british", "international")){
      
      DateRegEx <- paste0(dateTypesRegEx[[yearType]][[formatType]][1], sepRegEx, 
                          dateTypesRegEx[[yearType]][[formatType]][2], sepRegEx,
                          dateTypesRegEx[[yearType]][[formatType]][3])
      

      
      testVector <- as.logical(sapply(characterDate[1:testUpperLimit], 
                                      function(x){grepl(paste0("^", DateRegEx, "$"), x)})) 

      if((sum(testVector) / length(testVector) * 100 > testPercentPositive)){
        out[[yearType]] <- TRUE
        out[[formatType]] <- TRUE
        out$withoutTime <- TRUE
      }
      
      DateRegEx <- paste0(DateRegEx, "(\\D)", HHMMSSRegEx)
      

      
      testVector <- as.logical(sapply(characterDate[1:testUpperLimit], 
                                      function(x){grepl(paste0("^", DateRegEx, "$"), x)})) 
     
      if((sum(testVector) / length(testVector) * 100 > testPercentPositive)){
        
        out[[yearType]] <- TRUE
        out[[formatType]] <- TRUE
        out$withTime <- TRUE
       
        out$dateTimeSeparator <- gsub(DateRegEx, "\\1", 
                                      characterDate[min(which(testVector == TRUE))])
        
      }      
    }
  }                         
  
  
  if(sum(c(out$american, out$british, out$international)) == 1){
    if(out$american){
      out$formatStyle <- "American"
    } else if (out$british){
      out$formatStyle <- "British"
    } else if (out$international){
      out$formatStyle <- "International"
    } else {
      out$formatStyle <- "Unknown"
    }
  }else{
    out$formatStyle <- "Unknown"
  }
  
  if(sum(c(out$fullYear, out$partialYear)) == 1){
    if(out$fullYear){
      out$yearFormat <- "Full" 
    } else if(out$partialYear) {
      out$yearFormat <- "Partial"
    } else {
      out$yearFormat <- "Unknown"
    }
  }else if(sum(c(out$fullYear, out$partialYear)) > 1){
    warning("Format type unknown because multiple formats could be valid.")
    out$yearFormat <- "Unknown"
  }else{
    out$yearFormat <- "Unknown"
  }
  
  if(out$withTime && !out$withoutTime){
    out$timePresent <- TRUE
  }else if(!out$withTime && out$withoutTime){  
    out$timePresent <- FALSE
  }else{
    out$timePresent <- "unknown"
  }
  
  return(out)    
}  
