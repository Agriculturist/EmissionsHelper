#' reformatTime
#'
#' This function reformats time so it is always in a consistant format.
#'
#' This function is useful for importing date from a text file
#'
#' @param cdate the date an event occured
#' @param ctime the time of an event
#' @param format format string to output results
#' @param tz time zone to use
#' @keywords time
#' @examples
#'
#' @export

reformatTime <- function(cdate, ctime = NULL, format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+6"){
  if(is.null(ctime)){
    as.POSIXct(strptime(as.character(cdate), format = format))
  }else{
    as.POSIXct(strptime(mapply(paste, as.character(cdate), as.character(ctime)), 
                        format = format, tz = tz))
  }}
