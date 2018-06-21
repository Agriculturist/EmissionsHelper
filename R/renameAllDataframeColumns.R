#' renameAllDataframeColumns
#'
#' This function renames all the columns in a dataframe.  
#' 
#' This function is useful because wind measuring equipment may output a less than
#' ideal format for R.  If column names are less than ideal for use in R 
#' this function can be used to change the names of all of the columns. 
#' 
#' The first parameter is the dataframe
#' 
#' The second parameter is the column names to change the header.
#'
#'
#' @param dataframe the dataframe to edit
#' @param newColumnNames the new column names 
#' @return same dataframe except with columns renamed
#' @keywords rename dataframe column
#' @examples
#' 
#' df <- data.frame(Column1 = 1, Column2 = 2)
#' renameAllDataframeColumns(df, c('ColumnA', 'ColumnB'))
#' 
#' @export

renameAllDataframeColumns <- function(dataframe, newColumnNames){
  
  if (ncol(dataframe) == length(newColumnNames)){
    names(dataframe) <- newColumnNames 
  }else{
    warning(paste0("The number of columns in the dataframe is ",  ncol(oldColumnNams), 
                   " and the number of elements in newColumnNames is ", 
                   length(newColumnNames)))
  }
  
  return(dataframe)
} 