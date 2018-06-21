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
#' If truncate columns is true then extra columns are removed.  This is useful
#' if a header does not import properly.
#'
#' @param dataframe the dataframe to edit
#' @param newColumnNames the new column names 
#' @param truncateColumns if TRUE this function removes extra columns not in the column names
#' @param truncateStartColumn starting column number truncation starts
#' @return same dataframe except with columns renamed
#' @keywords rename dataframe column
#' @examples
#' 
#' df <- data.frame(Column1 = 1, Column2 = 2)
#' renameAllDataframeColumns(df, c('ColumnA', 'ColumnB'))
#' 
#' @export

renameAllDataframeColumns <- function(dataframe, newColumnNames, 
                                      truncateColumns = FALSE,
                                      truncateStartColumn = 1){
  
  if (ncol(dataframe) == length(newColumnNames)){
    names(dataframe) <- newColumnNames 
  }else{
    if (truncateColumns){
      if (ncol(dataframe) > length(newColumnNames)){
        dataframe <- dataframe[,truncateStartColumn:length(newColumnNames)]  
        names(dataframe) <- newColumnNames 
      }else{
        warning(paste0("The number of columns in the dataframe is ",  ncol(dataframe), 
                       " and the number of elements in newColumnNames is ", 
                       length(newColumnNames)), "\n too many column names.")        
      }
    }else{
      warning(paste0("No renaming!\n",
                     "The number of columns in the dataframe is ",  ncol(dataframe), 
                     " and the number of elements in newColumnNames is ", 
                     length(newColumnNames)))      
    }
  }
  
  return(dataframe)
} 
