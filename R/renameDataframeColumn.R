#' renameDataframeColumn
#'
#' This function renames columns in R.
#'
#'
#' @param dataframe the dataframe to edit
#' @param oldColumnName the old column name (Can also be the number of the column)
#' @param newColumnName the new name to change the old name to
#' @return same dataframe except with columns renamed
#' @keywords rename dataframe column
#' @examples
#' 
#' df <- data.frame(Column1 = 1, Column2 = 2)
#' renameDataframeColumn(df, 'Column1', 'ColumnX')
#' renameDateframeColumn(df, 2, 'ColumnY')
#'
#' @export

renameDataframeColumn <- function(dataframe, oldColumnName, newColumnName){
  
  if (is.numeric (oldColumnName)){
    names(dataframe)[oldColumnName] <- newColumnName 
  }else{
    names(dataframe)[names(dataframe) == oldColumnName] <- newColumnName
  }
  
  return(dataframe)
} 