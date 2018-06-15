#' removeDataframeColumns
#'
#' This function is removes unneeded columns from datasets.
#'
#'
#' @param dataframe the dataframe to removed the columns from
#' @param ... the column names to be removed as character strings (quoted).
#' @return same dataframe with columns removed
#' @keywords Columns Dataframes
#' @examples
#' 
#' df <- data.frame(column1 = 1, column2 = 2, column3 = 3)
#' df <- removeDataframeColumns(df, 'column1', 'column3')
#'
#' @export

removeDataframeColumns <- function(dataframe, ...){
  return(within(dataframe , rm(...)))
 
} 