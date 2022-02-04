#' Fill in NA rows with the first element.
#'
#' @description This is a helper function that fills in empty rows
#' of a dataframe. The key assumption is that either all rows will be
#' filled or only the first row will be filled. Therefore, all NA
#' rows are given the value of the first element.
#' @param df A data frame.
#' @return A data frame.
#' @export

fill_down_rows <- function(df) {
  # function that fills NA elements with the first element
  for(i in 1:ncol(df)) {
    tmp <- df[,i]
    tmp[is.na(tmp)] <- tmp[1]
    df[,i] <- tmp
  }
  return(df)
}
