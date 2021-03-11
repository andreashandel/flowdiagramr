#' Adds row column to user supplied nodes data frame.
#'
#' @param df A data frame.
#' @return A data frame.
#' @noRd

add_rowid <- function(df) {
  ys <- df$y

  if(is.null(ys)) {
    stop("The data frame does not have a 'y' column.")
  }

  df$row <- as.numeric(as.factor(ys))
  df <- df[ , c("id", "label", "row", "x", "y")]
  return(df)
}
