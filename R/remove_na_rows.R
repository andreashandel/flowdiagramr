#' Remove rows with no location information.
#'
#' @param df A data frame.
#' @return A data frame.
#' @export

remove_na_rows <- function(df) {
  if(nrow(df) != 0) {  # only do this if there are rows to process (almost always)
    rmrow <- logical(length = nrow(df))  # create a logical vector

    # set all elements to TRUE, meaning all rows will be retained unless
    # the rmrow element is updated to FALSE below
    rmrow[] <- TRUE
    for(i in 1:nrow(df)) {
      tmp <- df[i, ]  # work with one row at a time
      if(
        is.na(tmp$xmin) & is.na(tmp$ymin) &
        is.na(tmp$xmax) & is.na(tmp$ymax)
      ) {
        rmrow[i] <- FALSE  # set to FALSE if there is no location information
      }
    }
    df <- df[rmrow, ]  # only keep rows with location information (TRUE vals in rmrow)
  }
  return(df)
}
