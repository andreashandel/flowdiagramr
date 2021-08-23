#' Remove rows with no location information.
#'
#' @param df A data frame.
#' @return A data frame.
#' @noRd

remove_na_rows <- function(df) {
  if(nrow(df) != 0) {
    rmrow <- logical(length = nrow(df))
    rmrow[] <- TRUE
    for(i in 1:nrow(df)) {
      tmp <- df[i, ]
      if(
        is.na(tmp$xmin) & is.na(tmp$ymin) &
        is.na(tmp$xmax) & is.na(tmp$ymax)
      ) {
        rmrow[i] <- FALSE
      }
    }
    df <- df[rmrow, ]
  }
  return(df)
}
