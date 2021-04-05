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
        is.na(tmp$xstart) & is.na(tmp$ystart) &
        is.na(tmp$xend) & is.na(tmp$yend)
      ) {
        rmrow[i] <- FALSE
      }
    }
    df <- df[rmrow, ]
  }
  return(df)
}
