#' Sets the curvature values for feedback arrows
#'
#' @param edf Edges df
#' @return A data frame
#' @export

set_feedback_curvature <- function(edf) {
  # first find the feedback edges by iterative subsetting
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  fdf <- subset(sdf, to == from)

  if(nrow(fdf) > 0) {
    # now adjust the xs to get arrow above and feeding back into the node
    fdf$curvature <- -2  # default value for big loop
    fdf$ylabel <- fdf$ylabel + 0.4  # this gets the label just above the loop

    # just replace the rows in edf that are in fdf (our updates)
    toreplace <- match(paste0(edf$to, edf$from), paste0(fdf$to, fdf$from))
    toreplace <- which(!is.na(toreplace))
    edf[toreplace, ] <- fdf
    return(edf)
  } else {
    # return the same object if there are no feedback edges
    return(edf)
  }
}
