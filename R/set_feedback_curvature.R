#' Sets the curvature values for feedback arrows
#'
#' @param flows flows data frame
#' @return Updated flows data frame
#' @export

set_feedback_curvature <- function(flows) {
  # first find the feedback edges by iterative subsetting
  sdf <- subset(flows, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  fdf <- subset(sdf, to == from)

  if(nrow(fdf) > 0) {
    # now adjust the xs to get arrow above and feeding back into the node
    fdf$curvature <- -2  # default value for big loop
    fdf$ylabel <- fdf$ylabel + 0.4  # this gets the label just above the loop

    # just replace the rows in flows that are in fdf (our updates)
    toreplace <- match(paste0(flows$to, flows$from), paste0(fdf$to, fdf$from))
    toreplace <- which(!is.na(toreplace))
    flows[toreplace, ] <- fdf
    return(flows)
  } else {
    # return the same object if there are no feedback edges
    return(flows)
  }
}
