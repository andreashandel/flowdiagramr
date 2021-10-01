#' Updates the x position of labels running vertically.
#'
#'
#' @param flows An edge data frame.
#'
#' @return The edge data frame.
#'
#' @export
#'
update_straight_labels <- function(flows) {
  # find the rows where flows are straight and the xmin == xmax, this
  # implies a vertical arrow
  ids <- which(flows$curvature == 0 & flows$xmin == flows$xmax)

  # add a small offset to move the label to the right of the arrow,
  # otherwise the label is right on top of the arrow.
  flows[ids, "xlabel"] <- flows[ids, "xlabel"] + 0.5
  return(flows)
}
