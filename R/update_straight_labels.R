#' Updates the x position of labels running vertically.
#'
#'
#' @param flows An edge data frame.
#'
#' @return The edge data frame.
#'
#' @noRd
#'
update_straight_labels <- function(flows) {
  ids <- which(flows$curvature == 0 & flows$xmin == flows$xmax)
  flows[ids, "xlabel"] <- flows[ids, "xlabel"] + 0.5
  return(flows)
}
