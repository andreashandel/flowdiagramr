#' Sets the curvature values for curved arrows. used by prepare_diagram
#'
#'
#' @param flows flow data frame
#' @param variables variable data frame
#' @return An updated flow data frame
#' @export

set_curvature <- function(variables, flows) {
  # default for all to start
  flows$curvature <- 0  # straight lines

  # if the connection is an interaction, then set to different values of
  # curvature to avoid overlapping top of "from" node
  flows[flows$interaction==TRUE | flows$direct_interaction == TRUE, "curvature"] <- 0.4  # default to be updated if conditions below are met

  for(i in 1:nrow(flows)) {
    # set to lower curvature if the arrow is going from an "invisible" node
    # to another flow
    if(flows[i, "interaction"] == TRUE & is.na(flows[i, "linkfrom"])) {
      flows[i, "curvature"] <- 0.1
    }

    # set to higher values if the arrow is going from one node to
    # another. 0.7 is used for vertical alignments, 0.4 is used for
    # horizontal alignments
    if(flows[i, "interaction"] == TRUE & !is.na(flows[i, "linkfrom"])) {
      if(abs(flows[i, "ymin"] - flows[i, "ymax"]) > 0.5) {
        # this indicates that the alignment is vertical, requiring
        # more curvature to bend around the top of the box
        flows[i, "curvature"] <- 0.7
      } else {
        # this will be the "regular" horizontal aliment
        flows[i, "curvature"] <- 0.5
      }
    }
  }

  # add curvature midpoint for accurate label placement, easiest to loop
  # over rows to apply the calc_control_points function
  for(i in 1:nrow(flows)) {
    tmp <- flows[i, ]

    # only update labels for arrows with a curve
    if(tmp$curvature != 0) {
      mids <- calc_control_points(x1 = tmp$xmin,
                                  y1 = tmp$ymin,
                                  x2 = tmp$xmax,
                                  y2 = tmp$ymax,
                                  angle = 90,
                                  curvature = tmp$curvature,
                                  ncp = 1)
      flows[i, "xlabel"] <- mids$x
      flows[i, "ylabel"] <- mids$y
    }
  }

  return(flows)
}
