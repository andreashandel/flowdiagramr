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


  # AH: turning off until updated to work with actual location values not row

  # # add in row info
  # flows <- merge(flows, variables[ , c("id", "row")], by.x = "to", by.y = "id")
  # flows$row <- as.numeric(flows$row)
  #
  # # Update curvature based on row, if only 2 rows
  # if(max(as.numeric(variables$row)) > 1 & max(as.numeric(variables$row)) <= 2) {
  #   # just get the edges where curvature needs to be greater/less than than 0
  #   # these are  interactions or arrows that connect non-contiguous nodes
  #   tmp <- subset(flows, (diff > 1 & diff < 9000) & (to != from) | interaction == TRUE)
  #
  #   # set curvature to 0.25 if on the first row so the arrow curves upward
  #   # if on the second row, set to -0.25 so the arrow curves downward
  #   tmp$curvature <- ifelse(tmp$row == 1, 0.25, -0.25)
  #
  #   # add tmp curvature back into the flows data frame
  #   flows[(flows$diff > 1 & flows$diff < 9000) &
  #         (flows$to != flows$from) |
  #         flows$interaction == TRUE, "curvature"] <- tmp$curvature
  #
  #   ### TODO Delete this after testing...
  #   # also update ystart and yend
  #   # cdf$ystart <- ifelse(cdf$row == 2, cdf$ystart-1, cdf$ystart)
  #   # cdf$yend <- ifelse(cdf$row == 2, cdf$ystart, cdf$yend)
  # }

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
        flows[i, "curvature"] <- 0.4
      }
    }

    ### TODO Delete after testing
    # if(!is.na(cdf[i, "linkfrom"])) {
    #   # curves need to move up 0.5 units to connect with tops/bottoms
    #   # of node rectangles, only when linking to horizontal flow
    #   cdf[i, "ystart"] <- cdf[i, "ystart"] + 0.5
    #   cdf[i, "yend"] <- cdf[i, "yend"]  + 0.5
    #   cdf[i, "ymid"]  <- cdf[i, "ymid"]  + 0.5
    #
    #   # if curve is for an interaction term, then yend needs to be moved
    #   # back down by 0.5 to meet up with the edge rather than the node
    #   if(cdf[i, "interaction"] == TRUE) {
    #     cdf[i, "yend"] <- cdf[i, "yend"] - 0.5
    #   }
    # }

    # if(is.na(cdf[i, "linkfrom"])) {
    #   s <- cdf[i, "xstart"]
    #   e <- cdf[i, "xend"]
    #   if(s < e) {
    #     cdf[i, "xstart"] <- cdf[i, "xstart"] + 0.5
    #     cdf[i, "xend"] <- cdf[i, "xend"]  - 0.5
    #     cdf[i, "xmid"]  <- cdf[i, "xmid"]  - 0.5
    #   }
    #   if(s > e) {
    #     cdf[i, "xstart"] <- cdf[i, "xstart"] - 0.5
    #     cdf[i, "xend"] <- cdf[i, "xend"]  + 0.5
    #     cdf[i, "xmid"]  <- cdf[i, "xmid"]  + 0.5
    #   }
    #   if(s == e) {
    #     sy <- cdf[i, "ystart"]
    #     ey <- cdf[i, "yend"]
    #     if(sy > ey) {
    #       cdf[i, "ystart"] <- cdf[i, "ystart"] - 0.5
    #       cdf[i, "yend"] <- cdf[i, "yend"] + 0.5
    #       cdf[i, "ymid"]  <- cdf[i, "ymid"]  + 0.5
    #     } else {
    #       cdf[i, "ystart"] <- cdf[i, "ystart"] + 0.5
    #       cdf[i, "yend"] <- cdf[i, "yend"] - 0.5
    #       cdf[i, "ymid"]  <- cdf[i, "ymid"]  - 0.5
    #     }
    #   }
    #
    # }
  }

  # add curvature midpoint for accurate label placement
  for(i in 1:nrow(flows)) {
    tmp <- flows[i, ]

    # only update labels for arrows with a curve
    if(tmp$curvature != 0) {

      # this if statement identifies "regular" connections from nodes to
      # arrows or nodes to nodes. Otherwise, we are dealing with interactions
      # that go from an arrow to an arrow, these get special treatment following
      # the "else" statement below
      if(!is.na(tmp$linkfrom) | (is.na(tmp$linkfrom) & (!is.na(tmp$to) | !is.na(tmp$from)))) {
        mids <- calc_control_points(x1 = tmp$xmin,
                                    y1 = tmp$ymin,
                                    x2 = tmp$xmax,
                                    y2 = tmp$ymax,
                                    angle = 90,
                                    curvature = tmp$curvature,
                                    ncp = 1)
        flows[i, "xlabel"] <- mids$x
        flows[i, "ylabel"] <- mids$y + 0.15  # move above the midpoint
        if(flows[i, "curvature"] == 0.7) {
          # this indicates vertical alignment, so x location of label
          # needs to be nudged away from the larger curve.
          flows[i, "xlabel"] <- mids$x + 0.15
        }
      } else {
        # here we move the label just a bit away from the arrow
        # bevcause these tend to be in slightly different positions
        # really close to other arrows and nodes
        s <- flows[i, "xmin"]
        e <- flows[i, "xmax"]
        if(s < e) {
          flows[i, "xlabel"] <- tmp$xmax - 0.25
          flows[i, "ylabel"] <- tmp$ymax - 0.25
        }
        if(s > e) {
          flows[i, "xlabel"] <- tmp$xmax + 0.15
          flows[i, "ylabel"] <- tmp$ymax + 0.15
        }
      }
    }
  }


  # add y offset to curve labels according to row, only when
  # curvature is not 0
  # AH: turning off for now, uses row. also not clear what's special about row 2, why is that hard-coded?
  # for(i in 1:nrow(flows)) {
  #   tmp <- flows[i, ]
  #   if(tmp$curvature != 0) {
  #     offset <- ifelse(flows[i, "row"] == 2, -0.2, 0.2)
  #   } else {
  #     offset <- 0
  #   }
  #   flows[i, "ylabel"] <- flows[i, "ylabel"] + offset
  # }

  return(flows)
}
