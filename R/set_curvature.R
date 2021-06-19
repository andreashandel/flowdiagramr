#' Sets the curvature values for curved arrows. used by prepare_diagram
#'
#' @param cdf Curved edge df
#' @param ndf Nodes df
#' @return A data frame
#' @noRd

set_curvature <- function(cdf, ndf) {
  # default for all to start
  cdf$curvature <- 0.5

  # add in row info
  cdf <- merge(cdf, ndf[ , c("id", "row")], by.x = "to", by.y = "id")
  cdf$row <- as.numeric(cdf$row)

  # Update curvature based on row, if only 2 rows
  if(max(as.numeric(ndf$row)) > 1 & max(as.numeric(ndf$row)) <= 2) {
    cdf$curvature <- ifelse(cdf$row == 1, 0.25, -0.25)

    # also update ystart and yend
    cdf$ystart <- ifelse(cdf$row == 2, cdf$ystart-1, cdf$ystart)
    cdf$yend <- ifelse(cdf$row == 2, cdf$ystart, cdf$yend)
  }

  cdf[cdf$interaction==TRUE, "curvature"] <- 0.4

  for(i in 1:nrow(cdf)) {
    if(cdf[i, "interaction"] == TRUE & is.na(cdf[i, "linkfrom"])) {
      cdf[i, "curvature"] <- 0.1
    }
    if(cdf[i, "interaction"] == TRUE & !is.na(cdf[i, "linkfrom"])) {
      if(cdf[i, "xmid"] == cdf[i, "xend"]) {
        # this indicates that the alignment is vertical, requiring
        # more curvature to bend around the top of the box
        cdf[i, "curvature"] <- 0.7
      } else {
        cdf[i, "curvature"] <- 0.4
      }
    }

    if(!is.na(cdf[i, "linkfrom"])) {
      # curves need to move up 0.5 units to connect with tops/bottoms
      # of node rectangles, only when linking to horizontal flow
      cdf[i, "ystart"] <- cdf[i, "ystart"] + 0.5
      cdf[i, "yend"] <- cdf[i, "yend"]  + 0.5
      cdf[i, "ymid"]  <- cdf[i, "ymid"]  + 0.5

      # if curve is for an interaction term, then yend needs to be moved
      # back down by 0.5 to meet up with the edge rather than the node
      if(cdf[i, "interaction"] == TRUE) {
        cdf[i, "yend"] <- cdf[i, "yend"] - 0.5
      }
    }

    if(is.na(cdf[i, "linkfrom"])) {
      s <- cdf[i, "xstart"]
      e <- cdf[i, "xend"]
      if(s < e) {
        cdf[i, "xstart"] <- cdf[i, "xstart"] + 0.5
        cdf[i, "xend"] <- cdf[i, "xend"]  - 0.5
        cdf[i, "xmid"]  <- cdf[i, "xmid"]  - 0.5
      }
      if(s > e) {
        cdf[i, "xstart"] <- cdf[i, "xstart"] - 0.5
        cdf[i, "xend"] <- cdf[i, "xend"]  + 0.5
        cdf[i, "xmid"]  <- cdf[i, "xmid"]  + 0.5
      }
      if(s == e) {
        sy <- cdf[i, "ystart"]
        ey <- cdf[i, "yend"]
        if(sy > ey) {
          cdf[i, "ystart"] <- cdf[i, "ystart"] - 0.5
          cdf[i, "yend"] <- cdf[i, "yend"] + 0.5
          cdf[i, "ymid"]  <- cdf[i, "ymid"]  + 0.5
        } else {
          cdf[i, "ystart"] <- cdf[i, "ystart"] + 0.5
          cdf[i, "yend"] <- cdf[i, "yend"] - 0.5
          cdf[i, "ymid"]  <- cdf[i, "ymid"]  - 0.5
        }
      }

    }
  }

  # add curvature midpoint for accurate label placement
  cdf$labelx <- NA
  cdf$labely <- NA
  for(i in 1:nrow(cdf)) {
    tmp <- cdf[i, ]
    if(!is.na(tmp$linkfrom) | (is.na(tmp$linkfrom) & (!is.na(tmp$to) | !is.na(tmp$from)))) {
      mids <- calc_control_points(x1 = tmp$xstart,
                                  y1 = tmp$ystart,
                                  x2 = tmp$xend,
                                  y2 = tmp$yend,
                                  angle = 90,
                                  curvature = tmp$curvature,
                                  ncp = 1)
      cdf[i, "labelx"] <- mids$x
      cdf[i, "labely"] <- mids$y
      if(cdf[i, "curvature"] == 0.7) {
        # this indicates vertical alignment, so x location of label
        # needs to be nudged away from the larger curve.
        cdf[i, "labelx"] <- mids$x + 0.15
      }
    } else {
      s <- cdf[i, "xstart"]
      e <- cdf[i, "xend"]
      if(s < e) {
        cdf[i, "labelx"] <- tmp$xend - 0.25
        cdf[i, "labely"] <- tmp$yend - 0.25
      }
      if(s > e) {
        cdf[i, "labelx"] <- tmp$xend + 0.15
        cdf[i, "labely"] <- tmp$yend + 0.05
      }

    }

  }


  # add y offset to curve labels according to row
  for(i in 1:nrow(cdf)) {
    tmp <- cdf[i, ]
    offset <- ifelse(cdf[i, "row"] == 2, -0.2, 0.2)
    cdf[i, "labely"] <- cdf[i, "labely"] + offset
  }

  return(cdf)
}
