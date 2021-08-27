#' Update the x,y positions for external interaction flows
#'
#' @param ext The subsetting external interactions (extints).
#' @param ndf The variables (nodes) data frame.
#' @return The extints data frame with location information.
#' @noRd

update_external_interaction_positions <- function(ext, ndf) {

  if(nrow(ext > 0 )) {
    # merge with nodes to get new "from"/min positions
    ext <- merge(ext, ndf[, c("id", "xmin", "xmax", "ymin", "ymax")],
                 by.x = "from", by.y = "id")

    # loop over the exts and apply xmin based on direction of flow
    for (i in 1:nrow(ext)) {
      tmp <- ext[i,]
      dir <- tmp$from - tmp$to
      if(sign(dir) == -1) {
        # if going left to right, then set xmin to the xmax of the from node
        tmp$xmin <- tmp$xmax

        # ymin is the middle of the node
        tmp$ymin <- mean(c(tmp$ymax, tmp$ymin))
      }
      if(sign(dir) == 1) {
        # if going right to left, then set xmin to the xmin of the from node
        tmp$xmin <- tmp$xmin

        # ymin is the middle of the node
        tmp$ymin <- mean(c(tmp$ymax, tmp$ymin))
      }
      ext[i, ] <- tmp
    }

    # now remerge with nodes to get the "to"/max positions
    ext <- merge(ext, ndf[, c("id", "xmin", "xmax", "ymin", "ymax")],
                 by.x = "to", by.y = "id", suffixes = c(".flow",".var"))

    # loop over the exts and apply xmax and ymax based on direction of flow
    newext <- list()
    for (i in 1:nrow(ext)) {
      tmp <- ext[i,]
      dir <- tmp$from - tmp$to
      if(sign(dir) == -1) {
        # if going left to right, then set xmin to the xmax of the from node
        tmp$xmax <- tmp$xmin.var

        # ymin is the middle of the node
        tmp$ymax <- tmp$ymax.var + 0.25
      }
      if(sign(dir) == 1) {
        # if going right to left, then set xmin to the xmin of the from node
        tmp$xmax <- tmp$xmax.var

        # ymin is the middle of the node
        tmp$ymax <- tmp$ymin.var - 0.25
      }
      newext <- rbind(newext, tmp)
    }

    # rename the min positions to retain
    newext$xmin <- newext$xmin.flow
    newext$ymin <- newext$ymin.flow

    # define the label positions as mean of x,y plus a bump to get above the line
    # must loop over each row to get means correct
    for(i in 1:nrow(newext)) {
      newext[i, "xlabel"] <- mean(c(newext[i,"xmin"], newext[i, "xmax"]))
      newext[i, "ylabel"] <- mean(c(newext[i,"ymin"], newext[i, "ymax"]))
    }

    # add the diff column
    newext$diff <- with(newext, abs(to-from))

    # add the external interaction curvature specification
    newext$curvature <- 0.1

    # add row column as NA just to keep column names
    newext$row <- NA_real_

    # keep only relevant columns
    ext <- newext[ , c("to", "from", "label", "interaction", "direct_interaction",
                       "linkto", "linkfrom", "xmin", "xmax", "ymin", "ymax",
                       "xlabel", "ylabel", "diff", "curvature", "row")]
  }


  return(ext)
}
