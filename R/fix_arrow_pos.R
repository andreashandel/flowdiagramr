#' Adjust arrow positions to avoid overlaps.
#' helper function for prepare_diagram that is only applied to
#' arrows from nowhere or to nowhere (e.g., birth or death arrows).
#'
#' @param flows The flows data frame.
#' @export

fix_arrow_pos <- function(flows) {
  # subset flows down to the flows that are into or out of the system
  sdf <- subset(flows, (is.na(from) | is.na(to)) & interaction == FALSE)

  # pull just the location information
  v <- sdf[ , c("xmin", "ymin", "xmax", "ymax")]

  # find duplicates in any column
  reps <- duplicated(v) | duplicated(v, fromLast = TRUE)

  # if there are duplicates, apply a small offset to move the first
  # encounter to the left and the second encounter to the right.
  # note that this is limited to just two instances for now.
  if(length(reps > 0)) {
    tmp <- sdf[reps, ]

    off <- 0.25
    tmp[1, "xmin"] <- tmp[1, "xmin"] - off
    tmp[1, "xmax"] <- tmp[1, "xmax"] - off
    # tmp[1, "xlabel"] <- tmp[1, "xlabel"] - off
    tmp[2, "xmin"] <- tmp[2, "xmin"] + off
    tmp[2, "xmax"] <- tmp[2, "xmax"] + off
    # tmp[2, "xlabel"] <- tmp[2, "xlabel"] + off

    out <- rbind(sdf[!reps, ], tmp)
  } else {
    out <- sdf
  }

  # remove rows with all NAs
  tokeep <- which(!(is.na(out$to) & is.na(out$from)))
  out <- out[tokeep, ]

  # only replace rows in the full dataframe that we have potentially adjusted
  toreplace <- match(rownames(flows), rownames(sdf))
  toreplace <- which(!is.na(toreplace))
  flows[toreplace, ] <- out
  return(flows)

}
