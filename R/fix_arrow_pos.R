#' Adjust arrow positions to avoid overlaps.
#' helper function for prepare_diagram that is only applied to
#' arrows from nowhere or to nowhere (e.g., birth or death arrows).
#'
#' @param edf The edges (flows) data frame.
#' @export

fix_arrow_pos <- function(edf) {
  # subset flows down to the flows from/to dummies (into or out of the system)
  # these are identified by any diff less than 1 or greater than 9000
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  # here we make sure the diff is in the 9900 range in absolute units. this
  # avoids any interaction links in the abs(5000) range.
  vdf <- subset(sdf, abs(diff) >= 9900)
  # pull just the location information
  v <- vdf[ , c("xmin", "ymin", "xmax", "ymax")]
  # find duplicates in any column
  reps <- duplicated(v) | duplicated(v, fromLast = TRUE)

  # if there are duplicates, apply a small offset to move the first
  # encounter to the left and the second encounter to the right.
  # note that this is limited to just two instances for now.
  if(length(reps > 0)) {
    tmp <- vdf[reps, ]

    off <- 0.25
    tmp[1, "xmin"] <- tmp[1, "xmin"] - off
    tmp[1, "xmax"] <- tmp[1, "xmax"] - off
    tmp[1, "xlabel"] <- tmp[1, "xlabel"] - off
    tmp[2, "xmin"] <- tmp[2, "xmin"] + off
    tmp[2, "xmax"] <- tmp[2, "xmax"] + off
    tmp[2, "xlabel"] <- tmp[2, "xlabel"] + off

    out <- rbind(vdf[!reps, ], tmp)
  } else {
    out <- vdf
  }

  # remove rows with all NAs
  tokeep <- which(!(is.na(out$to) & is.na(out$from)))
  out <- out[tokeep, ]

  # only replace rows in the full dataframe that we have potentially adjusted
  toreplace <- match(paste0(edf$to, edf$from), paste0(vdf$to, vdf$from))
  toreplace <- which(!is.na(toreplace))
  edf[toreplace, ] <- out
  return(edf)

}
