#' Adjust arrow positions to avoid overlaps. helper function for prepare_diagram
#'
#' @param edf The edges (flows) data frame.
#' @expoert

fix_arrow_pos <- function(edf) {
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  vdf <- subset(sdf, abs(diff) >= 9900)
  v <- vdf[ , c("xmin", "ymin", "xmax", "ymax")]
  reps <- duplicated(v) | duplicated(v, fromLast = TRUE)

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

  toreplace <- match(paste0(edf$to, edf$from), paste0(vdf$to, vdf$from))
  toreplace <- which(!is.na(toreplace))
  edf[toreplace, ] <- out
  return(edf)

}
