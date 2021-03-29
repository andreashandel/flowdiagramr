#' Adjust arrow positions to avoid overlaps
#'
#' @param vdf A vertical edges data frame.
#' @noRd

fix_arrow_pos <- function(vdf) {
  v <- vdf[ , c("xstart", "ystart", "xend", "yend")]
  reps <- duplicated(v) | duplicated(v, fromLast = TRUE)

  if(length(reps > 0)) {
    tmp <- vdf[reps, ]

    off <- 0.25
    tmp[1, "xstart"] <- tmp[1, "xstart"] - off
    tmp[1, "xend"] <- tmp[1, "xend"] - off
    tmp[1, "xmid"] <- tmp[1, "xmid"] - off
    tmp[2, "xstart"] <- tmp[2, "xstart"] + off
    tmp[2, "xend"] <- tmp[2, "xend"] + off
    tmp[2, "xmid"] <- tmp[2, "xmid"] + off

    out <- rbind(vdf[!reps, ], tmp)
    return(out)
  } else {
    return(vdf)
  }

}
