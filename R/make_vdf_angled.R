#' Helper function to adjust vertical edge start and end positions
#'     to create angles.
#'     helper function for prepare_diagram
#'
#' @param edf The edges data frame.
#' @return A dataframe.
#' @noRd

make_vdf_angled <- function(edf) {
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  vdf <- subset(sdf, abs(diff) >= 9900)

  innies <- which(vdf$to < 9991)
  outies <- which(vdf$to > 9990)
  vdf[innies, "xmin"] <- vdf[innies, "xmin"] - 0.5
  vdf[innies, "ymin"] <- vdf[innies, "ymin"] - 0.5
  # vdf[innies, "ymax"] <- vdf[innies, "ymax"] + 0.5
  vdf[innies, "xlabel"] <- vdf[innies, "xlabel"] - 0.25
  vdf[innies, "ylabel"] <- vdf[innies, "ylabel"] - 0.25
  vdf[outies, "xmax"] <- vdf[outies, "xmax"] + 0.5
  # vdf[outies, "ymin"] <- vdf[outies, "ymin"] - 0.5
  vdf[outies, "ymax"] <- vdf[outies, "ymax"] + 0.5
  vdf[outies, "xlabel"] <- vdf[outies, "xlabel"] - 0.05

  toreplace <- match(paste0(edf$to, edf$from), paste0(vdf$to, vdf$from))
  toreplace <- which(!is.na(toreplace))
  edf[toreplace, ] <- vdf
  return(edf)
}
