#' Helper function to adjust vertical edge start and end positions
#'     to create angles.
#'     helper function for prepare_diagram
#'
#' @param vdf The vertical edges data frame.
#' @return A dataframe.
#' @noRd

make_vdf_angled <- function(vdf) {
  innies <- which(vdf$to < 9991)
  outies <- which(vdf$to > 9990)
  vdf[innies, "xstart"] <- vdf[innies, "xstart"] - 0.5
  vdf[innies, "ystart"] <- vdf[innies, "ystart"] - 1
  vdf[innies, "yend"] <- vdf[innies, "yend"] + 0.5
  vdf[innies, "xmid"] <- vdf[innies, "xmid"] - 0.25
  vdf[innies, "ymid"] <- vdf[innies, "ymid"] - 0.25
  vdf[outies, "xend"] <- vdf[outies, "xend"] + 0.5
  vdf[outies, "ystart"] <- vdf[outies, "ystart"] - 0.5
  vdf[outies, "yend"] <- vdf[outies, "yend"] + 1
  vdf
}
