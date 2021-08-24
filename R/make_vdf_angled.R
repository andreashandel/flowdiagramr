#' Helper function to adjust vertical edge start and end positions
#'     to create angles.
#'     helper function for prepare_diagram
#'
#' @param edf The edges (flows) data frame.
#' @param ndf The nodes (variables) data frame.
#' @param model_settings The model_settings list.
#' @return A dataframe.
#' @noRd

make_vdf_angled <- function(edf, ndf, model_settings) {
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  vdf <- subset(sdf, abs(diff) >= 9900)

  innies <- which(vdf$to < 9991)
  outies <- which(vdf$to > 9990)

  inx <- ndf[which(ndf$id %in% vdf[innies,"to"]), "xmin"]
  iny <- ndf[which(ndf$id %in% vdf[innies,"to"]), "ymax"]
  outx <- ndf[which(ndf$id %in% vdf[outies,"from"]), "xmax"]
  outy <- ndf[which(ndf$id %in% vdf[outies,"from"]), "ymin"]

  inids <- which(ndf$id %in% vdf[innies,"to"])
  outids <- which(ndf$id %in% vdf[outies,"from"])

  vdf[innies, "xmin"] <- inx
  vdf[innies, "ymin"] <- iny + (model_settings$varspace_y_scaling/2)
  vdf[innies, "xlabel"] <- as.numeric(rowMeans(as.matrix(vdf[innies, c("xmin", "xmax")])))
  vdf[innies, "ylabel"] <- as.numeric(rowMeans(as.matrix(vdf[innies, c("ymin", "ymax")])))
  vdf[innies, "ylabel"] <- vdf[innies, "ylabel"] + 0.25

  vdf[outies, "xmax"] <- outx
  vdf[outies, "xmin"] <- outx - (model_settings$varbox_x_scaling/2)
  vdf[outies, "ymin"] <- outy
  vdf[outies, "ymax"] <- outy - (model_settings$varspace_y_scaling/2)
  vdf[outies, "xlabel"] <- as.numeric(rowMeans(as.matrix(vdf[outies, c("xmin", "xmax")])))
  vdf[outies, "ylabel"] <- as.numeric(rowMeans(as.matrix(vdf[outies, c("ymin", "ymax")])))
  vdf[outies, "ylabel"] <- vdf[outies, "ylabel"] - 0.25

  toreplace <- match(paste0(edf$to, edf$from), paste0(vdf$to, vdf$from))
  toreplace <- which(!is.na(toreplace))
  edf[toreplace, ] <- vdf
  return(edf)
}
