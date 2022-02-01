#' Helper function to adjust vertical edge start and end positions
#'     to create angles.
#'     helper function for prepare_diagram
#'
#' @param edf The edges (flows) data frame.
#' @param ndf The nodes (variables) data frame.
#' @param model_settings The model_settings list.
#' @return A dataframe.
#' @export

make_vdf_angled <- function(edf, ndf, model_settings) {
  # subset flows down to the inflow and outflow arrows, which are
  # identified by being non-interactions and by having diffs that are
  # less than 1 or greater tha 9000.
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  vdf <- subset(sdf, abs(diff) >= 9900)  # subset further to get just the inflow/outflows

  innies <- which(vdf$to < 9991)  # all inflows will have ids less than 9991
  outies <- which(vdf$to > 9990)  # all outflows will have ids greater than 9990

  inx <- numeric(length(innies))  # vector to hold x locations
  iny <- numeric(length(innies))  # vector to hold y locations
  # loop over the inflow ids and find the upper-left corner locations
  # the upper-left location will define the start of the arrow. then end of
  # the arrow remains the middle-top of the box. in combination, this creates
  # an arrow that is angled point down from the left
  for(i in 1:length(innies)) {
    inx[i] <- ndf[which(ndf$id %in% vdf[innies[i],"to"]), "xmin"] # left
    iny[i] <- ndf[which(ndf$id %in% vdf[innies[i],"to"]), "ymax"] # upper
  }

  outx <- numeric(length(outies)) # vector to hold x locations
  outy <- numeric(length(outies)) # vector to hold y locations
  # loop over the outflow ids and find the lower-right corner locations
  # the lower-right location will define the end of the arrow. then start of
  # the arrow remains the middle-bottom of the box. in combination, this creates
  # an arrow that is angled point down from the left
  for(i in 1:length(outies)) {
    outx[i] <- ndf[which(ndf$id %in% vdf[outies[i],"from"]), "xmax"] # right
    outy[i] <- ndf[which(ndf$id %in% vdf[outies[i],"from"]), "ymin"] # bottom
  }

  # update locations in the data frame
  vdf[innies, "xmin"] <- inx

  ## TODO(andrew): Remove mean() usage after decision on model settings
  vdf[innies, "ymin"] <- iny + (mean(model_settings$varspace_y_size)/2)

  # take means of the x locations for the midpoint for label
  vdf[innies, "xlabel"] <- as.numeric(rowMeans(as.matrix(vdf[innies, c("xmin", "xmax")])))
  # take means of the y locations for midpoint for lable
  vdf[innies, "ylabel"] <- as.numeric(rowMeans(as.matrix(vdf[innies, c("ymin", "ymax")])))
  # add a minor y offset to move the label up a bit, just for visually pleasing effect
  vdf[innies, "ylabel"] <- vdf[innies, "ylabel"] + 0.25

  # update locations in the data frame
  vdf[outies, "xmax"] <- outx

  ## TODO(andrew): Remove mean() usage after decision on model settings
  vdf[outies, "xmin"] <- outx - (mean(model_settings$varbox_x_size)/2)

  vdf[outies, "ymin"] <- outy

  ## TODO(andrew): Remove mean() usage after decision on model settings
  vdf[outies, "ymax"] <- outy - (mean(model_settings$varspace_y_size)/2)

  # take means of the x locations for the midpoint for label
  vdf[outies, "xlabel"] <- as.numeric(rowMeans(as.matrix(vdf[outies, c("xmin", "xmax")])))
  # take means of the y locations for midpoint for lable
  vdf[outies, "ylabel"] <- as.numeric(rowMeans(as.matrix(vdf[outies, c("ymin", "ymax")])))
  # add a minor y offset to move the label up a bit, just for visually pleasing effect
  vdf[outies, "ylabel"] <- vdf[outies, "ylabel"] - 0.25

  # just replace the rows in the flows (edf) dataframe that have been changed
  toreplace <- match(paste0(edf$to, edf$from), paste0(vdf$to, vdf$from))
  toreplace <- which(!is.na(toreplace))
  edf[toreplace, ] <- vdf
  return(edf)
}
