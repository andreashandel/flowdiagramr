#' Updates the numeric to/from ids to the variable label
#'
#' @param targ The target data frame, typically the flows df.
#' @param nodes The nodes data frame.
#' @export

update_tofroms <- function(targ, nodes) {
  labs <- nodes[ , c("id", "label")]  # extract the labels and assoc. ids
  if(nrow(targ) > 0) {  # only do this if there are rows (there almost always are)
    for(i in 1:nrow(targ)) {
      tid <- targ[i, "to"]  # the to id for the flow
      fid <- targ[i, "from"]  # the from id for the flow
      tlab <- subset(labs, id == tid)$label  # find the label associated with tid
      flab <- subset(labs, id == fid)$label  # find the label associated with fid

      # Set the labels to NA if they don't show up in the nodes, these
      # are typically flows to dummy compartments without names
      if(length(tlab) == 0) {
        tlab <- NA
      }
      if(length(flab) == 0) {
        flab <- NA
      }

      # update the to/from columns with actual labels
      targ[i, "to"] <- tlab
      targ[i, "from"] <- flab
    }
  }
  return(targ)
}
