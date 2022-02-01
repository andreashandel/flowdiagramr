#' Updates the numeric to/from ids to the variable label
#'
#' @param flows The flows data frame.
#' @param variables The variables data frame.
#' @export

update_tofroms <- function(flows, variables) {
  labs <- variables[ , c("id", "name")]  # extract the names and assoc. ids
  if(nrow(flows) > 0) {  # only do this if there are rows (there almost always are)
    for(i in 1:nrow(flows)) {
      tid <- flows[i, "to"]  # the to id for the flow
      fid <- flows[i, "from"]  # the from id for the flow
      tlab <- subset(labs, id == tid)$name  # find the label associated with tid
      flab <- subset(labs, id == fid)$name  # find the label associated with fid

      # Set the labels to NA if they don't show up in the nodes, these
      # are typically flows to dummy compartments without names
      if(length(tlab) == 0) {
        tlab <- NA
      }
      if(length(flab) == 0) {
        flab <- NA
      }

      # update the to/from columns with actual labels
      flows[i, "to"] <- tlab
      flows[i, "from"] <- flab
    }
  }
  return(flows)
}
