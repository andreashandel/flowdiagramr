#' Set to and from values to NA if value not present in nodes df
#'
#' @param targdf A data frame.
#' @param nodedf A data frame.
#' @export

set_node_to_na <- function(targdf, nodedf) {
  accepts <- nodedf$id  # all possible node ids
  if(nrow(targdf) > 0) {  # only do this if we have flows (which is almost always)
    for(i in 1:nrow(targdf)) {
      # if the "to" column in the flows data is not in the node data frame, set to NA
      if(!(targdf[i,"to"] %in% accepts)) targdf[i,"to"] <- NA

      # if the "from" column in the flows data is not in the node data frame, set to NA
      if(!(targdf[i,"from"] %in% accepts)) targdf[i,"from"] <- NA
    }
  }
  return(targdf)
}
