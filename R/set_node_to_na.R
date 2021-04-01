#' Set to and from values to NA if value not present in nodes df
#'
#' @param targdf A data frame.
#' @param nodedf A data frame.
#' @noRd

set_node_to_na <- function(targdf, nodedf) {
  accepts <- nodedf$id
  if(nrow(targdf) > 0) {
    for(i in 1:nrow(targdf)) {
      if(!(targdf[i,"to"] %in% accepts)) targdf[i,"to"] <- NA
      if(!(targdf[i,"from"] %in% accepts)) targdf[i,"from"] <- NA
    }
  }
  return(targdf)
}
