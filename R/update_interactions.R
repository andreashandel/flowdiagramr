#' Updates the interaction column for plotting.
#'
#' @description
#' `update_interactions()` sets the interaction column of an edge
#' data frame to TRUE if the `direct_interaction` columns is true. It
#' then removes the `direct_interaction` column because it is no longer
#' needed.
#'
#' @param df An edge data frame.
#'
#' @return The edge data frame.
#'
#' @export

update_interactions <- function(df) {
  # get row ids for the "direct interactions"
  ids <- which(df$interaction == FALSE & df$direct_interaction == TRUE)
  # set interaction to TRUE since this now is just for plotting aesthetics
  df[ids, "interaction"] <- TRUE
  # remove the direct_interaction column because all processing is complete
  df$direct_interaction <- NULL  # delete the flagging column
  return(df)
}
