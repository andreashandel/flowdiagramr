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
#' @noRd

update_interactions <- function(df) {
  ids <- which(df$interaction == FALSE & df$direct_interaction == TRUE)
  df[ids, "interaction"] <- TRUE
  df$direct_interaction <- NULL  # delete the flagging column
  return(df)
}
