#' Moves interaction label to main flow label.
#'
#' @param dfs The data frames.
#' @return dfs The data frames.
#' @noRd

move_interaction_label <- function(flows) {

  ints <- subset(flows, interaction == TRUE)
  ints$to <- ints$from
  ints$from <- ints$link

  for(i in 1:nrow(ints)) {
    to <- ints[i, "to"]
    from <- ints[i, "from"]
    id <- which(flows$to == to & flows$from == from)
    flows[id, "label"] <- ints[i, "label"]
  }

  flows <- subset(flows, interaction == FALSE)
  return(flows)
}
