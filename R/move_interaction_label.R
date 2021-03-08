#' Moves interaction label to main flow label.
#'
#' @param dfs The data frames.
#' @return dfs The data frames.
#' @noRd

move_interaction_label <- function(dfs) {
  hdf <- dfs$horizontal_edges
  cdf <- dfs$curved_edges

  ints <- subset(cdf, interaction == TRUE)
  ints$to <- ints$from
  ints$from <- ints$link

  for(i in 1:nrow(ints)) {
    to <- ints[i, "to"]
    from <- ints[i, "from"]
    id <- which(hdf$to == to & hdf$from == from)
    hdf[id, "label"] <- ints[i, "label"]
  }

  cdf <- subset(cdf, interaction == FALSE)

  dfs <- list("nodes" = dfs$nodes,
              "horizontal_edges" = hdf,
              "vertical_edges" = dfs$vertical_edges,
              "curved_edges" = cdf,
              "feedback_edges" = dfs$feedback_edges)
  return(dfs)
}
