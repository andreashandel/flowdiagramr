#' Make a ggplot2 model diagram.
#'
#' @param df_list A list of data frames returned from the
#'     \code{make_dataframes} function.
#' @return A ggplot2 object.
#' @import ggplot2
#' @export

make_diagram <- function (df_list) {
  # TODO error checking
  # TODO ggrepel for labels?

  # unlist the data frames to objects
  nodes <- df_list$nodes
  horizontal_edges <- df_list$horizontal_edges
  vertical_edges <- df_list$vertical_edges
  curved_edges <- df_list$curved_edges
  feedback_edges <- df_list$feedback_edges

  # make the plot
  outplot <- ggplot() +
    geom_tile(data = nodes,
              aes(x = x, y = y),
              color = "black",
              fill = "white",
              width = 1,
              height = 1) +
    geom_text(data = nodes,
              aes(x = x, y = y, label = label),
              size = 8) +
    geom_segment(data = horizontal_edges,
                 aes(x = xstart+0.5, y = ystart, xend = xend-0.5, yend = yend),
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 arrow.fill = "black",
                 lineend = "round",
                 linejoin = "mitre") +
    geom_text(data = horizontal_edges,
              aes(x = xmid, y = ymid, label = label)) +
    geom_segment(data = vertical_edges,
                 aes(x = xstart, y = ystart-0.5, xend = xend, yend = yend+0.5),
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 arrow.fill = "black",
                 lineend = "round",
                 linejoin = "mitre") +
    geom_text(data = vertical_edges,
              aes(x = xmid+0.25, y = ymid, label = label)) +
    geom_curve(data = feedback_edges,
               ncp = 100,
               curvature = -2,
               aes(x = xstart-0.25, y = ystart+0.5, xend = xend+0.25, yend = yend+0.5),
               arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
               arrow.fill = "black",
               lineend = "round") +
    geom_text(data = feedback_edges,
              aes(x = xmid, y = ymid+0.85, label = label)) +
    coord_equal(clip = "off") +
    theme_void()

  if(nrow(curved_edges) > 0) {
    outplot <- outplot +
      lapply(split(curved_edges, 1:nrow(curved_edges)), function(dat) {
        geom_curve(data = dat, aes(x = xstart,
                                   y = ystart,
                                   xend = xend,
                                   yend = yend),
                   curvature = dat["curvature"],
                   arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                   arrow.fill = "black",
                   lineend = "round") }
      ) +
      geom_text(data = curved_edges,
                aes(x = labelx, y = labely, label = label))
  }

  return(outplot)
}
