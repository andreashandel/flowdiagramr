#' Write the ggplot2 code.
#'
#' @return A ggplot2 code snippet as a character string.
#' @export

write_diagram_code <- function() {
  code <- 'ggplot() +
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
                   linetype = as.numeric(dat["interaction"]) + 1,
                   curvature = dat["curvature"],
                   arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                   arrow.fill = "black",
                   lineend = "round") }
      ) +
      geom_text(data = curved_edges,
                aes(x = labelx, y = labely, label = label))
  }'

  return(code)
}
