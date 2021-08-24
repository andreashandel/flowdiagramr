#' Returns the ggplot2 code so that this can be in one place
#'
#' @noRd

get_code <- function() {
  code <- '
# Start with an empty ggplot2 canvas. The coord_equal function ensures
# that the x and y coordinates are displayed in equal proportions to
# on another (that is, it makes sure that the squares look like squares).
# All layers are added sequentially onto this blank canvas.
diagram_plot <- ggplot() +
  coord_equal(clip = "off")


# LAYER 1: STATE VARIABLES
# plot the states variable nodes as rectangles

# The variables data frame is used to create rectangles, with size determined
# by the xmin, xmax, ymin, and ymax values in the nodes data frame. The
# outline color of the rectangles is defined by var_outline_color; the
# inside color (fill) of the rectangles is defined by var_fill_color.
# The color variables can be a single value or a vector, giving different
# colors to different rectangles/nodes/state variables. If a vector, the
# color and fill vectors must have a length that is equal to the number
# of rows in the nodes data frame (one value for each row).
for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +
    geom_rect(
      data = variables[i, ],
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      color = variables[i, "color"],
      fill = variables[i, "fill"]
    )
}

for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = variables[i, ],
      aes(x = xlabel, y = ylabel, label = plot_label),
      size = variables[i, "label_size"],
      color = variables[i, "label_color"]
    )
}

for(i in 1:nrow(flows)) {
  diagram_plot <- diagram_plot +
    geom_curve(
      data = flows[i, ],
      aes(x = xmin,
          y = ymin,
          xend = xmax,
          yend = ymax),
      linetype = flows[i, "linetype"],
      arrow = arrow(length = unit(flows[i, "arrowsize"],"cm"), type = "closed"),
      color = flows[i, "color"],
      arrow.fill = flows[i, "color"],
      lineend = "round",
      size = flows[i, "size"],
      curvature = flows[i, "curvature"],
      ncp = 1000
    )
}

for(i in 1:nrow(flows)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = flows[i, ],
      aes(x = xlabel, y = ylabel, label = label),
      size = flows[i, "label_size"],
      color = flows[i, "label_color"])
}

# If with_grid == FALSE (default) then void out the theme
# otherwise keep the grey background with grid
# the grid can be useful for updating positions of items
if(with_grid == FALSE) {
  diagram_plot <- diagram_plot +
    theme_void()
} else {
  # The else here may seem silly, but otherwise the returned plot is NULL
  diagram_plot <- diagram_plot
}
'
  return(code)
}
