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
      color = var_outline_color[i],
      fill = var_fill_color[i]
    )
}

for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = variables[i, ],
      aes(x = labelx, y = labely, label = plot_label),
      size = variables[i, "plot_label_size"],
      color = var_text_color[i]
    )
}

for(i in 1:nrow(flows)) {
  dat <- flows[i, ]  # get a temporary data frame for this row

  # define the temporary aesthetics for this line based on the
  # interaction
  this_line_type <- ifelse(as.numeric(dat["interaction"]),
                           interaction_arrow_linetype,
                           main_arrow_linetype)
  this_line_color <- ifelse(as.numeric(dat["interaction"]),
                            interaction_arrow_color,
                            main_arrow_color)
  this_arrow_fill <- ifelse(as.numeric(dat["interaction"]),
                            interaction_arrow_color,
                            main_arrow_color)
  this_line_size <- ifelse(as.numeric(dat["interaction"]),
                           interaction_arrow_size,
                           main_arrow_size)

  diagram_plot <- diagram_plot +
    geom_curve(
      data = dat,
      aes(x = xstart,
          y = ystart,
          xend = xend,
          yend = yend),
      linetype = this_line_type,
      arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
      color = this_line_color,
      arrow.fill = this_arrow_fill,
      lineend = "round",
      size = this_line_size,
      curvature = dat["curvature"],
      ncp = 1000
    )
}

if(label_flows == TRUE) {
  for(i in 1:nrow(flows)) {
    dat <- flows[i, ]

    diagram_plot <- diagram_plot +
      geom_text(
        data = dat,
        aes(x = labelx, y = labely, label = label),
        size = flow_text_size[i],
        color = flow_text_color[i])
  }
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
