#' Returns the ggplot2 code so that this can be in one place
#'
#' @export

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
      color = ds$var_outline_color[i],
      fill = ds$var_fill_color[i]
      #color = variables[i, "var_outline_color"],
      #fill = variables[i, "var_fill_color"]
    )
}

for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = variables[i, ],
      aes(x = xlabel, y = ylabel, label = label),
      size = ds$var_label_size[i],
      color = ds$var_label_color[i]
      #size = variables[i, "var_label_size"],
      #color = variables[i, "var_label_color"]
    )
}

# process all main flows

mainflows <- flows[flows$type=="main",]

for(i in 1:nrow(mainflows)) {
  diagram_plot <- diagram_plot +
    geom_curve(
      data = mainflows[i, ],
      aes(x = xmin,
          y = ymin,
          xend = xmax,
          yend = ymax),
      linetype = ds$main_flow_linetype[i],
      arrow = arrow(length = unit(ds$main_flow_arrow_size[i],"cm"), type = "closed"),
      color = ds$main_flow_color[i],
      arrow.fill = ds$main_flow_color[i],
      lineend = "round",
      size = ds$main_flow_size[i],
      curvature = mainflows[i, "curvature"],
      ncp = 1000
    )
}

for(i in 1:nrow(flows)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = mainflows[i, ],
      aes(x = xlabel, y = ylabel, label = label),
      size = ds$main_flow_label_size[i],
      color = ds$main_flow_label_color[i])
}

# repeat for interaction flows


interactionflows <- flows[flows$type=="interaction",]

for(i in 1:nrow(interactionflows)) {
  diagram_plot <- diagram_plot +
    geom_curve(
      data = interactionflows[i, ],
      aes(x = xmin,
          y = ymin,
          xend = xmax,
          yend = ymax),
      linetype = ds$interaction_flow_linetype[i],
      arrow = arrow(length = unit(ds$interaction_flow_arrow_size[i],"cm"), type = "closed"),
      color = ds$interaction_flow_color[i],
      arrow.fill = ds$interaction_flow_color[i],
      lineend = "round",
      size = ds$interaction_flow_size[i],
      curvature = interactionflows[i, "curvature"],
      ncp = 1000
    )
}

for(i in 1:nrow(flows)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = interactionflows[i, ],
      aes(x = xlabel, y = ylabel, label = label),
      size = ds$interaction_flow_label_size[i],
      color = ds$interaction_flow_label_color[i])
}



# repeat for external flows

externalflows <- flows[flows$type=="external",]

for(i in 1:nrow(externalflows)) {
  diagram_plot <- diagram_plot +
    geom_curve(
      data = externalflows[i, ],
      aes(x = xmin,
          y = ymin,
          xend = xmax,
          yend = ymax),
      linetype = ds$external_flow_linetype[i],
      arrow = arrow(length = unit(ds$external_flow_arrow_size[i],"cm"), type = "closed"),
      color = ds$external_flow_color[i],
      arrow.fill = ds$external_flow_color[i],
      lineend = "round",
      size = ds$external_flow_size[i],
      curvature = externalflows[i, "curvature"],
      ncp = 1000
    )
}

for(i in 1:nrow(flows)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = externalflows[i, ],
      aes(x = xlabel, y = ylabel, label = label),
      size = ds$external_flow_label_size[i],
      color = ds$external_flow_label_color[i])
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
