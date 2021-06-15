library(ggplot2)
library(flowdiagramr)

model_list <- list(varlabels = c("S", "I", "R"), flows = list(S_flows = "-b*S*I", I_flows = c("b*S*I", "-g*I"), R_flows = "g*I"))

model_settings <- NULL

diagram_list <- prepare_diagram(model_list = model_list, model_settings = model_settings)

variables <- diagram_list$variables
flows <- diagram_list$flows

label_flows <- TRUE
external_flows <- TRUE
interaction_label <- TRUE
var_outline_color <- NA
var_fill_color <- '#6aa4c8'
var_text_color <- 'white'
var_text_size <- NA
flow_text_color <- 'black'
flow_text_size <- NA
main_flow_color <- 'grey25'
main_flow_linetype <- 'solid'
main_flow_size <- 0.7
interaction_flow_color <- 'grey25'
interaction_flow_linetype <- 'dashed'
interaction_flow_size <- 0.7
with_grid <- FALSE

var_outline_color <- flowdiagramr:::recycle_values(var_outline_color, nrow(variables))
var_fill_color <- flowdiagramr:::recycle_values(var_fill_color, nrow(variables))
var_text_color <- flowdiagramr:::recycle_values(var_text_color, nrow(variables))
flow_text_color <- flowdiagramr:::recycle_values(flow_text_color, nrow(flows))


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
      size = var_text_size[i],
      color = var_text_color[i]
    )
}

for(i in 1:nrow(flows)) {
  dat <- flows[i, ]  # get a temporary data frame for this row

  # define the temporary aesthetics for this line based on the
  # interaction
  this_line_type <- ifelse(as.numeric(dat["interaction"]),
                           interaction_flow_linetype,
                           main_flow_linetype)
  this_line_color <- ifelse(as.numeric(dat["interaction"]),
                            interaction_flow_color,
                            main_flow_color)
  this_flow_fill <- ifelse(as.numeric(dat["interaction"]),
                            interaction_flow_color,
                            main_flow_color)
  this_line_size <- ifelse(as.numeric(dat["interaction"]),
                           interaction_flow_size,
                           main_flow_size)

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
      arrow.fill = this_flow_fill,
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


# These lines plot or save the generated diagram. 
# Uncomment them if you want to perform either action. 
# plot(diagram_plot) 
# ggsave('diagram_plot.png',diagram_plot)