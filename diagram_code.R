## load libraries ----
library(ggplot2)
library(flowdiagramr)


variables <- data.frame(
  id = 1:3,
  name = c("S", "I", "R"),
  xmin = c(0, 2, 4),
  xmax = c(1, 3, 5),
  ymin = c(0, 0, 0),
  ymax = c(1, 1, 1),
  xlabel = c(0.25, 2.25, 4.75),
  ylabel = c(0.75, 0.75, 0.25),
  outline_color = c("black", "black", "black"),
  fill_color = c("#6aa4c8", "#6aa4c8", "#6aa4c8"),
  label_text = c("S", "I", "R"),
  label_color = c("green", "blue", "red"),
  label_size = c(10, 10, 10)
)

flows <- data.frame(
  name = c("e_mI", "e_mR", "e_mS", "e_n", "i_bSI", "m_bSI", "m_gI"),
  type = c("external", "external", "external", "external", "interaction", "main", "main"),
  id = c(5L, 6L, 4L, 3L, 7L, 2L, 1L),
  from = c("I", "R", "S", NA, "I", "S", "I"),
  to = c(NA, NA, NA, "S", NA, "I", "R"),
  xstart = c(2.5, 4.5, 0.5, 0, 2, 1, 3),
  xend = c(3, 5, 1, 0.5, 1.5, 2, 4),
  ystart = c(0, 0, 0, 1.5, 1, 0.5, 0.5),
  yend = c(-0.5, -0.5, -0.5, 1, 0.5, 0.5, 0.5),
  xlabel = c(3.05, 5.05, 1.05, 0.55, 1.625, 1.6, 3.6),
  ylabel = c(-0.25, -0.25, -0.25, 1.25, 1.1, 0.6, 0.6),
  curvature = c(0, 0, 0, 0, 0.5, 0, 0),
  line_color = c("grey25", "grey25", "grey25", "red", "orange", "grey25", "grey25"),
  line_size = c(0.7, 0.7, 0.7, 0.7, 1.5, 0.7, 0.7),
  line_type = c("dotted", "dotted", "dotted", "dotted", "longdash", "solid", "solid"),
  label_text = c("m*I", "m*R", "m*S", "n", "transmission", "", "g*I"),
  label_color = c("black", "black", "black", "black", "black", "black", "black"),
  label_size = c(5, 5, 5, 5, 5, 5, 5),
  show_label = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  arrow_size = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
  show_arrow = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)


 ## ggplot2 code ----
###
# make the diagram with ggplot2
###
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

# create the nodes/boxes/variables
# these are just empty rectangles with no text
for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +  # add new stuff to blank canvas
    geom_rect(
      data = variables[i, ],  # one row of the data frame
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),  # location information
      color = variables[i, "outline_color"],  # border color
      fill = variables[i, "fill_color"]  # internal, fill color
    )
}

# add label text, which goes on top of boxes based on location information
for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +  # add text to boxes
    geom_text(
      data = variables[i, ],
      aes(x = xlabel, y = ylabel, label = label_text),
      size = variables[i, "label_size"],
      color = variables[i, "label_color"]
    )
}

## add in all the flows
# start with the lines/arrows
for(i in 1:nrow(flows)) {
  if(flows[i, "show_arrow"] == TRUE) {
    diagram_plot <- diagram_plot +  # add the lines to the plot with boxes
      geom_curve(  # always use geom_curve, which is straight when cuvature = 1
        data = flows[i, ],
        aes(x = xstart,
            y = ystart,
            xend = xend,
            yend = yend),
        linetype = flows[i, "line_type"],
        arrow = arrow(length = unit(flows[i, "arrow_size"],"cm"), type = "closed"),
        color = flows[i, "line_color"],
        arrow.fill = flows[i, "line_color"],
        lineend = "round",
        size = flows[i, "line_size"],
        curvature = flows[i, "curvature"],
        ncp = 1000  # controls smoothness of curve, larger number = more smooth
      )
  }
}

for(i in 1:nrow(flows)) {
  if(flows[i, "show_label"] == TRUE) {
    diagram_plot <- diagram_plot +  # now add the flow labels to the canvas
      geom_text(
        data = flows[i, ],
        aes(x = xlabel, y = ylabel, label = label_text),
        size = flows[i, "label_size"],
        color = flows[i, "label_color"])
  }
}

# If with_grid == FALSE (default) then void out the theme
# otherwise keep the grey background with grid
# the grid can be useful for updating positions of items
with_grid <- FALSE  # default is false
if(with_grid == FALSE) {
  diagram_plot <- diagram_plot +
    theme_void()  # makes an empty plot theme with no axes, grids, or ticks
} else {
  # The else here may seem silly, but otherwise the returned plot is NULL
  diagram_plot <- diagram_plot  # just returns default ggplot2 theme
}
  


# These lines plot or save the generated diagram. 
# Uncomment them if you want to perform either action. 
# plot(diagram_plot) 
# ggsave('diagram_plot.png',diagram_plot)