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
  xlabel = c(0.5, 2.5, 4.5),
  ylabel = c(0.5, 0.5, 0.5),
  outline_color = c("black", "black", "black"),
  fill_color = c("#6aa4c8", "#6aa4c8", "#6aa4c8"),
  label_text = c("S", "I", "R"),
  label_color = c("white", "white", "white"),
  label_size = c(10, 10, 10)
)

flows <- data.frame(
  id = 1:7,
  name = c("m_gI", "m_bSI", "e_n", "e_mS", "e_mI", "e_mR", "i_bSI"),
  type = c("main", "main", "external", "external", "external", "external", "interaction"),
  from = c("I", "S", NA, "S", "I", "R", "I"),
  to = c("R", "I", "S", NA, NA, NA, NA),
  xstart = c(3, 1, 0, 0.5, 2.5, 4.5, 2.5),
  xend = c(4, 2, 0.5, 1, 3, 5, 1.5),
  ystart = c(0.5, 0.5, 1.5, 0, 0, 0, 1),
  yend = c(0.5, 0.5, 1, -0.5, -0.5, -0.5, 0.5),
  xlabel = c(3.5, 1.5, 0.45, 0.95, 2.95, 4.95, 1.875),
  ylabel = c(0.6, 0.6, 1.25, -0.25, -0.25, -0.25, 1.2),
  curvature = c(0, 0, 0, 0, 0, 0, 0.5),
  line_color = c("grey25", "grey25", "grey25", "grey25", "grey25", "grey25", "grey25"),
  line_size = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7),
  line_type = c("solid", "solid", "dotted", "dotted", "dotted", "dotted", "longdash"),
  label_text = c("g*I", "", "n", "m*S", "m*I", "m*R", "b*S*I"),
  label_color = c("black", "black", "black", "black", "black", "black", "black"),
  label_size = c(5, 5, 5, 5, 5, 5, 5),
  show_label = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  arrow_size = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
  show_arrow = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)


 ## ggplot2 code ----
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
      color = variables[i, "outline_color"],
      fill = variables[i, "fill_color"]
    )
}

for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = variables[i, ],
      aes(x = xlabel, y = ylabel, label = label_text),
      size = variables[i, "label_size"],
      color = variables[i, "label_color"]
    )
}

## add in all the flows
for(i in 1:nrow(flows)) {
  if(flows[i, "show_arrow"] == TRUE) {
    diagram_plot <- diagram_plot +
      geom_curve(
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
        ncp = 1000
      )
  }
}

for(i in 1:nrow(flows)) {
  if(flows[i, "show_label"] == TRUE) {
    diagram_plot <- diagram_plot +
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
    theme_void()
} else {
  # The else here may seem silly, but otherwise the returned plot is NULL
  diagram_plot <- diagram_plot
}

#######################
### YOUR NEW ADDITION
#######################
# make a new data frame of text
text_df <- data.frame(  x = 3,  y = -1,
                        lab = "Infected and Infectious"
)
diagram_plot <- diagram_plot +
  geom_text(data = text_df, aes(x = x, y = y, label = lab), size = 8)



# These lines plot or save the generated diagram.
# Uncomment them if you want to perform either action.
# plot(diagram_plot)
# ggsave('diagram_plot.png',diagram_plot)
