## load libraries ----
library(ggplot2)
library(flowdiagramr)


variables <- data.frame(
  id = 1:7,
  name = c("Sc", "Ic", "Rc", "Sa", "Ia", "Ra", "P"),
  xmin = c(0, 6, 9, 0, 6, 9, 3),
  xmax = c(1, 7, 10, 1, 7, 10, 4),
  ymin = c(4, 4, 4, 0, 0, 0, 2),
  ymax = c(5, 5, 5, 1, 1, 1, 3),
  xlabel = c(0.5, 6.5, 9.5, 0.5, 6.5, 9.5, 3.5),
  ylabel = c(4.5, 4.5, 4.5, 0.5, 0.5, 0.5, 2.5),
  outline_color = c("#031e79", "#031e79", "#031e79", "#031e79", "#031e79", "#031e79", "#031e79"),
  fill_color = c("#cde9fa", "#cde9fa", "#cde9fa", "#40f9cf", "#40f9cf", "#40f9cf", "#b2b2b2"),
  label_text = c("Sc", "Ic", "Rc", "Sa", "Ia", "Ra", "P"),
  label_color = c("black", "black", "black", "black", "black", "black", "black"),
  label_size = c(8, 8, 8, 8, 8, 8, 8)
)

flows <- data.frame(
  name = c("e_dP", "i_baaSaIa", "i_bacSaIc", "i_bapSaP", "i_bcaScIa", "i_bccScIc", "i_bcpScP", "i_raIa", "i_rcIc", "m_bacSaIc", "m_bccScIc", "m_gaIa", "m_gcIc"),
  type = c("external", "interaction", "interaction", "interaction", "interaction", "interaction", "interaction", "interaction", "interaction", "main", "main", "main", "main"),
  id = c(5L, 12L, 11L, 13L, 9L, 8L, 10L, 7L, 6L, 4L, 3L, 2L, 1L),
  from = c("P", "Ia", "Ic", "P", "Ia", "Ic", "P", "Ia", "Ic", "Sa", "Sc", "Ia", "Ic"),
  to = c(NA, NA, NA, NA, NA, NA, NA, "P", "P", "Ia", "Ic", "Ra", "Rc"),
  xstart = c(3.5, 6.5, 6.5, 3, 6.5, 6.5, 3, 6, 6, 1, 1, 7, 7),
  xend = c(4, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 4, 4, 6, 6, 9, 9),
  ystart = c(2, 1, 4, 2.5, 1, 5, 2.5, 0.5, 4.5, 0.5, 4.5, 0.5, 4.5),
  yend = c(1.5, 0.5, 0.5, 0.5, 4.5, 4.5, 4.5, 2.5, 2.5, 0.5, 4.5, 0.5, 4.5),
  xlabel = c(3.95, 4.875, 6.925, 1.895, 6.975, 4.875, 2.045, 5.15, 5.1, 3.5, 3.5, 8, 8),
  ylabel = c(1.25, -0.3, 3.75, 1.75, 1.5, 5.7, 3.75, 2, 3.25, 0.6, 4.6, 0.85, 4.85),
  curvature = c(0, -0.5, -0.3, 0.7, 0.3, 0.5, -0.5, 0, 0, 0, 0, 0, 0),
  line_color = c("gray", "#40f9cf", "#cde9fa", "orange", "#40f9cf", "#cde9fa", "orange", "#f94075", "#f94075", "grey25", "grey25", "grey25", "grey25"),
  line_size = c(1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.25),
  line_type = c("dotted", "longdash", "longdash", "longdash", "longdash", "longdash", "longdash", "longdash", "longdash", "solid", "solid", "solid", "solid"),
  label_text = c("d*P", "baa*Sa*Ia", "bac*Sa*Ic", "bap*Sa*P", "bca*Sc*Ia", "bcc*Sc*Ic", "bcp*Sc*P", "ra*Ia", "rc*Ic", "", "", "ga*Ia", "gc*Ic"),
  label_color = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black"),
  label_size = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
  show_label = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  arrow_size = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
  show_arrow = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
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

# ORIGINAL
# for(i in 1:nrow(variables)) {
#   diagram_plot <- diagram_plot +
#     geom_rect(
#       data = variables[i, ],
#       aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#       color = variables[i, "outline_color"],
#       fill = variables[i, "fill_color"]
#     )
# }

# EDITS
for(i in 1:nrow(variables)) {
if(variables$name[i] == "P") {
  diagram_plot <- diagram_plot +
    geom_point(
      data = variables[i, ],
      aes(x = xmin+(xmax-xmin)/2, y = ymin+(ymax-ymin)/2), #centers
      color = variables[i, "outline_color"],
      fill = variables[i, "fill_color"],
      shape = 21,
      size = 23
    )
} else {
  diagram_plot <- diagram_plot +
    geom_rect(
      data = variables[i, ],
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      color = variables[i, "outline_color"],
      fill = variables[i, "fill_color"]
    )
}
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



# These lines plot or save the generated diagram.
# Uncomment them if you want to perform either action.
# plot(diagram_plot)
# ggsave('diagram_plot.png',diagram_plot)
