library(ggplot2)
library(flowdiagramr)

model_list <- list(varlabels = c("B", "I"), flows = list(B_flows = c("+g*B*(1-B/bmax)", "-dB*B", "-k*B*I"), I_flows = c("+r*B*I", "-dI*I")))

model_settings <- list(varnames = NULL, use_varnames = FALSE, var_label_size = 10, varlocations = NULL)

diagram_list <- prepare_diagram(model_list = model_list, model_settings = model_settings)

variables <- diagram_list$variables
flows <- diagram_list$flows

# setup linetypes mapping from numeric to text
ltys <- data.frame(code = 0:6,
                   text = c("blank", "solid", "dashed",
                            "dotted", "dotdash", "longdash",
                            "twodash"))

var_outline_color <- 'red'
var_fill_color <- c('#6aa4c8', 'green')
var_label_on <- TRUE
var_label_color <- 'red'
var_label_size <- 10
main_flow_on <- TRUE
main_flow_color <- 'green'
main_flow_linetype <- 3
main_flow_size <- 0.7
main_flow_label_on <- TRUE
main_flow_label_color <- 'black'
main_flow_label_size <- 4
interaction_flow_on <- TRUE
interaction_flow_color <- 'grey25'
interaction_flow_linetype <- 'dashed'
interaction_flow_size <- 2
interaction_flow_label_on <- TRUE
interaction_flow_label_color <- 'black'
interaction_flow_label_size <- 5
external_flow_on <- TRUE
external_flow_color <- 'grey25'
external_flow_linetype <- 'solid'
external_flow_size <- 0.7
external_flow_label_on <- TRUE
external_flow_label_color <- 'black'
external_flow_label_size <- 5
with_grid <- FALSE

# recycle values as needed
variables$color <- flowdiagramr:::recycle_values(var_outline_color, nrow(variables))
variables$fill <- flowdiagramr:::recycle_values(var_fill_color, nrow(variables))
variables$label_color <- flowdiagramr:::recycle_values(var_label_color, nrow(variables))
variables$label_size <- flowdiagramr:::recycle_values(var_label_size, nrow(variables))
variables$plot_label_size <- NULL

mains <- subset(flows, type == "main")
mains$color <- flowdiagramr:::recycle_values(main_flow_color, nrow(mains))
if(is.numeric(main_flow_linetype)) {
  main_flow_linetype <- subset(ltys, code == main_flow_linetype)[,"text"]
}
mains$linetype <- flowdiagramr:::recycle_values(main_flow_linetype, nrow(mains))
mains$size <- flowdiagramr:::recycle_values(main_flow_size, nrow(mains))
mains$label_color <- flowdiagramr:::recycle_values(main_flow_label_color, nrow(mains))
mains$label_size <- flowdiagramr:::recycle_values(main_flow_label_size, nrow(mains))

ints <- subset(flows, type == "interaction")
ints$color <- flowdiagramr:::recycle_values(interaction_flow_color, nrow(ints))
if(is.numeric(interaction_flow_linetype)) {
  interaction_flow_linetype <- subset(ltys, code == interaction_flow_linetype)[,"text"]
}
ints$linetype <- flowdiagramr:::recycle_values(interaction_flow_linetype, nrow(ints))
ints$size <- flowdiagramr:::recycle_values(interaction_flow_size, nrow(ints))
ints$label_color <- flowdiagramr:::recycle_values(interaction_flow_label_color, nrow(ints))
ints$label_size <- flowdiagramr:::recycle_values(interaction_flow_label_size, nrow(ints))

exts <- subset(flows, type == "external")
exts$color <- flowdiagramr:::recycle_values(external_flow_color, nrow(exts))
if(is.numeric(external_flow_linetype)){
  external_flow_linetype <- subset(ltys, code == external_flow_linetype)[,"text"]
}
exts$linetype <- flowdiagramr:::recycle_values(external_flow_linetype, nrow(exts))
exts$size <- flowdiagramr:::recycle_values(external_flow_size, nrow(exts))
exts$label_color <- flowdiagramr:::recycle_values(external_flow_label_color, nrow(exts))
exts$label_size <- flowdiagramr:::recycle_values(external_flow_label_size, nrow(exts))

# recombine flows data frame with aesthetics as columns
 flows <- rbind(mains, ints, exts)
flows$arrowsize <- 0.25  # default arrow size


# turn off flows completely by setting linetype to blank as needed
if(main_flow_on == FALSE) {
  flows[flows$type == "main", "linetype"] <- "blank"
  flows[flows$type == "main", "arrowsize"] <- 0
}
if(interaction_flow_on == FALSE) {
  flows[flows$type == "interaction", "linetype"] <- "blank"
  flows[flows$type == "interaction", "arrowsize"] <- 0
}
if(external_flow_on == FALSE) {
 flows[flows$type == "external", "linetype"] <- "blank"
 flows[flows$type == "external", "arrowsize"] <- 0
}


# set label to "" to suppress label if requested
# also do not show label if the flow itself is turned off
flows$math <- flows$label
if(main_flow_on == FALSE || main_flow_label_on == FALSE) {
  flows[flows$type == "main", "label"] <- ""
}
if(interaction_flow_on == FALSE || interaction_flow_label_on == FALSE) {
  flows[flows$type == "interaction", "label"] <- ""
}
if(external_flow_on == FALSE || external_flow_label_on == FALSE) {
  flows[flows$type == "external", "label"] <- ""
}


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
      aes(x = labelx, y = labely, label = plot_label),
      size = variables[i, "label_size"],
      color = variables[i, "label_color"]
    )
}

for(i in 1:nrow(flows)) {
  diagram_plot <- diagram_plot +
    geom_curve(
      data = flows[i, ],
      aes(x = xstart,
          y = ystart,
          xend = xend,
          yend = yend),
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
      aes(x = labelx, y = labely, label = label),
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


# These lines plot or save the generated diagram. 
# Uncomment them if you want to perform either action. 
# plot(diagram_plot) 
# ggsave('diagram_plot.png',diagram_plot)