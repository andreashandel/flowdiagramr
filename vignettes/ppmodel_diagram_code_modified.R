library(ggplot2)
library(flowdiagramr)
library(statebins)

library(ggplot2)
library(flowdiagramr)

model_list <- list(varlabels = c("B", "I"), flows = list(B_flows = c("+g*B*(1-B/bmax)", "-dB*B", "-k*B*I"), I_flows = c("+r*B*I", "-dI*I")))

diagram_list <- prepare_diagram(model_list = model_list)

nodes <- diagram_list$nodes
horizontal_edges <- diagram_list$horizontal_edges
vertical_edges <- diagram_list$vertical_edges
curved_edges <- diagram_list$curved_edges
feedback_edges <- diagram_list$feedback_edges

label_flows <- TRUE
external_flows <- TRUE
interaction_label <- TRUE
node_outline_color <- NA
node_fill_color <- '#6aa4c8'
node_text_color <- 'white'
node_text_size <- 10
flow_text_color <- 'black'
flow_text_size <- 4
main_arrow_color <- 'grey25'
main_arrow_linetype <- 'dotted'
main_arrow_size <- 0.7
interaction_arrow_color <- 'grey25'
interaction_arrow_linetype <- 'dashed'
interaction_arrow_size <- 0.7
with_grid <- FALSE
use_varnames <- FALSE


# Start with an empty ggplot2 canvas. The coord_equal function ensures
# that the x and y coordinates are displayed in equal proportions to
# on another (that is, it makes sure that the squares look like squares).
# All layers are added sequentially onto this blank canvas.
diagram_plot <- ggplot() +
  coord_equal(clip = "off")


# LAYER 1: STATE VARIABLES
# plot the states variable nodes as rectangles

# The nodes data frame is used to create rectangles, with size determined
# by the xmin, xmax, ymin, and ymax values in the nodes data frame. The
# outline color of the rectangles is defined by node_outline_color; the
# inside color (fill) of the rectangles is defined by node_fill_color.
# The color variables can be a single value or a vector, giving different
# colors to different rectangles/nodes/state variables. If a vector, the
# color and fill vectors must have a length that is equal to the number
# of rows in the nodes data frame (one value for each row).
diagram_plot <- diagram_plot +
  statebins:::geom_rrect(
    data = nodes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    color = node_outline_color,
    fill = node_fill_color
  )

# The same nodes data frame contains the label for each rectangle, which
# is either a one letter abbreviation or the full name for the node. The
# size of each label is determined by node_text_size and the colors by
# node_text_color; each can be a single value or a vector the length
# of the number of rows in the nodes data frame (one value for each row).
diagram_plot <- diagram_plot +
  geom_text(
    data = nodes,
    aes(x = labelx, y = labely, label = label),
    size = node_text_size,
    color = node_text_color
  )


# LAYER 2: PHYSICAL FLOWS TO NEIGHBORING STATE VARIABLES
# add the physical flows from one node to another
# these are the flows that go from one node to the next without
# by-passing a node. flows that by-pass nodes are added using the
# curved_edges dataframe. The rows of the data frame are looped over
# so that the "interaction" value can be evaluated to determine the
# linetype to use.
if(nrow(horizontal_edges) > 0) {
  for(i in 1:nrow(horizontal_edges)) {
    dat <- horizontal_edges[i, ]  # get a temporary data frame for this row

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
      geom_segment(
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
        size = this_line_size
      )
  }
}


# The default is label_flows = TRUE, but the user can specify not
# to label flows. So the text above flow arrows is wrapped in an if
# statement. Like the nodes and arrows, colors and sizes  can be specific
# to each label. Thus, the size and color variables must either be of
# length 1 or the length must be the number of rows in the horizontal_edges
# data frame.
if(label_flows == TRUE) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = horizontal_edges,
      aes(x = labelx, y = labely, label = label),
      size = flow_text_size,
      color = flow_text_color
    )
}



# LAYER 3: PHYSICAL FLOWS INTO AND OUT OF THE SYSTEM
# these are the flows that enter or leave single state variables
# as opposed to flows that connect state variables. The user can opt
# not to include these via the external_flows argument, so this
# whole layer is wrapped in an if statement.
if(external_flows == TRUE) {
  diagram_plot <- diagram_plot +
    geom_segment(
      data = vertical_edges,
      aes(x = xstart, y = ystart, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
      color = main_arrow_color,
      arrow.fill = main_arrow_color,
      lineend = "round",
      linejoin = "mitre",
      linetype = main_arrow_linetype,
      size = main_arrow_size
    )

  # Labels are also optional, like above
  if(label_flows == TRUE) {
    diagram_plot <- diagram_plot +
      geom_text(
        data = vertical_edges,
        aes(x = labelx, y = labely, label = label),
        size = flow_text_size,
        color = flow_text_color
      )
  }
}



# LAYER 4: FEEDBACK FLOWS INTO AND OUT OF THE SAME STATE VARIABLE
# these are flows that interact with single state variable and
# represent positive or negative feedbacks. these are represented
# with curves.
diagram_plot <- diagram_plot +
  geom_curve(
    data = feedback_edges,
    ncp = 100,  # number of "points" over which to interpolate the curve
    curvature = -2,  # large curvature to get near circular feedback
    aes(x = xstart, y = ystart, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
    color = main_arrow_color,
    arrow.fill = main_arrow_color,
    lineend = "round",
    linetype = main_arrow_linetype,
    size = main_arrow_size
  )

# As ever, if the label_flows is not true, then no labels need to be printed
if(label_flows == TRUE) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = feedback_edges,
      aes(x = labelx, y = labely, label = label),
      size = flow_text_size,
      color = flow_text_color
    )
}



# LAYER 5: FLOWS THAT BYPASS STATE VARIABLES OR THAT ARE INTERACTIONS
# this layer adds physical flows from one state variable to another
# when another state variable is bypassed. this requires a curved
# connection to avoid crossing over a state variable node. the layer
# also adds interaction arrows that go from a state variable to the
# middle of a physical flow arrow. these are also curved because they go
# from the top/bottom of a node to middle of an arrow. these curves
# are added via the lapply statement so that the "interaction" flag and
# the "curvature" variable can be interpreted and applied correctly.
# the interaction flag in the data frame dictates the linetype; the
# curvature variable in the data frame dictates the curvature. Like
# the horizontal_edges, we loop over the curved_edges data frame rows
# to apply row-specific aesthetics that are difficult to apply via
# ggplot2 mapping in the normal way.
for(i in 1:nrow(curved_edges)) {
  dat <- curved_edges[i, ]  # get a temporary data frame for this row

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
      curvature = dat["curvature"],
      arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
      color = this_line_color,
      arrow.fill = this_arrow_fill,
      lineend = "round",
      size = this_line_size
    )
}

# As ever, if the label_flows is not true, then no labels need to be printed
if(label_flows == TRUE) {
  diagram_plot <- diagram_plot +
    geom_text(
      data = curved_edges,
      aes(x = labelx, y = labely, label = label),
      size = flow_text_size,
      color = flow_text_color)
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


plot(diagram_plot)
