library(ggplot2)
library(flowdiagramr)

model_list <- list(varlabels = c("S", "I", "R"), flows = list(S_flows = "-b*S*I", I_flows = c("+b*S*I", "-g*I"), R_flows = "+g*I"), varnames = c("Susceptible", "Infected", "Recovered"), varlocations = structure(c("S", "", "", "I", "R", ""), .Dim = 2:3))

# Since a user-supplied diagram_list is provided,
# the default one created by prepare_diagram() is not used
# diagram_list <- prepare_diagram(model_list = model_list)

# nodes <- diagram_list$nodes
# horizontal_edges <- diagram_list$horizontal_edges
# vertical_edges <- diagram_list$vertical_edges
# curved_edges <- diagram_list$curved_edges
# feedback_edges <- diagram_list$feedback_edges

nodes <- data.frame(
  id = c(1, 2, 3),
  label = c("S", "I", "R"),
  name = c("Susceptible", "Infected", "Recovered"),
  row = c(1, 1, 1),
  x = c(3, 6, 9),
  y = c(-2, -4, -2),
  xmin = c(2.5, 5.5, 8.5),
  xmax = c(3.5, 6.5, 9.5),
  ymin = c(-2.5, -4.5, -2.5),
  ymax = c(-1.5, -3.5, -1.5)
)

horizontal_edges <- data.frame(
  to = c(2, 3),
  from = 1:2,
  label = c("", "g*I"),
  interaction = c(FALSE, FALSE),
  xstart = c(3.5, 6.5),
  ystart = c(-2, -4),
  xend = c(5.5, 8.5),
  yend = c(-4, -2),
  xmid = c(4.5, 7.5),
  ymid = c(-2.75, -2.75)
)

vertical_edges <- data.frame(
  to = numeric(0),
  from = integer(0),
  label = character(0),
  xstart = numeric(0),
  ystart = numeric(0),
  xend = numeric(0),
  yend = numeric(0),
  xmid = numeric(0),
  ymid = numeric(0)
)

curved_edges <- data.frame(
  to = NA_real_,
  from = 2L,
  label = "b*S*I",
  interaction = TRUE,
  xstart = 6,
  ystart = -4.5,
  xend = 4.5,
  yend = -3,
  curvature = -0.7,
  row = 1,
  labelx = 5,
  labely = -4
)

feedback_edges <- data.frame(
  to = numeric(0),
  from = integer(0),
  label = character(0),
  xstart = numeric(0),
  ystart = numeric(0),
  xend = numeric(0),
  yend = numeric(0),
  xmid = numeric(0),
  ymid = numeric(0)
)

label_flows <- TRUE
external_flows <- TRUE
interaction_label <- TRUE
with_grid <- FALSE
node_outline_color <- NA
node_fill_color <- '#6aa4c8'
node_text_color <- 'white'
node_text_size <- 10
flow_text_color <- 'black'
flow_text_size <- 5
main_arrow_color <- 'grey25'
main_arrow_linetype <- 1
main_arrow_size <- 0.7
interaction_arrow_color <- 'grey25'
interaction_arrow_linetype <- 'dashed'
interaction_arrow_size <- 0.7


outplot <- ggplot() +

  # LAYER 1: STATE VARIABLES
  # plot the states variable nodes as rectangles
  geom_rect(data = nodes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = node_outline_color,
            fill = node_fill_color) +
  geom_text(data = nodes,
            aes(x = x, y = y, label = label),
            size = node_text_size,
            color = node_text_color) +

  # LAYER 2: PHYSICAL FLOWS TO NEIGHBORING STATE VARIABLES
  # add the physical flows from one node to another
  # these are the flows that go from one node to the next without
  # by-passing a node. flows that by-pass nodes are added using the
  # curved_edges dataframe.

  lapply(split(horizontal_edges, 1:nrow(horizontal_edges)), function(dat) {
      geom_segment(data = dat, aes(x = xstart,
                                 y = ystart,
                                 xend = xend,
                                 yend = yend),
                 linetype = ifelse(as.numeric(dat["interaction"]),
                                   interaction_arrow_linetype,
                                   main_arrow_linetype),
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 color = ifelse(as.numeric(dat["interaction"]),
                                interaction_arrow_color,
                                main_arrow_color),
                 arrow.fill = ifelse(as.numeric(dat["interaction"]),
                                interaction_arrow_color,
                                main_arrow_color),
                 lineend = "round",
                 size = ifelse(as.numeric(dat["interaction"]),
                               interaction_arrow_size,
                               main_arrow_size)) }
    )  +
  {if(label_flows) {
    geom_text(data = horizontal_edges,
            aes(x = xmid, y = ymid, label = label),
            size = flow_text_size,
            color = flow_text_color)
  }} +

  # LAYER 3: PHYSICAL FLOWS INTO AND OUT OF THE SYSTEM
  # these are the flows that enter or leave single state variables
  # as opposed to flows that connect state variables.
  {if(external_flows){
    geom_segment(data = vertical_edges,
                 aes(x = xstart, y = ystart, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 color = main_arrow_color,
                 arrow.fill = main_arrow_color,
                 lineend = "round",
                 linejoin = "mitre",
                 linetype = main_arrow_linetype,
                 size = main_arrow_size)
  }} +
  {if(label_flows & external_flows) {
    geom_text(data = vertical_edges,
            aes(x = xmid, y = ymid, label = label),
            size = flow_text_size,
            color = flow_text_color)
  }} +

  # LAYER 4: FEEDBACK FLOWS INTO AND OUT OF THE SAME STATE VARIABLE
  # these are flows that interact with single state variable and
  # represent positive or negative feedbacks. these are represented
  # with curves.
  geom_curve(data = feedback_edges,
             ncp = 100,
             curvature = -2,
             aes(x = xstart-0.25, y = ystart+0.5, xend = xend+0.25, yend = yend+0.5),
             arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
             color = main_arrow_color,
             arrow.fill = main_arrow_color,
             lineend = "round",
             linetype = main_arrow_linetype,
             size = main_arrow_size) +
  {if(label_flows) {
    geom_text(data = feedback_edges,
            aes(x = xmid, y = ymid+0.85, label = label),
            size = flow_text_size,
            color = flow_text_color)
  }}

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
# curvature variable in the data frame dictates the curvature.
if(nrow(curved_edges) > 0) {
  outplot <- outplot +
    lapply(split(curved_edges, 1:nrow(curved_edges)), function(dat) {
      geom_curve(data = dat, aes(x = xstart,
                                 y = ystart,
                                 xend = xend,
                                 yend = yend),
                 linetype = ifelse(as.numeric(dat["interaction"]),
                                   interaction_arrow_linetype,
                                   main_arrow_linetype),
                 curvature = dat["curvature"],
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 color = ifelse(as.numeric(dat["interaction"]),
                                interaction_arrow_color,
                                main_arrow_color),
                 arrow.fill = ifelse(as.numeric(dat["interaction"]),
                                interaction_arrow_color,
                                main_arrow_color),
                 lineend = "round",
                 size = ifelse(as.numeric(dat["interaction"]),
                               interaction_arrow_size,
                               main_arrow_size)) }
    ) +
    {if(label_flows) {
      geom_text(data = curved_edges,
              aes(x = labelx, y = labely, label = label),
              size = flow_text_size,
              color = flow_text_color)
    }}

}

# if with_grid == FALSE (default) then void out the theme
# otherwise keep the grey background with grid
# the grid can be useful for updating positions of items
if(!with_grid) {
  outplot <- outplot +
    theme_void()
}

# make the coordinates equal on both sides and avoid clipping
outplot <- outplot +
  coord_equal(clip = "off")

print(outplot)