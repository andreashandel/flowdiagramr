#' Returns the ggplot2 code so that this can be in one place
#'
#' @noRd

get_code <- function() {
  code <- 'outplot <- ggplot() +

    # LAYER 1: STATE VARIABLES
    # plot the states variable nodes as tiles
    geom_tile(data = nodes,
              aes(x = x, y = y),
              color = node_outline_color,
              fill = node_fill_color,
              width = 1,
              height = 1) +
    geom_text(data = nodes,
              aes(x = x, y = y, label = label),
              size = node_text_size,
              color = node_text_color) +

    # LAYER 2: PHYSICAL FLOWS TO NEIGHBORING STATE VARIABLES
    # add the physical flows from one node to another
    # these are the flows that go from one node to the next without
    # by-passing a node. flows that by-pass nodes are added using the
    # curved_edges dataframe.
    geom_segment(data = horizontal_edges,
                 aes(x = xstart+0.5, y = ystart, xend = xend-0.5, yend = yend),
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 color = main_arrow_color,
                 arrow.fill = main_arrow_color,
                 lineend = "round",
                 linejoin = "mitre",
                 linetype = main_arrow_linetype,
                 size = main_arrow_size) +
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
    coord_equal(clip = "off")'

  return(code)
}