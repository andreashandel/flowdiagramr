#' Make a ggplot2 model diagram.
#'
#' @param df_list A list of data frames returned from the
#'     \code{make_dataframes} function.
#' @param with_grid A logical indicating whether to return the ggplot
#'     with a grid. Default is FALSE. The grid can be helpful if you
#'     want/need to move items around.
#' @param node_outline_color A character string or vector of character strings
#'     specifying the color of node outlines. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' @param node_fill_color A character string or vector of character strings
#'     specifying the fill color of nodes. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' @param node_text_color A character string or vector of character strings
#'     specifying the text color for node labels. If a vector, the colors will
#'     be recycled in the order of the variables in the supplied data frame.
#' @param node_text_size A numeric scalar specifying the text size for node
#'     labels. Default value is 8.
#' @param flow_text_color A character string or vector of character strings
#'     specifying the text color for flow labels. If a vector, the colors will
#'     be recycled in the order of the flows in the supplied data frame.
#' @param flow_text_size A numeric scalar specifying the text size for flow
#'     labels. Default value is 3.
#' @param main_arrow_color A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the colors will be recycled in the order of the flows
#'     in the supplied data frame.
#' @return A ggplot2 object.
#' @import ggplot2
#' @export

make_diagram <- function (df_list,
                          with_grid = FALSE,
                          node_outline_color = "black",
                          node_fill_color = "white",
                          node_text_color = "black",
                          node_text_size = 8,
                          flow_text_color = "black",
                          flow_text_size = 3,
                          main_arrow_color = "black",
                          main_arrow_linetype = 1,
                          inter_arrow_color = "black",
                          inter_arrow_linetype = 2) {
  # TODO error checking
  # TODO ggrepel for labels?

  # unlist the data frames to objects
  nodes <- df_list$nodes
  horizontal_edges <- df_list$horizontal_edges
  vertical_edges <- df_list$vertical_edges
  curved_edges <- df_list$curved_edges
  feedback_edges <- df_list$feedback_edges

  # make the plot
  outplot <- ggplot() +

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
                 linejoin = "mitre") +
    geom_text(data = horizontal_edges,
              aes(x = xmid, y = ymid, label = label),
              size = flow_text_size,
              color = flow_text_color) +

    # LAYER 3: PHYSICAL FLOWS INTO AND OUT OF THE SYSTEM
    # these are the flows that enter or leave single state variables
    # as opposed to flows that connect state variables.
    geom_segment(data = vertical_edges,
                 aes(x = xstart, y = ystart, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                 color = main_arrow_color,
                 arrow.fill = main_arrow_color,
                 lineend = "round",
                 linejoin = "mitre") +
    geom_text(data = vertical_edges,
              aes(x = xmid, y = ymid, label = label),
              size = flow_text_size,
              color = flow_text_color) +

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
               lineend = "round") +
    geom_text(data = feedback_edges,
              aes(x = xmid, y = ymid+0.85, label = label),
              size = flow_text_size,
              color = flow_text_color)

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
                   linetype = as.numeric(dat["interaction"]) + 1,
                   curvature = dat["curvature"],
                   arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
                   color = main_arrow_color,
                   arrow.fill = main_arrow_color,
                   lineend = "round") }
      ) +
      geom_text(data = curved_edges,
                aes(x = labelx, y = labely, label = label),
                size = flow_text_size,
                color = flow_text_color)
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

  return(outplot)
}
