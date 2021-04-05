#' Make a ggplot2 model diagram.
#'
#' @param df_list A list of data frames returned from the
#'     \code{prepare_diagram} function. See that function for details
#'     about this object.
#' @param label_flows A logical indicating whether to label the flows
#'     (TRUE, default) or not (FALSE).
#' @param external_flows A logical indicating whether to include flows into
#'     and out of the system (external flows). Default is TRUE (include).
#' @param interaction_label A logical indicating whether to make the diagram
#'     with interaction terms (typically curved arrows leading to the
#'     mid point of another arrow) or to simply label the main flow. See
#'     vignettes for examples.
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
#' @param main_arrow_linetype Either a numeric scalar or a character scalar
#'     specifying the linetype for main arrows (non-interaction arrows). This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid).
#' @param main_arrow_size A numeric scaler specifying the line size for the
#'     main arrows (non-interaction arrows).
#' @param interaction_arrow_color A character string or vector of character
#'     strings specifying the text color for interaction flow arrows.
#'     If a vector, the colors will be recycled in the order of the flows
#'     in the supplied data frame.
#' @param interaction_arrow_linetype Either a numeric scalar or a character scalar
#'     specifying the linetype for interaction arrows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 2 (dashed).
#' @param interaction_arrow_size A numeric scalar specifying the line size for
#'     the interaction arrows.
#' @return A ggplot2 object.
#' @import ggplot2
#' @export

make_diagram <- function (df_list,
                          label_flows = TRUE,
                          external_flows = TRUE,
                          interaction_label = TRUE,
                          with_grid = FALSE,
                          node_outline_color = "black",
                          node_fill_color = "white",
                          node_text_color = "black",
                          node_text_size = 8,
                          flow_text_color = "black",
                          flow_text_size = 3,
                          main_arrow_color = "black",
                          main_arrow_linetype = "solid",
                          main_arrow_size = 0.5,
                          interaction_arrow_color = "black",
                          interaction_arrow_linetype = "dashed",
                          interaction_arrow_size = 0.5) {
  # TODO error checking

  if(interaction_label == FALSE) {
    df_list <- move_interaction_label(df_list)
  }

  # unlist the data frames to objects
  nodes <- df_list$nodes
  horizontal_edges <- df_list$horizontal_edges
  vertical_edges <- df_list$vertical_edges
  curved_edges <- df_list$curved_edges
  feedback_edges <- df_list$feedback_edges

  # recycle colors as needed
  node_outline_color <- recycle_values(node_outline_color, nrow(nodes))
  node_fill_color <- recycle_values(node_fill_color, nrow(nodes))
  node_text_color <- recycle_values(node_text_color, nrow(nodes))

  # get the ggplot2 code as text
  code <- get_code()

  # evaluate the ggplot2 code using current environment args
  theplot <- eval(parse(text = code))

  return(theplot)
}
