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

  # unlist the data frames to objects
  nodes <- df_list$nodes
  horizontal_edges <- df_list$horizontal_edges
  vertical_edges <- df_list$vertical_edges
  curved_edges <- df_list$curved_edges
  feedback_edges <- df_list$feedback_edges

  # get the ggplot2 code as text
  code <- get_code()

  # evaluate the ggplot2 code using current environment args
  theplot <- eval(parse(text = code))

  return(theplot)
}
