#' Make a ggplot2 model diagram.
#'
#' @description
#' `make_diagram()` generates a **ggplot2** object based on the data frames
#' made with \code{\link{prepare_diagram}}. The function only applies
#' aesthetics that are not associated with x, y locations. Colors, linetypes,
#' and other graphical options can be set by the user.
#'
#' @param diagram_list A list of data frames returned from the
#'     \code{prepare_diagram} function. See that function for details
#'     about this object.
#' @param diagram_settings A list of diagram aesthetic settings. The
#'     following elements are supported and default values are provided:
#' \itemize{
#' \item `label_flows`: A logical indicating whether to label the flows
#'     (TRUE, default) or not (FALSE).
#' \item `external_flows`: A logical indicating whether to include flows into
#'     and out of the system (external flows). Default is TRUE (include).
#' \item `interaction_label`: A logical indicating whether to make the diagram
#'     with interaction terms (typically curved arrows leading to the
#'     mid point of another arrow) or to simply label the main flow. See
#'     vignettes for examples.
#' \item `with_grid`: A logical indicating whether to return the ggplot
#'     with a grid. Default is FALSE. The grid can be helpful if you
#'     want/need to move items around.
#' \item `node_outline_color`: A character string or vector of character strings
#'     specifying the color of node outlines. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' \item `node_fill_color`: A character string or vector of character strings
#'     specifying the fill color of nodes. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' \item `node_text_color`: A character string or vector of character strings
#'     specifying the text color for node labels. If a vector, the colors will
#'     be recycled in the order of the variables in the supplied data frame.
#' \item `node_text_size`: A numeric scalar specifying the text size for node
#'     labels. Default value is 8.
#' \item `flow_text_color`: A character string or vector of character strings
#'     specifying the text color for flow labels. If a vector, the colors will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `flow_text_size`: A numeric scalar specifying the text size for flow
#'     labels. Default value is 3.
#' \item `main_arrow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the colors will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `main_arrow_linetype`: Either a numeric scalar or a character scalar
#'     specifying the linetype for main arrows (non-interaction arrows). This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid).
#' \item `main_arrow_size`: A numeric scaler specifying the line size for the
#'     main arrows (non-interaction arrows).
#' \item `interaction_arrow_color`: A character string or vector of character
#'     strings specifying the text color for interaction flow arrows.
#'     If a vector, the colors will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `interaction_arrow_linetype`: Either a numeric scalar or a character scalar
#'     specifying the linetype for interaction arrows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 2 (dashed).
#' \item `interaction_arrow_size`: A numeric scalar specifying the line size for
#'     the interaction arrows.
#' }
#'
#' @param use_varnames A logical indicating whether to label nodes with
#'     variable abbreviations (`FALSE`; default) or to use the full names
#'     provided in the `varnames` element of `model_list` (`TRUE`).
#'
#' @return A ggplot2 object.
#' @import ggplot2
#' @export

make_diagram <- function (diagram_list,
                          diagram_settings = list(
                            label_flows = TRUE,
                            external_flows = TRUE,
                            interaction_label = TRUE,
                            node_outline_color = NA,
                            node_fill_color = "#6aa4c8",
                            node_text_color = "white",
                            node_text_size = 10,
                            flow_text_color = "black",
                            flow_text_size = 5,
                            main_arrow_color = "grey25",
                            main_arrow_linetype = "solid",
                            main_arrow_size = 0.7,
                            interaction_arrow_color = "grey25",
                            interaction_arrow_linetype = "dashed",
                            interaction_arrow_size = 0.7),
                          use_varnames = FALSE,
                          with_grid = FALSE
                          ) {
  # TODO error checking

  # assign default settings to be updated by user
  defaults <- eval(formals(make_diagram)$diagram_settings)

  # update defaults with user settings
  defaults[names(diagram_settings)] <- diagram_settings

  # assign settings list to objects
  for(i in 1:length(defaults)) {
    assign(names(defaults)[i], defaults[[i]])
  }

  if(interaction_label == FALSE) {
    # This removes interaction segments and puts the flow label
    # back with the physical flow.
    diagram_list <- move_interaction_label(diagram_list)
  }

  # unlist the data frames to objects
  nodes <- diagram_list$nodes
  horizontal_edges <- diagram_list$horizontal_edges
  vertical_edges <- diagram_list$vertical_edges
  curved_edges <- diagram_list$curved_edges
  feedback_edges <- diagram_list$feedback_edges

  # change the label to full name, if requested
  if(use_varnames) {
    nodes$label <- nodes$name
  }

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
