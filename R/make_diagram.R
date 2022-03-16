#' Make a ggplot2 model diagram.
#'
#' @description
#' `make_diagram()` generates a **ggplot2** object based on the data frames
#' made with \code{\link{prepare_diagram}}. The function only applies
#' aesthetics that are not associated with x, y locations. Colors, linetypes,
#' and other graphical options can be set by the user.
#'
#' @param diagram_list A required list of data frames returned from the
#'     \code{prepare_diagram} function. See that function for details
#'     about this object.
#' @param diagram_settings An optional list of diagram aesthetic settings. The
#'     following elements are supported and default values are provided:
#' \itemize{
#' \item `var_outline_color`: A character string or vector of character strings
#'     specifying the color of variable outlines. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' \item `var_fill_color`: A character string or vector of character strings
#'     specifying the fill color of variables. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' \item `var_label_on`: A logical indicating if the labels for the variables
#'     should be plotted.
#' \item `var_label_color`: A character string or vector of character strings
#'     specifying the text color for variable labels. If a vector, the colors will
#'     be recycled in the order of the variables in the supplied data frame.
#' \item `var_label_text`: A character vector the same length as the number
#'     of variables in the `variables` data frame. If provided, these values
#'     update the variable labels provided to \code{\link{prepare_diagram}}.
#' \item `var_label_size`: A numeric scalar specifying the text size for variable
#'     labels. Note that any value supplied here overwrites
#'     entries in the list structure returned by \code{\link{prepare_diagram}}.
#'     Specifically, if you set this parameter when calling \code{\link{prepare_diagram}}
#'     with \code{use_varnames = TRUE}, the value is used to compute box size,
#'     but then the actual size of the label as provided here is applied.
#'
#' \item `main_flow_on`: A logical indicating if the main flow arrows should be plotted.
#' \item `main_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the values will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `main_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for main flows (non-interaction flows). This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_size`: A numeric scalar or vector specifying the line size for the
#'     main flows (non-interaction flows). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_label_on`: A logical indicating if the labels for the main
#'     flows should be plotted.
#' \item `main_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for main flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for main flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_arrow_size`: A scalar or numeric vector specifying size of arrow
#'
#' \item `interaction_flow_on`: A logical indicating if the interaction flow arrows should be plotted.
#' \item `interaction_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the values will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `interaction_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for interaction flows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_size`: A numeric scalar or vector specifying the line size for the
#'     interaction flows (non-interaction flows). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_label_on`: A logical indicating if the labels for the interaction
#'     flows should be plotted.
#' \item `interaction_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for interaction flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for interaction flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_arrow_size`: A scalar or numeric vector specifying size of arrow
#'
#' \item `external_flow_on`: A logical indicating if the external flow arrows should be plotted.
#' \item `external_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the values will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `external_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for external flows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_size`: A numeric scalar or vector specifying the line size for the
#'     external flows (non-interaction flows). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_label_on`: A logical indicating if the labels for the external
#'     flows should be plotted.
#' \item `external_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for external flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for external flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_arrow_size`: A scalar or numeric vector specifying size of arrow
#`
#' \item `with_grid` A logical indicating whether to return the ggplot
#'     with a grid. Default is FALSE. The grid can be helpful if you
#'     want/need to move items around.
#' }
#'
#' @return A ggplot2 object.
#' @examples
#' mymodel = list(variables = c("S","I","R"),
#'                flows = list(S_flows = c("-b*S*I"),
#'                             I_flows = c("b*S*I","-g*I"),
#'                             R_flows = c("g*I") ) )
#' diagram_list <- prepare_diagram(model_list = mymodel)
#'
#' # make diagram without grid
#' diagram <- make_diagram(diagram_list)
#'
#' # make diagram with grid
#' diagram_with_grid <- make_diagram(diagram_list, diagram_settings = list(with_grid = TRUE))
#' @import ggplot2
#' @export
#'

make_diagram <- function (diagram_list, with_grid = FALSE) {

  # unlist the data frames to objects
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  # assign the plot_label column, can be updated below if specified
  # variables$plot_label <- variables$label

  # # determine number of variables and each flow
  # nvars = nrow(variables)
  # nmain = sum(flows$type == "main")
  # ninteraction = sum(flows$type == "interaction")
  # nexternal = sum(flows$type == "external")

  # get the ggplot2 code as text
  code <- get_code()

  # evaluate the ggplot2 code using current environment args
  theplot <- eval(parse(text = code))

  return(theplot)
}
