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
#' @param with_grid A logical indicating whether to return the ggplot
#'     with a grid. Default is FALSE. The grid can be helpful if you
#'     want/need to move items around.
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
#' diagram_with_grid <- make_diagram(diagram_list, with_grid = TRUE)
#'
#' @import ggplot2
#' @export
#'

make_diagram <- function (diagram_list, with_grid = FALSE) {

  # check input data frames for conformity
  check_dataframes(diagram_list)

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
