#' Create data frames for plotting from model elements.
#'
#' @param input_list A list of model elements. Currently only accepts
#'     a modelbuilder list object. At a minimum, the list must contain
#'     two elements with names \code{vars} and \code{flows}.
#' @return A list of data frames.
#' @export

make_dataframes <- function(input_list) {
  # TODO error checking


  # extract list elements
  vars <- input_list[["vars"]]
  flows <- input_list[["flows"]]


  # create nodes data frame
  nvars <- length(vars)

  if(is.list(vars)) {
    varnames <- unlist(lapply(vars, "[[", 1))
  }

  node_df <- data.frame(
    id = 1:nvars,     # number of nodes
    label = varnames  # labels of nodes
  )

}
