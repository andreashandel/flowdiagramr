#' Extract the variables and parameters from a flow. used by prepare_diagram
#'
#' This function takes as input a modelbuilder flow and
#' extracts the variables and parameters (excludes the
#' math notation).
#'
#' @description The flow must be a character string of typical modelbuilder
#'     model structure.
#' @param flow A modelbuilder flow, which is a character string.
#' @return A character vector of the variables and parameters, in order.
#' @author Andrew Tredennick and Andreas Handel
#' @export

get_vars_pars <- function(flow) {
  #extract just the variables and parameters, in order, from the flows
  #by splitting the string based upon math symbols
  mathpattern <- "[-+\\++\\*+\\(+\\)+\\^+/+]"
  flowsymbols <- unlist(strsplit(flow, mathpattern))
  to_rm <- which(flowsymbols == "")
  if(length(to_rm) != 0) {
    flowsymbols <- flowsymbols[-to_rm]
  }

  return(flowsymbols)
}
