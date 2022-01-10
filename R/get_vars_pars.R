#' Extract the variables and parameters from a flow. used by prepare_diagram
#'
#' This function takes as input a single flow expression
#' and extracts the variables and parameters (excludes the
#' math notation).
#'
#' @description The flow must be a character string as used in flowdiagramr
#' and modelbuilder.
#' @param flow A flow, which is a character string.
#' @return A character vector of the variables and parameters, in order.
#' @author Andrew Tredennick and Andreas Handel
#' @export

get_vars_pars <- function(flow) {
  # take the flow apart by splitting on any math symbols that connect
  # what should be left over is a collection of variable and parameter names
  # we can currently only deal with basic math operations
  mathpattern <- "[-+\\++\\*+\\(+\\)+\\^+/+]"
  flowsymbols <- unlist(strsplit(flow, mathpattern))
  #there might be some empty entries, remove those
  flowsymbols = flowsymbols[nchar(flowsymbols)>0]
  #alternative version to above, not sure which is better
  #to_rm <- which(flowsymbols == "")
  #if(length(to_rm) != 0) {
  #  flowsymbols <- flowsymbols[-to_rm]
  #}

  return(flowsymbols)
}


