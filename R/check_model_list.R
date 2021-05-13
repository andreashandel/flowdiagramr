#' @description
#' `check_model_list` makes sure that the necessary elements are included
#' in the model list provided by the user. This is an internal function.
#'
#' @param model_list the model input structure needed for the \code{\link{prepare_diagram}} function
#' @return Either an error or nothing.
#' @noRd

check_model_list <- function(model_list) {

  # High level check for list elements
  elements <- names(model_list)
  if(!("varlabels" %in% elements)) {
    msg <- "The model_list object must contain a list element called varlabels."
    return(list(bad = TRUE, msg = msg))
  }

  if(!("flows" %in% elements)) {
    msg <- "The model_list object must contain a list element called flows."
    return(list(bad = TRUE, msg = msg))
  }

  # Make sure all varlabels are unique
  if(length(unique(model_list$varlabels)) != length(model_list$varlabels)) {
    msg <- "All varlabels need to be unique, you can't use a label more than once."
    return(list(bad = TRUE, msg = msg))
  }

  # Check that there are the same number of varlabels and X_flow entries
  if(length((model_list$varlabels)) != length(model_list$flows)) {
    msg <- "You need one flow entry for each variable, make sure the number of varlabels and X_flow entries are the same."
    return(list(bad = TRUE, msg = msg))
  }

  # Check that each varlabels entry has a matched X_flow and the reverse
  flownamelabels = gsub("_flows","",names(model_list$flows))
  if( length(intersect(flownamelabels, model_list$varlabels)) != length(model_list$varlabels)) {
    msg <- "All flow names must match the varlables. Make sure all XYZ in your XYZ_flows names correspond to entries in varlabels."
    return(list(bad = TRUE, msg = msg))
  }


  # Check that naming for varlabels is correct (all start with upper case, no blank)
  # Check that naming for parameters is correct (all start with lower case, no blank)
  # THERE IS ALREADY CODE FOR THIS IN check_model IN modelbuilder.


  # Make sure all variables in flows are in the varlabels
  allflows <- paste(unlist(model_list$flows), collapse = "+")
  vps <- flowdiagramr:::get_vars_pars(allflows)
  vars_in_flows <- unique(vps[which(vps %in% LETTERS)])
  badids <- which(!(vars_in_flows %in% model_list$varlabels))
  if(length(badids) != 0) {
    missing_vars <- paste(vars_in_flows[badids], collapse = ", ")
    msg <- paste0("The following variables were found in the flows but are not",
                 " in the varlabels vector: ", missing_vars, ".",
                 " Check your model.")
    return(list(bad = TRUE, msg = msg))
  }

  return(list(bad = FALSE))
}
