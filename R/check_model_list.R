#' @description
#' `check_model_list` makes sure that the necessary elements are included
#' in the model list provided by the user. This is an internal function.
#'
#' @param model_list
#' @return Either an error or nothing.
#' @noRd

check_model_list <- function(model_list) {

  # High level check for list elements
  elements <- names(model_list)
  if(!("varlabels" %in% elements)) {
    msg <- "The model_list object must contain a varlabels element."
    return(list(bad = TRUE, msg = msg))
  }

  if(!("flows" %in% elements)) {
    msg <- "The model_list object must contain a flows element."
    return(list(bad = TRUE, msg = msg))
  }


  # Make sure all variables in flows are in the varlabels
  allflows <- paste(unlist(model_list$flows), collapse = "+")
  vps <- get_vars_pars(allflows)
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
