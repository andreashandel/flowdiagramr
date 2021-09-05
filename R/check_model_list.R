#' Check model_list input for prepare_diagram for correctness.
#'
#' @description
#' This function makes sure that the model_list structure
#' contains a properly specified model.
#' This is an internal function.
#'
#' @param model_list the model input structure needed for the \code{\link{prepare_diagram}} function
#' @return Either an error or nothing.
#' @export

check_model_list <- function(model_list) {


  msg <- NULL

  # High level check for list elements
  elements <- names(model_list)
  if(!("varlabels" %in% elements)) {
    msg <- "The model_list object must contain a list element called varlabels."
    return(msg)
  }

  if(!("flows" %in% elements)) {
    msg <- "The model_list object must contain a list element called flows."
    return(msg)
  }

  # Make sure all varlabels are unique
  if(length(unique(model_list$varlabels)) != length(model_list$varlabels)) {
    msg <- "All varlabels need to be unique, you can't use a label more than once."
    return(msg)
  }

  # Check that each varlabels entry has a matched X_flow and the reverse
  # Also ensure they are in the same order
  flownamelabels = gsub("_flows","",names(model_list$flows))
  if (sum(flownamelabels != model_list$varlabels) > 0)
  {
    msg <- "All flow names must follow the varlabel naming and order. Make sure all XYZ in your XYZ_flows names correspond to entries in varlabels in the right order."
    return(msg)
  }

  # Check that naming for varlabels is correct (all start with upper case, no blank)
  # Check that naming for parameters is correct (all start with lower case, no blank)
  # THERE IS ALREADY CODE FOR THIS IN check_model IN modelbuilder.

  # Check that each flow has no more than 2 variables, current limitation
  # of the package until we write a parser.
  flows <- model_list$flows
  #turns flow list into matrix, adding NA, found it online,
  #not sure how exactly it works (from AH and modelbuilder code base)
  #variables are along rows and flows along columns.
  flowmat <- t(sapply(flows, `length<-`, max(lengths(flows))))
  #strip leading +/- from flows and replace with no space
  flowmatred <- sub("\\+|-","",flowmat)
  #convert to character vector
  flowtext <- as.character(flowmatred)
  for(i in 1:length(flowtext)) {
    tmp <- flowtext[i]
    vps <- get_vars_pars(tmp)
    vars_in_flows <- unique(vps[which(vps %in% model_list$varlabels)])
    if(length(vars_in_flows) > 2){
      msg <- paste0("flowdiagramr cannot currently process flows that include\n",
                    "an interaction between more than two variables. Consider\n",
                    "breaking flows apart.")
      return(msg)
    }
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
    return(msg)
  }

  return(NULL)
}
