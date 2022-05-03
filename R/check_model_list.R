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

  # to reduce typing
  # also helps if someone like AH tries to rename everything again in the future :)
  variables <- model_list$variables
  flows <- model_list$flows


  ################################
  # Check things for variables
  ################################

  # High level check for list elements
  elements <- names(model_list)
  if(!("variables" %in% elements))
  {
    msg <- "The model_list object must contain a list element called variables."
    return(msg)
  }

  # Make sure all variables are unique
  if(length(unique(variables)) != length(variables))
  {
    msg <- "All variable names need to be unique."
    return(msg)
  }

  #check that naming for variables is correct (all start with upper case, no blank)
  varpattern = "^[A-Z]+[A-Za-z0-9_]*$"
  if (sum(!grepl(varpattern,substr(variables, 1, 1)))>0)
  {
    msg = "Please start with a upper case letter and use only use letters and numbers for variables"
    return(msg)
  }


  ################################
  # Check things for flows
  ################################


  if(!("flows" %in% elements))
  {
    msg <- "The model_list object must contain a list element called flows."
    return(msg)
  }

  # Check that each variables entry has a matched X_flow and the reverse
  # Also ensure they are in the same order
  flownamelabels = gsub("_flows","",names(flows))
  if (sum(flownamelabels != variables) > 0)
  {
    msg <- "All flow names must follow the variables naming and order. Make sure all XYZ in your XYZ_flows names correspond to entries in variables in the right order."
    return(msg)
  }

  # more detailed flow checking to try and catch anything that doesn't make sense
  # or that contains unsupported syntax

  #loop over each variable
  #then for each variable loop over all flows and check them
  #could likely be written more efficient, but speed doesn't matter here
  #and a flow at a time is easy to follow along
  for (nv in 1:length(variables))
  {
    varflows = flows[[nv]]
    for (nf in 1:length(varflows))
    {
      nowflow = varflows[nf]

      # uses small helper function to get names of individal parameters
      # and variables for a flow
      flowsymbols = get_vars_pars(nowflow)

      #look at any symbols that are not variables, those should be parameter names
      #nothing else (e.g. sin() or such) is currently allowed
      parnames = setdiff(flowsymbols, variables)
      # check that parameter names follow the right naming convention
      parpattern = "^[a-z0-9]+[A-Za-z0-9_]*$"
      if (sum(!grepl(parpattern,parnames))>0)
      {
        msg = "Please start with a lower case letter and use only use letters and numbers for parameters";
        return(msg)
      }
      # these are allowed, nothing else
      math_symbols <- c("+", "-", "*", "^", "/", "(", ")", " ","")
      allsymbols = c(math_symbols,variables, parnames, 0:9)
      if (sum(!(flowsymbols %in% allsymbols)) >0)
      {
        wrongflows <- flowsymbols[!(flowsymbols %in% allsymbols)]
        msg = paste0("Your flows for variable ",variables[nv], " contain these non-allowed symbols: ", paste0(wrongflows, collapse = ", "));
        return(msg)
      }

      # this checks if a flow has more than 3 variables - can't do that right now, but should implement
      # note that this doesn't catch something like s*I^2
      # figuring out that I^2 is really I*I seems hard (is it necessary or is it really 3 distinct variables?)
      varnames = intersect(flowsymbols, variables)
      if (length(varnames)>2)
      {
        msg <- "flowdiagramr cannot currently process flows that include an interaction between more than two variables.\n Consider breaking flows apart."
        return(msg)
      }
    } #end loop over flows
  } #end loop over variables

  # if not problem occured above, return NULL as an indication that all is ok
  return(NULL)
}
