#' @description
#' `check_model_settings` makes sure that all optional elements are properly specified.
#'  This is an internal function.
#'
#' @param model_settings the optional input structure for the \code{\link{prepare_diagram}} function
#' @param model_list model list structure, needed to do some computations
#' @param defaults default values for model_settings
#' @return Either an error message or null
#' @noRd

check_model_settings <- function(model_settings, model_list, defaults) {

  msg <- NULL

  ######################################################################
  # check if user supplies a non-recognized argument, if yes, stop
  ######################################################################

  nonrecognized_inputs <- setdiff(names(model_settings),  names(defaults))
  if (length(nonrecognized_inputs>0) )
  {
    msg <- paste0('These elements of model_settings are not recognized: ', nonrecognized_inputs)
    return(msg)
  }

  ######################################################################
  # Check if varlocation matrix is provided
  # Make sure the varlocations matrix entries match those in model_list
  ######################################################################
  varlocation_matrix <- model_settings$varlocations
  if(!is.null(model_settings$varlocations))
  {
    varlocnames = as.vector(model_settings$varlocations)
    varlocnames = varlocnames[varlocnames !=""] #remove empty entries
    if (!setequal(varlocnames, model_list$varlabels))
    {
      # returns fatal error if variables do not match
      msg <- "varlocation entries do not match varlabels in model_list."
      return(msg)
    }
  }

  ######################################################################
  #check the scaling settings
  ######################################################################
  # check to make sure scaling parameters are of length 0 (not there) or 1
  # (same scaling for all boxes)
  # or length of the number of variables. hard error out if not
  nvars = length(model_list$varlabels)

  if( !(length(model_settings$varbox_x_scaling) %in% c(0,1,nvars) ) )
  {
    msg <- "varbox_x_scaling must be of length 1 or length of the number of variables"
    return(msg)
  }
  if( !(length(model_settings$varbox_y_scaling) %in% c(0,1,nvars) ) )
  {
    msg <- "varbox_y_scaling must be of length 1 or length of the number of variables"
    return(msg)
  }
  if( !(length(model_settings$varspace_x_scaling) %in% c(0,1,nvars) ) )
  {
    msg <- "varspace_x_scaling must be of length 1 or length of the number of variables"
    return(msg)
  }
  if( !(length(model_settings$varspace_y_scaling) %in% c(0,1,nvars) ) )
  {
    msg <- "varspace_y_scaling must be of length 1 or length of the number of variables"
    return(msg)
  }

  #returns NULL msg
  return(msg)
}
