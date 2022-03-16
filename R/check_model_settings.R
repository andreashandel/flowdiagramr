#' Check optional model_settings input for prepare_diagram for correctness.
#'
#' @description
#' This function checks the model_settings input argument for \code{\link{prepare_diagram}}.
#' This is mainly an internal function, called by \code{\link{prepare_diagram}}.
#'
#' @param model_list model list structure, required input
#' @param model_settings additional settings, optional input
#' @return Either an error message or null
#' @export

check_model_settings <- function(model_list, model_settings) {

  msg <- NULL

  # get default settings
  defaults <- eval(formals(flowdiagramr::prepare_diagram)$model_settings)

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
    if (!setequal(varlocnames, model_list$variables))
    {
      # returns fatal error if variables do not match
      msg <- "varlocation entries do not match variable names in model_list."
      return(msg)
    }
  }

  ######################################################################
  #check the box size settings
  ######################################################################
  # check to make sure box size parameters are one of
  # length 0 (NULL/not there) or
  # 1 (same size for all boxes) or
  # length of the number of variables. hard error out if not
  nvars = length(model_list$variables)

  if( !(length(model_settings$varbox_x_size) %in% c(0,1,nvars) ) )
  {
    msg <- "varbox_x_size must be of length 1 or length of the number of variables"
    return(msg)
  }
  if( !(length(model_settings$varbox_y_size) %in% c(0,1,nvars) ) )
  {
    msg <- "varbox_y_size must be of length 1 or length of the number of variables"
    return(msg)
  }


  ######################################################################
  #check the space size settings
  ######################################################################
  # check to make sure box size parameters are of length 0 (not there) or 1
  # (same size for all boxes)
  # or length-1 of varlocations matrix rows/columns. hard error out if not
  if( !is.null(model_settings$varspace_x_size) && is.null(model_settings$varlocations) )
  {
    msg <- "to use varspace_x_size, you must provide a varlocations matrix."
    return(msg)
  }
  if( !(length(model_settings$varspace_x_size) %in% c(0,1,ncol(model_settings$varlocations)-1) ) )
  {
    msg <- "varspace_x_size must be of length 1 or length of one less than varlocation matrix columns"
    return(msg)
  }

  if( !is.null(model_settings$varspace_y_size) && is.null(model_settings$varlocations) )
  {
    msg <- "to use varspace_y_size, you must provide a varlocations matrix."
    return(msg)
  }
  if( !(length(model_settings$varspace_y_size) %in% c(0,1,nrow(model_settings$varlocations)-1) ) )
  {
    msg <- "varspace_y_size must be of length 1 or length of one less than varlocation matrix rows"
    return(msg)
  }

  #returns NULL msg
  return(msg)
}
