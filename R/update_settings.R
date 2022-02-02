#' Updates the aesthetics settings in the variables and flows data frames
#'
#' @param diagram_list A required list of data frames returned from the
#'     \code{prepare_diagram} function. See that function for details
#'     about this object.
#' @param diagram_settings A required list of diagram aesthetic settings. The
#'     following elements are supported and default values are provided:
#' \itemize{
#' \item `var_outline_color`: A character string or vector of character strings
#'     specifying the color of variable outlines. Must either be of length 1
#'     or the number of rows in the variables data frame.
#' \item `var_fill_color`: A character string or vector of character strings
#'     specifying the fill color of variables. Must either be of length 1
#'     or the number of rows in the variables data frame.
#' \item `var_label_color`: A character string or vector of character strings
#'     specifying the text color for variable labels. Must either be of length 1
#'     or the number of rows in the variables data frame.
#' \item `var_label_size`: A numeric scalar specifying the text size for variable
#'     labels. Must either be of length 1 or the number of rows in the
#'     variables data frame.
#'
#' \item `main_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows. Must either
#'     be of length 1 or the number of main flows.
#' \item `main_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for main flows (non-interaction flows). This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). Must either
#'     be of length 1 or the number of main flows.
#' \item `main_flow_size`: A numeric scalar or vector specifying the line size for the
#'     main flows (non-interaction flows). Must either
#'     be of length 1 or the number of main flows.
#' \item `main_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for main flow labels. Must either
#'     be of length 1 or the number of main flows.
#' \item `main_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for main flow labels. Must either
#'     be of length 1 or the number of main flows.
#'
#' \item `interaction_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows. Must either
#'     be of length 1 or the number of interaction flows.
#' \item `interaction_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for interaction flows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). Must either
#'     be of length 1 or the number of interaction flows.
#' \item `interaction_flow_size`: A numeric scalar or vector specifying the line size for the
#'     interaction flows (non-interaction flows). Must either
#'     be of length 1 or the number of interaction flows.
#' \item `interaction_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for interaction flow labels. Must either
#'     be of length 1 or the number of interaction flows.
#' \item `interaction_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for interaction flow labels. Must either
#'     be of length 1 or the number of interaction flows.
#'
#' \item `external_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     Must either be of length 1 or the number of external flows.
#' \item `external_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for external flows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). Must either be of length 1 or
#'     the number of external flows.
#' \item `external_flow_size`: A numeric scalar or vector specifying the line size for the
#'     external flows (non-interaction flows). Must either be of length 1 or
#'     the number of external flows.
#' \item `external_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for external flow labels. Must either be of
#'     length 1 or the number of external flows.
#' \item `external_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for external flow labels. Must either be of
#'     length 1 or the number of external flows.
#' }

update_settings <- function(diagram_list,
                            diagram_settings = list(
                              var_outline_color = "black",
                              var_fill_color = "#6aa4c8",
                              var_label_text = NA,
                              var_label_color = "white",
                              var_label_size = 10,
                              main_flow_color = "grey25",
                              main_flow_linetype = "solid",
                              main_flow_size = 0.7,
                              main_flow_label_color = "black",
                              main_flow_label_size = 5,
                              main_flow_arrow_size = 0.25,
                              interaction_flow_color = "grey25",
                              interaction_flow_linetype = "dashed",
                              interaction_flow_size = 0.7,
                              interaction_flow_label_color = "black",
                              interaction_flow_label_size = 5,
                              interaction_flow_arrow_size =  0.25,
                              external_flow_color = "grey25",
                              external_flow_linetype = "solid",
                              external_flow_size = 0.7,
                              external_flow_label_color = "black",
                              external_flow_label_size = 5,
                              external_flow_arrow_size =  0.25)
                            ) {


  ###
  # checks
  ###

  # if the user does not provide any settings, warn them
  # and return the data frames
  if(is.null(diagram_settings)) {
    warning("No settings were provided; returning diagram_list without updates.")
    return(diagram_list)
  }  # otherwise, carry on


  # load the defaults
  ds <- eval(formals(update_settings)$diagram_settings)

  # check user inputs provided in diagram_settings,
  # if user supplies a non-recognized argument, stop
  nonrecognized_inputs <- setdiff(names(diagram_settings),  names(ds))
  if (length(nonrecognized_inputs>0) )
  {
    stop('These elements of diagram_settings are not recognized: ', nonrecognized_inputs)
  }


  # check if anything is different from the defaults. if not, warn the
  # user that no changes were requested and the the diagram_list is
  # returned with no updates.




  ###
  # preliminaries for bookkeeping
  ###
  # unlist the data frames to objects
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  # determine number of variables and each flow
  nvars <- nrow(variables)
  nmain <- sum(flows$type == "main")
  ninteraction <- sum(flows$type == "interaction")
  nexternal <- sum(flows$type == "external")


  ###
  # update variable settings
  ###
  # extract list names for variable settings
  var_settings_names <- grep("var_", names(diagram_settings), value = TRUE)

  # test that all entries are of length 1 or nvars
  for(dovar in var_settings_names) {
    tmp <- diagram_settings[[dovar]]
    if(!length(tmp) %in% c(1, nvars)) {
      stop(paste("Length of", dovar, "must be either 1 or", nvars))
    }
  }

  # make a data frame of settings. number of rows is equal to nvars
  var_settings <- data.frame(id = 1:nvars)

  var_settings <- as.data.frame(t(unlist(diagram_settings[var_settings_names])))
  # check to make sure var_settings is 1 row or nvars rows
  if(!nrow(var_settings) %in% c(1, nvars)) {
    stop("Length of variable settings must be one or ")
  }



}
