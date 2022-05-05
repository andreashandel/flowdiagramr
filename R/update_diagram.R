#' Updates the aesthetics settings in the variables and flows data frames
#'
#' @description
#' This function takes a list as created by \code{\link{prepare_diagram}}
#' and allows the user to update the styling of the boxes and arrows by setting
#' the different arguments in the input list of this function.
#'
#' @param diagram_list A required list of data frames returned from the
#'     \code{\link{prepare_diagram}} function. See that function for details
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
#' \item `main_flow_line_color`: A character string or vector of character strings
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
#' \item `main_flow_line_size`: A numeric scalar or vector specifying the line size for the
#'     main flows (non-interaction flows). Must either
#'     be of length 1 or the number of main flows.
#' \item `main_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for main flow labels. Must either
#'     be of length 1 or the number of main flows.
#' \item `main_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for main flow labels. Must either
#'     be of length 1 or the number of main flows.
#'
#' \item `interaction_flow_line_color`: A character string or vector of character strings
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
#' \item `interaction_flow_line_size`: A numeric scalar or vector specifying the line size for the
#'     interaction flows (non-interaction flows). Must either
#'     be of length 1 or the number of interaction flows.
#' \item `interaction_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for interaction flow labels. Must either
#'     be of length 1 or the number of interaction flows.
#' \item `interaction_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for interaction flow labels. Must either
#'     be of length 1 or the number of interaction flows.
#'
#' \item `external_flow_line_color`: A character string or vector of character strings
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
#' \item `external_flow_line_size`: A numeric scalar or vector specifying the line size for the
#'     external flows (non-interaction flows). Must either be of length 1 or
#'     the number of external flows.
#' \item `external_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for external flow labels. Must either be of
#'     length 1 or the number of external flows.
#' \item `external_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for external flow labels. Must either be of
#'     length 1 or the number of external flows.
#' }
#'
#' @return The same list of data frames that was sent into the function
#'     with updated style settings (different values in the updated
#'     columns/variables) of the data frames.
#'
#' @details The user can provide any number of updates in the
#'     \code{diagram_settings} list. Only those provided are updated. If the
#'     function is called with no updates, a warning is issued and the original
#'     data frames list object is returned.
#'
#' @examples
#' # basic model specification
#' variables <- c("S","I","R")
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' mymodel <- list(variables = variables, flows = flows)
#' diag_list <- prepare_diagram(model_list = mymodel)
#' diag_list2 <- update_diagram(diag_list, diagram_settings = list(var_fill_color = "green"))
#'
#'
#'
#' # more extensive updates
#' newsettings <- list(var_label_color = c("green","blue","red"),
#'                     interaction_flow_size = 1.5,
#'                     interaction_flow_color = "orange")
#' diag_list3 <- update_diagram(diag_list, diagram_settings = newsettings)
#'
#' @export





update_diagram <- function(diagram_list, diagram_settings = NULL) {


  ###
  # checks:
  # make sure diagram_list is a valid object
  #
  # make sure any provided input in diagram_settings is a valid column
  # in diagram_list
  #
  # if a setting is provided as vector, make sure length matches number of
  # entities of that type (e.g. number of interaction flows)
  ###

  # extract the list elements as separate data frames
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  # if the user does not provide any settings, warn them
  # and return the data frames
  if(is.null(diagram_settings)) {
    warning("No settings were provided; returning diagram_list without updates.")
    return(diagram_list)
  }  # otherwise, carry on

  # possible variable (var) settings are the following
  var_setting_names <- paste0("var_",
                              c(
                                "outline_color",
                                "fill_color",
                                "label_text",
                                "label_color",
                                "label_size"
                              ))

  # possible flow settings are the following
  flow_setting_names <- c(
    "line_color",
    "linetype",
    "line_size",
    "label_color",
    "label_size",
    "arrow_size",
    "show_arrow"
  )

  # append flow type to each possible setting
  flow_setting_names <- c(
    paste0("main_flow_", flow_setting_names),
    paste0("interaction_flow_", flow_setting_names),
    paste0("external_flow_", flow_setting_names)
  )

  # concatenate into one character vector of setting names
  setting_names <- c(var_setting_names, flow_setting_names)


  # check user inputs provided in diagram_settings,
  # if user supplies a non-recognized argument, stop
  nonrecognized_inputs <- setdiff(names(diagram_settings),  setting_names)
  if (length(nonrecognized_inputs) > 0)
  {
    stop('These elements of diagram_settings are not recognized: ', nonrecognized_inputs)
  }


  ###
  # preliminaries for bookkeeping
  ###

  # determine number of variables and number of each flow type
  nvars <- nrow(variables)
  nmain <- sum(flows$type == "main")
  ninteraction <- sum(flows$type == "interaction")
  nexternal <- sum(flows$type == "external")

  # seperate out variable settings and flow settings
  variable_settings <- diagram_settings[grep("var", names(diagram_settings))]
  flow_settings <- diagram_settings[grep("flow", names(diagram_settings))]

  ###
  # update variable settings
  ###
  # extract list names for variable settings
  if(length(variable_settings) > 0) {
    var_settings_names <- grep("var_", names(diagram_settings), value = TRUE)

    # test that all entries are of length 1 or nvars
    checkmsg <- test_setting_lengths(diagram_settings, var_settings_names, nvars)
    if(!is.null(checkmsg))
    {
      stop(checkmsg)
    }


    # make a data frame of settings. number of rows is equal to nvars
    new_var_settings <- make_new_settings_df(nvars, variable_settings)

    # now update the columns in the variables df
    variables[colnames(new_var_settings)] <- new_var_settings
  }



  ###
  # update main flow settings
  ###
  if(length(flow_settings) > 0) {
    # only execute if there are main flows
    if(nmain > 0) {
      # extract list names for variable settings
      main_settings_names <- grep("main_", names(flow_settings), value = TRUE)
      if(length(main_settings_names) > 0) {
        # test that all entries are of length 1 or nvars
        checkmsg <- test_setting_lengths(flow_settings, main_settings_names, nmain)
        if(!is.null(checkmsg))
        {
          stop(checkmsg)
        }

        # make a data frame of settings. number of rows is equal to nmain
        new_main_settings <- make_new_settings_df(nmain,
                                                  flow_settings)

        # now update the columns in the flows data frame for type == "main"
        flows[flows$type == "main", colnames(new_main_settings)] <- new_main_settings
      }
    }

    ###
    # update external flow settings
    ###
    # only execute if there are external flows
    if(nexternal > 0) {
      # extract list names for variable settings
      ext_settings_names <- grep("external_", names(flow_settings), value = TRUE)

      if(length(ext_settings_names) > 0) {
        # test that all entries are of length 1 or nvars
        checkmsg <- test_setting_lengths(flow_settings, ext_settings_names, nexternal)
        if(!is.null(checkmsg))
        {
          stop(checkmsg)
        }

        # make a data frame of settings. number of rows is equal to nmain
        new_ext_settings <- make_new_settings_df(nexternal,
                                                 flow_settings)

        # now update the columns in the flows data frame for type == "main"
        flows[flows$type == "external", colnames(new_ext_settings)] <- new_ext_settings
      }
    }

    ###
    # update interaction flow settings
    ###
    # only execute if there are interaction flows
    if(ninteraction > 0) {
      # extract list names for variable settings
      int_settings_names <- grep("interaction_", names(flow_settings), value = TRUE)

      if(length(int_settings_names) > 0) {
        # test that all entries are of length 1 or nvars
        checkmsg <- test_setting_lengths(flow_settings, int_settings_names, ninteraction)
        if(!is.null(checkmsg))
        {
          stop(checkmsg)
        }

        # make a data frame of settings. number of rows is equal to nmain
        new_int_settings <- make_new_settings_df(ninteraction,
                                                 flow_settings)

        # now update the columns in the flows data frame for type == "main"
        flows[flows$type == "interaction", colnames(new_int_settings)] <- new_int_settings
      }
    }
  }


  ###
  # Check the data frames for conformity
  ###
  test <- check_dataframes(list(variables = variables, flows = flows))
  if(!is.null(test)) {
    stop(test)
  }

  ###
  # all settings have been updated, return the new diagram_list
  ###
  return(list(variables = variables, flows = flows))
}
