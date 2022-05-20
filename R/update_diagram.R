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
#' \item `var_xmin`: A named vector a mininum x locations for the specified
#'     variable. The name of each element must match one of the character
#'     values in the `variables$name` column. Must be numeric.
#' \item `var_xmax`: A named vector a maximum x locations for the specified
#'     variable. The name of each element must match one of the character
#'     values in the `variables$name` column. Must be numeric.
#' \item `var_ymin`: A named vector a minimum y locations for the specified
#'     variable. The name of each element must match one of the character
#'     values in the `variables$name` column. Must be numeric.
#' \item `var_ymax`: A named vector a maximum y locations for the specified
#'     variable. The name of each element must match one of the character
#'     values in the `variables$name` column. Must be numeric.
#' \item `var_xlabel`: A named vector x locations for the specified
#'     variable label. The name of each element must match one of the character
#'     values in the `variables$name` column. Must be numeric.
#' \item `var_ylabel`: A named vector y locations for the specified
#'     variable label. The name of each element must match one of the character
#'     values in the `variables$name` column. Must be numeric.
#' \item `var_outline_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `variables$name` column. If a scalar, the character value
#'     will be applied to every variable. Default is "black".
#' \item `var_fill_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `variables$name` column. If a scalar, the character value
#'     will be applied to every variable. Can be a named color or HEX code.
#'     Default is "#6aa4c8".
#' \item `var_label_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `variables$name` column. If a scalar, the character value
#'     will be applied to every variable. Can be a named color or HEX code.
#'     Default is "white".
#' \item `var_label_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `variables$name` column. If a scalar, the numeric value
#'     will be applied to every variable. Default is 10.
#'
#' \item `flow_xmin`: A named vector a mininum x locations for the specified
#'     flow. The name of each element must match one of the character
#'     values in the `flows$name` column. Must be numeric. xmin is the
#'     starting point of the flow.
#' \item `flow_xmax`: A named vector a maximum x locations for the specified
#'     flow. The name of each element must match one of the character
#'     values in the `flows$name` column. Must be numeric. xmax is the
#'     ending point of the flow.
#' \item `flow_ymin`: A named vector a mininum y locations for the specified
#'     flow. The name of each element must match one of the character
#'     values in the `flows$name` column. Must be numeric. ymin is the
#'     starting point of the flow.
#' \item `flow_ymax`: A named vector a maximum x locations for the specified
#'     flow. The name of each element must match one of the character
#'     values in the `flows$name` column. Must be numeric. ymax is the
#'     ending point of the flow.
#' \item `flow_xlabel`: A named vector x locations for the specified
#'     flow label. The name of each element must match one of the character
#'     values in the `flows$name` column. Must be numeric.
#' \item `flow_ylabel`: A named vector y locations for the specified
#'     flow label. The name of each element must match one of the character
#'     values in the `flows$name` column. Must be numeric.
#' \item `flow_curvature`: A named vector numeric curvature values for the
#'     specified flow label. The name of each element must match one of the
#'     character values in the `flows$name` column. Must be numeric.
#'
#' \item `main_flow_line_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the character value will be applied to every flow of type "main".
#'     Default is "black".
#' \item `main_flow_linetype`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the character value will be applied to every flow of type "main". This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." flowdiagramr uses the character name. Default is "solid".
#' \item `main_flow_line_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the numeric value will be applied to every flow of type "main". Default
#'     value is 1.
#' \item `main_flow_label_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the character value will be applied to every flow of type "main".
#'     Default is "black".
#' \item `main_flow_label_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the numeric value will be applied to every flow of type "main". Default
#'     value is 5.
#' \item `main_flow_show_label`: Either a scalar or named logical vector
#'     indicating whether the flow label should be shown (TRUE) or not (FALSE).
#'     If a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the numeric value will be applied to every flow of type "main". Default
#'     value is TRUE.
#' \item `main_flow_arrow_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "main"`. If a scalar,
#'     the numeric value will be applied to every flow of type "main". Default
#'     value is 0.25.
#' \item `main_flow_show_arrow`: Either a scalar or named logical vector
#'     indicating whether the entire flow line and arrow should be drawn (TRUE)
#'     or not (FALSE). If a named vector, the name of each element must match
#'     one of the character values in the `flows$name` column of
#'     `flows$type = "main"`. If a scalar, the numeric value will be applied to
#'     every flow of type "main". Default value is TRUE.
#'
#' \item `interaction_flow_line_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the character value will be applied to every flow of type "interaction".
#'     Default is "black".
#' \item `interaction_flow_linetype`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the character value will be applied to every flow of type "interaction". This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." flowdiagramr uses the character name. Default is "solid".
#' \item `interaction_flow_line_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the numeric value will be applied to every flow of type "interaction". Default
#'     value is 1.
#' \item `interaction_flow_label_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the character value will be applied to every flow of type "interaction".
#'     Default is "black".
#' \item `interaction_flow_label_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the numeric value will be applied to every flow of type "interaction". Default
#'     value is 5.
#' \item `interaction_flow_show_label`: Either a scalar or named logical vector
#'     indicating whether the flow label should be shown (TRUE) or not (FALSE).
#'     If a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the numeric value will be applied to every flow of type "interaction". Default
#'     value is TRUE.
#' \item `interaction_flow_arrow_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "interaction"`. If a scalar,
#'     the numeric value will be applied to every flow of type "interaction". Default
#'     value is 0.25.
#' \item `interaction_flow_show_arrow`: Either a scalar or named logical vector
#'     indicating whether the entire flow line and arrow should be drawn (TRUE)
#'     or not (FALSE). If a named vector, the name of each element must match
#'     one of the character values in the `flows$name` column of
#'     `flows$type = "interaction"`. If a scalar, the numeric value will be applied to
#'     every flow of type "interaction". Default value is TRUE.
#'
#' \item `external_flow_line_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the character value will be applied to every flow of type "external".
#'     Default is "black".
#' \item `external_flow_linetype`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the character value will be applied to every flow of type "external". This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." flowdiagramr uses the character name. Default is "solid".
#' \item `external_flow_line_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the numeric value will be applied to every flow of type "external". Default
#'     value is 1.
#' \item `external_flow_label_color`: Either a scalar or a named character vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the character value will be applied to every flow of type "external".
#'     Default is "black".
#' \item `external_flow_label_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the numeric value will be applied to every flow of type "external". Default
#'     value is 5.
#' \item `external_flow_show_label`: Either a scalar or named logical vector
#'     indicating whether the flow label should be shown (TRUE) or not (FALSE).
#'     If a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the numeric value will be applied to every flow of type "external". Default
#'     value is TRUE.
#' \item `external_flow_arrow_size`: Either a scalar or a named numeric vector. If
#'     a named vector, the name of each element must match one of the character
#'     values in the `flows$name` column of `flows$type = "external"`. If a scalar,
#'     the numeric value will be applied to every flow of type "external". Default
#'     value is 0.25.
#' \item `external_flow_show_arrow`: Either a scalar or named logical vector
#'     indicating whether the entire flow line and arrow should be drawn (TRUE)
#'     or not (FALSE). If a named vector, the name of each element must match
#'     one of the character values in the `flows$name` column of
#'     `flows$type = "external"`. If a scalar, the numeric value will be applied to
#'     every flow of type "external". Default value is TRUE.
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
#'                     interaction_flow_line_size = 1.5,
#'                     interaction_flow_line_color = "orange")
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
                                "xmin",
                                "xmax",
                                "ymin",
                                "ymax",
                                "xlabel",
                                "ylabel",
                                "outline_color",
                                "fill_color",
                                "label_text",
                                "label_color",
                                "label_size"
                              ))

  # possible flow settings are the following
  # these don't get prepended with flow type, just flow
  all_flow_setting_names <- paste0("flow_", c(
    "xmin",
    "xmax",
    "ymin",
    "ymax",
    "xlabel",
    "ylabel",
    "curvature"
  ))
  flow_setting_names <- c(  # these do get prepended with flow type
    "line_color",
    "linetype",
    "line_size",
    "label_color",
    "label_size",
    "show_label",
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
  setting_names <- c(var_setting_names,
                     all_flow_setting_names,
                     flow_setting_names)


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
  nflow <- nrow(flows)
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
  # update flow settings not specific to flow type
  ###
  if(length(flow_settings) > 0) {
    flow_settings_names <- grep("main_|external_|interaction_",
                                names(flow_settings),
                                invert = TRUE,
                                value = TRUE)
    if(length(flow_settings_names) > 0) {
      # test that all entries are of length nvars
      checkmsg <- test_setting_lengths(flow_settings, flow_settings_names, nflow)
      if(!is.null(checkmsg))
      {
        stop(checkmsg)
      }

      # make a data frame of settings. number of rows is equal to nmain
      new_flow_settings <- make_new_settings_df(nflow,
                                                flow_settings[flow_settings_names])

      # now update the columns in the flows data frame for type == "main"
      flows[ , colnames(new_flow_settings)] <- new_flow_settings
    }
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
                                                  flow_settings[main_settings_names])

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
                                                 flow_settings[ext_settings_names])

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
                                                 flow_settings[int_settings_names])

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
