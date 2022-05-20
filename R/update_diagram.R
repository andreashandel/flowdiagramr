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
#' @param diagram_settings A required list of diagram aesthetic settings. See
#'     **details** for allowable syntax. The following elements are supported
#'     and default values are provided:
#' \itemize{
#' \item `var_xmin`: A named numeric vector of mininum x locations.
#' \item `var_xmax`: A named numeric vector of maximum x locations.
#' \item `var_ymin`: A named numeric vector of minimum y locations.
#' \item `var_ymax`: A named numeric vector of maximum y locations.
#' \item `var_xlabel`: A named numeric vector of variable label x locations.
#' \item `var_ylabel`: A named numeric vector of variable label x locations.
#' \item `var_outline_color`: A named character vector of box outline colors.
#'     Default is "black".
#' \item `var_fill_color`: A named character vector of box fill colors. Can be
#'     a named color or HEX code. Default is "#6aa4c8".
#' \item `var_label_color`: A named character vector of variable label colors.
#'     Can be a named color or HEX code. Default is "white".
#' \item `var_label_size`: A named character vector of text sized for variable
#'     labels. Default is 10.
#' \item `flow_xmin`: A named numeric vector of minimum x locations (flow
#'     starting points).
#' \item `flow_xmax`: A named numeric vector of maximum x locations (flow
#'     ending points).
#' \item `flow_ymin`: A named numeric vector of minimum y locations (flow
#'     starting points).
#' \item `flow_ymax`: A named numeric vector of maximum y locations (flow
#'     ending points).
#' \item `flow_xlabel`: A named numeric vector of flow label x locations.
#' \item `flow_ylabel`: A named numeric vector of flow label y locations.
#' \item `flow_curvature`: A named numeric vector numeric curvature values.
#' \item `flow_line_color`: A named character vector specifying the color of
#'     of flow lines. Default is "black".
#' \item `flow_linetype`: A named character vector of linetypes. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." flowdiagramr uses the character name. Default is "solid".
#' \item `flow_line_size`: A named numeric vector of line sizes. Default
#'     value is 1.
#' \item `flow_label_color`: A named character vector of label colors. Default
#'     is "black".
#' \item `flow_label_size`: A named numeric vector of label text sizes. Default
#'     value is 5.
#' \item `flow_show_label`: A named logical vector of whether to display flow
#'     labels (TRUE) or not (FALSE). Default value is TRUE.
#' \item `flow_arrow_size`: A named numeric vector of arrow head sizes. Default
#'     value is 0.25.
#' \item `flow_show_arrow`: A named logical vector of whether to display flows
#'     (TRUE) or not (FALSE). Default value is TRUE.
#' }
#'
#' @return The same list of data frames that was sent into the function
#'     with updated style settings (different values in the updated
#'     columns/variables) of the data frames.
#'
#' @details The user can provide any number of updates in the
#'     \code{diagram_settings} list. Only those provided are updated. If the
#'     function is called with no updates, a warning is issued and the original
#'     data frames list object is returned. For location settings (e.g., xmin)
#'     and curvature settings (just for flows), the user must provide a named
#'     vector for the setting argument, where the name matches one or more of
#'     `variables$name` (for var_* settings) or `flows$name` (for flow_*
#'     settings). For visual aesthetics for variables (e.g., fill_color), the
#'     user must specify a named vector where the name is either "all"
#'     (aesthetic value for all variables) or the name matches one one or more
#'     of the `variables$name`. For visual aesthetics for flows (e.g.,
#'     line_color), the user must specify a named vector, where allowable
#'     names are: "all" (color for all lines,  regardless of type or name),
#'     "main" (color for main flows), "interaction" (color for interaction
#'     flows), "external" (color for external flows), or the name of one of the
#'     character values in `flows$name`. If a mix of type and name are supplied,
#'     the type is applied first and name-specific values are applied second.
#'     See examples.
#'
#' @examples
#' # basic model specification
#' variables <- c("S","I","R")
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' mymodel <- list(variables = variables, flows = flows)
#' diag_list <- prepare_diagram(model_list = mymodel)
#'
#' # make all variable boxes green
#' new_setts <- list(var_fill_color = c(all = "green"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # make just the S box green
#' new_setts <- list(var_fill_color = c(S = "green"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # make all flow lines red
#' new_setts <- list(flow_line_color = c(all = "red"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # make main flow lines red
#' new_setts <- list(flow_line_color = c(main = "red"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # make the b*S*I interaction flow line red
#' new_setts <- list(flow_line_color = c(i_bSI = "red"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # make all flow lines green except for b*S*I interaction flow, which is blue
#' new_setts <- list(flow_line_color = c(all = "green", i_bSI = "blue"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # combine variable and flow settings
#' new_setts <- list(flow_line_color = c(all = "green", i_bSI = "blue"),
#'                   var_fill_color = c(all = "red", S = "cyan"))
#' new_list <- update_diagram(diag_list, new_setts)
#'
#' # more extensive updates
#' newsettings <- list(var_label_color = c(S = "green", I = "blue", R = "red"),
#'                     flow_line_size = c(interaction = 1.5),
#'                     flow_line_color = c(all = "grey25",
#'                                         interaction = "orange",
#'                                         e_n = "red"))
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
