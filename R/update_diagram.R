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

update_diagram <- function(diagram_list,
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
  ds <- eval(formals(update_diagram)$diagram_settings)

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
  check_diff <- all.equal(diagram_settings, ds)
  if(check_diff == TRUE) {
    warning("Settings provided are the same as defautls; returning diagram_list without updates.")
    return(diagram_list)
  }  # otherwise, carry on



  ###
  ###
  # at this point, we have determined that at least one thing in the
  # diagram_list provided by the user is different than the defaults.
  # therefore, we can now update the settings columns in the variables
  # flows diagrams accordingly, after making sure the diagram_settings
  # provided by the user pass a few extra checks.
  ###
  ###

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
  test_setting_lengths(diagram_settings, var_settings_names, nvars)

  # make a data frame of settings. number of rows is equal to nvars
  var_df_names <- c("color", "fill", "label_color", "label_size")
  new_var_settings <- make_new_settings_df(nvars,
                                           diagram_settings,
                                           var_settings_names,
                                           var_settings_column_names)

  # now update the columns in the variables df
  variables[var_settings_columns] <- new_var_settings[var_settings_columns]



  ###
  # update main flow settings
  ###
  # only execute if there are main flows
  if(nmain > 0) {
    # extract list names for variable settings
    main_settings_names <- grep("main_", names(diagram_settings), value = TRUE)

    # test that all entries are of length 1 or nvars
    test_setting_lengths(diagram_settings, main_settings_names, nmain)

    # make a data frame of settings. number of rows is equal to nmain
    flow_df_names <- unlist(strsplit(main_settings_names, split = "main_flow_"))
    flow_df_names <- flow_df_names[which(flow_df_names != "")]
    new_main_settings <- make_new_settings_df(nmain,
                                              diagram_settings,
                                              main_settings_names,
                                              flow_df_names)

    # now update the columns in the flows data frame for type == "main"
    flows[flows$type == "main", flow_df_names] <- new_main_settings[flow_df_names]
  }



  ###
  # update external flow settings
  ###
  # only execute if there are external flows
  if(nexternal > 0) {
    # extract list names for variable settings
    ext_settings_names <- grep("external_", names(diagram_settings), value = TRUE)

    # test that all entries are of length 1 or nvars
    test_setting_lengths(diagram_settings, ext_settings_names, nexternal)

    # make a data frame of settings. number of rows is equal to nmain
    new_ext_settings <- make_new_settings_df(nexternal,
                                             diagram_settings,
                                             ext_settings_names,
                                             flow_df_names)

    # now update the columns in the flows data frame for type == "main"
    flows[flows$type == "external", flow_df_names] <- new_ext_settings[flow_df_names]
  }


  ###
  # update interaction flow settings
  ###
  # only execute if there are interaction flows
  if(ninteraction > 0) {
    # extract list names for variable settings
    int_settings_names <- grep("interaction_", names(diagram_settings), value = TRUE)

    # test that all entries are of length 1 or nvars
    test_setting_lengths(diagram_settings, int_settings_names, ninteraction)

    # make a data frame of settings. number of rows is equal to nmain
    new_int_settings <- make_new_settings_df(ninteraction,
                                             diagram_settings,
                                             int_settings_names,
                                             flow_df_names)

    # now update the columns in the flows data frame for type == "main"
    flows[flows$type == "interaction", flow_df_names] <- new_int_settings[flow_df_names]
  }


  ###
  # all settings have been updated, return the new diagram_list
  ###
  return(list(variables = variables, flows = flows))
}
