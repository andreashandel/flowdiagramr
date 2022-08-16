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
#' @param diagram_settings An optional list of diagram aesthetic settings. See
#'     **details** for allowable syntax. The following elements are supported
#'     and default values are provided:
#' \itemize{
#' \item `var_xmin`: A named numeric vector of offsets to mininum x locations.
#' \item `var_xmax`: A named numeric vector of offsets to maximum x locations.
#' \item `var_ymin`: A named numeric vector of offsets to minimum y locations.
#' \item `var_ymax`: A named numeric vector of offsets to maximum y locations.
#' \item `var_xlabel`: A named numeric vector of offsets to variable label x locations.
#' \item `var_ylabel`: A named numeric vector of offsets to variable label x locations.
#' \item `var_outline_color`: A named character vector of box outline colors.
#'     Default is "black".
#' \item `var_fill_color`: A named character vector of box fill colors. Can be
#'     a named color or HEX code. Default is "#6aa4c8".
#' \item `var_label_text`: A named character vector of label text for each flow.
#' \item `var_label_color`: A named character vector of variable label colors.
#'     Can be a named color or HEX code. Default is "white".
#' \item `var_label_size`: A named character vector of text sized for variable
#'     labels. Default is 10.
#' \item `flow_xstart`: A named numeric vector of offsets to the minimum x
#'     locations (flow starting points).
#' \item `flow_xend`: A named numeric vector of offsets to the maximum x
#'     locations (flow ending points).
#' \item `flow_ystart`: A named numeric vector of offsets to the minimum y
#'     locations (flow starting points).
#' \item `flow_yend`: A named numeric vector of offsets to the maximum y
#'     locations (flow ending points).
#' \item `flow_xlabel`: A named numeric vector of offsets to the flow label x locations.
#' \item `flow_ylabel`: A named numeric vector of offsets to the flow label y locations.
#' \item `flow_curvature`: A named numeric vector numeric curvature values.
#' \item `flow_line_color`: A named character vector specifying the color of
#'     of flow lines. Default is "black".
#' \item `flow_line_size`: A named numeric vector of line sizes. Default
#'     value is 1.
#' \item `flow_line_type`: A named character vector of linetypes. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." flowdiagramr uses the character name. Default is "solid".
#' \item `flow_label_text`: A named character vector of label text for each flow.
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
#'     function is called with no updates, the names of all elements in
#'     the `variables` and `flows` data frames are returned.
#'     For location settings (e.g., xmin)
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
#'     flows), "external" (color for external flows), "generator" (color for
#'     all generator flows), or the name of one of the
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
#'                                         m_bSI = "red"))
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
    message("No settings were provided; returning names of elements in diagram_list dataframes.")
    varelementnames = paste("Variables:",paste(variables$name, collapse = ", "))
    flowelementnames = paste("Flows:",paste(flows$name, collapse = ", "))
    elementnames = paste0(varelementnames, "\n", flowelementnames)
    return(cat(elementnames))
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
  flow_setting_names <- paste0("flow_", c(
    "xstart",
    "xend",
    "ystart",
    "yend",
    "xlabel",
    "ylabel",
    "curvature",
    "line_color",
    "line_size",
    "line_type",
    "label_text",
    "label_color",
    "label_size",
    "show_label",
    "arrow_size",
    "show_arrow"
  ))

  # concatenate into one character vector of setting names
  setting_names <- c(var_setting_names,
                     flow_setting_names)


  # check user inputs provided in diagram_settings,
  # if user supplies a non-recognized argument, stop
  nonrecognized_inputs <- setdiff(names(diagram_settings),  setting_names)
  if (length(nonrecognized_inputs) > 0)
  {
    stop(paste0('These elements of diagram_settings are not recognized: ',
                nonrecognized_inputs))
  }

  # if the user provides the same setting name twice, send error
  test <- any(duplicated(names(diagram_settings)))
  if(test == TRUE) {
    dups <- names(diagram_settings)[duplicated(names(diagram_settings))]
    stop(paste0("These elements of diagram_settings are duplicated: ",
                dups,
                ". Each element can only be assigned once. See examples."))
  }


  ###
  # extract the lists for variable and flows settings
  ###
  variable_settings <- diagram_settings[grep("var", names(diagram_settings))]
  flow_settings <- diagram_settings[grep("flow", names(diagram_settings))]


  ###
  # update variable settings
  ###
  # extract list names for variable settings
  if(length(variable_settings) > 0) {
    for(i in 1:length(variable_settings)) {
      this_setting <- variable_settings[[i]]

      # check to make sure the setting has named elements
      if(is.null(names(this_setting))) {
        stop("All list elements must be named vectors. One the supplied settings does not contain a named vector of length 1 or greater. See examples.")
      }

      # check to make sure no duplicated names within the setting
      test <- any(duplicated(names(this_setting)))
      if(test == TRUE) {
        dups <- names(this_setting)[duplicated(names(this_setting))]
        stop(paste0("These elements within one of the settings are duplicated: ",
                    dups,
                    ". Each element can only be assigned once. See examples."))
      }

      df_colname <- gsub("var_", "", names(variable_settings)[i])

      # check if this is a location setting, treated as offset below
      # all other, non-location settings are simply overwritten
      loc_flag <- grep(pattern = c("xmin|xmax|ymin|ymax|xlabel|ylabel"),
                       x = names(variable_settings)[i])

      # check for settings applied to all
      for_all <- this_setting["all"]
      if(!is.na(for_all)) {
        if(length(loc_flag) > 0) {
          # add offset
          variables[ , df_colname] <- variables[ , df_colname] + this_setting["all"]
        } else {
          # overwrite
          # this value will apply to all rows in that column
          variables[ , df_colname] <- this_setting["all"]
        }
      }

      # now apply variable-specific, if present
      named_settings <- this_setting[names(this_setting) != "all"]
      test <- which(!names(named_settings) %in% variables$name)
      if(length(test) > 0) {
        stop(paste0("One of the provided names for ",
                    names(variable_settings)[i],
                    " is not present in the variables data frame."))
      } else {
        var_vals <- variables[ , df_colname] # pull out vector of current values
        names(var_vals) <- variables$name  # name for matching
        if(length(loc_flag) > 0) {
          # if locations were changed above by "all", we need also
          # subtract the "all" offset. here we create and all_off object that
          # is 0 by default or the value in "all"
          all_off <- 0
          if(!is.na(for_all)) {
            all_off <- this_setting["all"]
          }

          to_update <- match(names(named_settings), names(var_vals))
          var_vals[to_update] <-  var_vals[to_update] - all_off + named_settings

          # overwrite the variables column (could be same values)
          variables[ , df_colname] <- var_vals
        } else {
          # update the one's that match
          to_update <- match(names(named_settings), names(var_vals))
          var_vals[to_update] <- named_settings

          # overwrite the variables column (could be same values)
          variables[ , df_colname] <- var_vals
        }  # end location if/then
      }  # end if/then for names testing
    }  # end variable settings loop
  }  # end variable settings if/then process chunk

  ###
  # update flow settings
  ###
  if(length(flow_settings) > 0) {
    for(i in 1:length(flow_settings)) {
      this_setting <- flow_settings[[i]]

      # check to make sure the setting has named elements
      if(is.null(names(this_setting))) {
        stop("All list elements must be named vectors. One the supplied settings does not contain a named vector of length 1 or greater. See examples.")
      }

      # check to make sure no duplicated names within the setting
      test <- any(duplicated(names(this_setting)))
      if(test == TRUE) {
        dups <- names(this_setting)[duplicated(names(this_setting))]
        stop(paste0("These elements within one of the settings are duplicated: ",
                    dups,
                    ". Each element can only be assigned once. See examples."))
      }

      df_colname <- gsub("flow_", "", names(flow_settings)[i])

      # check if this is a location setting, treated as offset below
      loc_flag <- grep(pattern = c("xstart|xend|ystart|yend|xlabel|ylabel"),
                       x = names(flow_settings)[i])

      # check for settings applied to all
      for_all <- this_setting["all"]

      if(!is.na(for_all)) {
        # if a location setting, apply as offset
        if(length(loc_flag) > 0) {
          flows[ , df_colname] <- flows[ , df_colname] + this_setting["all"]
        } else {
          # this value will apply to all rows in that column
          flows[ , df_colname] <- this_setting["all"]
        }
      }

      # now apply type specific settings -- not "all" or in flows$name
      typed_settings <- this_setting[names(this_setting) != "all"]
      typed_settings <- typed_settings[!names(typed_settings) %in% flows$name]
      test <- which(!names(typed_settings) %in% flows$type)
      if(length(test) > 0) {
        stop(paste0("One of the provided types/names for ",
                    names(flow_settings)[i],
                    " is not present in the flows data frame."))
      } else {
        typed_df <- data.frame(
          type = names(typed_settings),
          value = typed_settings
        )
        flows <- merge(flows, typed_df, all = TRUE)

        if(length(loc_flag) > 0) {
          # if locations were changed above by "all", we need also
          # subtract the "all" offset. here we create and all_off object that
          # is 0 by default or the value in "all"
          all_off <- 0
          if(!is.na(for_all)) {
            all_off <- this_setting["all"]
          }

          flows[ , df_colname] <- ifelse(is.na(flows$value),
                                         flows[ , df_colname],
                                         flows[ , df_colname] - all_off + flows$value)
          flows$value <- NULL
        } else {
          flows[ , df_colname] <- ifelse(is.na(flows$value),
                                         flows[ , df_colname],
                                         flows$value)
          flows$value <- NULL
        }
      }

      # now apply name specific settings -- not "all" or any possible types
      named_settings <- this_setting[names(this_setting) != "all"]
      avail_types <- c("main", "external", "interaction", "generator")
      named_settings <- named_settings[!names(named_settings) %in% avail_types]
      test <- which(!names(named_settings) %in% flows$name)
      if(length(test) > 0) {
        stop(paste0("One of the provided types/names for ",
                    names(flow_settings)[i],
                    " is not present in the flows data frame."))
      } else {
        named_df <- data.frame(
          name = names(named_settings),
          value = named_settings
        )
        flows <- merge(flows, named_df, all = TRUE)

        if(length(loc_flag) > 0) {
          # if locations were changed above by "all", we need also
          # subtract the "all" offset. here we create and all_off object that
          # is 0 by default or the value in "all"
          all_off <- 0
          if(!is.na(for_all)) {
            all_off <- this_setting["all"]
          }

          flows[ , df_colname] <- ifelse(is.na(flows$value),
                                         flows[ , df_colname],
                                         flows[ , df_colname] - all_off + flows$value)
          flows$value <- NULL
        } else {
          flows[ , df_colname] <- ifelse(is.na(flows$value),
                                         flows[ , df_colname],
                                         flows$value)
          flows$value <- NULL
        }
      }
    }  # end flow settings loop
  }  # end flow settings if/then process chunk


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
