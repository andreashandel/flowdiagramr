#' Applies default aesthetic values to data frames
#'
#' `apply_default_aesthetics` applies the default ggplot2 aesthetics from
#' `make_diagram()`. This is an internal helper function.
#'
#' @param diagram_list A list of data frames.
#' @return A list of data frames.
#' @noRd

apply_default_aesthetics <- function(diagram_list) {
  # assign default settings to be updated by user
  defaults <- eval(formals(make_diagram)$diagram_settings)

  # assign settings list to objects
  for(i in 1:length(defaults)) {
    assign(names(defaults)[i], defaults[[i]])
  }

  # unlist the data frames to objects
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  # if text size is not provided (NA), then use text sizes in the data
  # frames. otherwise, override and use the provided sizes for all.
  if(is.na(var_label_size)) {
    var_label_size <- variables$plot_label_size
  } else {
    var_label_size <- recycle_values(var_label_size, nrow(variables))
  }

  # recycle values as needed
  variables$color <- recycle_values(var_outline_color, nrow(variables))
  variables$fill <- recycle_values(var_fill_color, nrow(variables))
  variables$label_color <- recycle_values(var_label_color, nrow(variables))
  variables$label_size <- recycle_values(var_label_size, nrow(variables))
  variables$plot_label_size <- NULL

  mains <- subset(flows, type == "main")
  mains$color <- recycle_values(main_flow_color, nrow(mains))
  mains$linetype <- recycle_values(main_flow_linetype, nrow(mains))
  mains$size <- recycle_values(main_flow_size, nrow(mains))
  mains$label_color <- recycle_values(main_flow_label_color, nrow(mains))
  mains$label_size <- recycle_values(main_flow_label_size, nrow(mains))

  ints <- subset(flows, type == "interaction")
  ints$color <- recycle_values(interaction_flow_color, nrow(ints))
  ints$linetype <- recycle_values(interaction_flow_linetype, nrow(ints))
  ints$size <- recycle_values(interaction_flow_size, nrow(ints))
  ints$label_color <- recycle_values(interaction_flow_label_color, nrow(ints))
  ints$label_size <- recycle_values(interaction_flow_label_size, nrow(ints))

  exts <- subset(flows, type == "external")
  exts$color <- recycle_values(external_flow_color, nrow(exts))
  exts$linetype <- recycle_values(external_flow_linetype, nrow(exts))
  exts$size <- recycle_values(external_flow_size, nrow(exts))
  exts$label_color <- recycle_values(external_flow_label_color, nrow(exts))
  exts$label_size <- recycle_values(external_flow_label_size, nrow(exts))

  # recombine flows data frame with aesthetics as columns
  flows <- rbind(mains, exts, ints)
  flows$arrowsize <- 0.25  # default arrow size Applies default aesthetic values to data frames

  return(list(variables = variables,
              flows = flows))
}

