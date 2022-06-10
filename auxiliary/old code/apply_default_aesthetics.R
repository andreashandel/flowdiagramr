#' Applies default aesthetic values to data frames
#' This function pulls the default settings from `make_diagram()`
#' and applies it to variables and flows data frames
#' This is done at the end of prepare_diagram()
#' This way, the user gets data frames that contain columns for these settings
#' and can manually overwrite/replace individual entries
#' This is an internal helper function called by prepare_diagram().
#'
#' @param diagram_list A list consisting of the two data frames variables and flows.
#' @return Same list of data frames supplied as input, with additional columns.
#' Those additional columns contain default specifications for various aesthetics.
#' @export

apply_default_aesthetics <- function(diagram_list) {

  # get default settings from make_diagram inputs
  # assign default settings to be updated by user
  # this is done such that
  defaults <- eval(formals(make_diagram)$diagram_settings)

  var_aesthetics = defaults[grepl("var_",names(defaults))]

  #add all variable related aesthetics to the variable
  # data frame as new columns
  variables <- data.frame(diagram_list$variables, var_aesthetics)


  # # assign settings list to objects
  # for(i in 1:length(defaults)) {
  #   assign(names(defaults)[i], defaults[[i]])
  # }
  #
  # # unlist the data frames to objects
  # variables <- diagram_list$variables
  # flows <- diagram_list$flows
  #
  # # if text size is not provided (NA), then use text sizes in the data
  # # frames. otherwise, override and use the provided sizes for all.
  # if(is.na(var_label_size)) {
  #   var_label_size <- 10
  # } else {
  #   var_label_size <- recycle_values(var_label_size, nrow(variables))
  # }

  # recycle values as needed
  # variables$color <- recycle_values(var_outline_color, nrow(variables))
  # variables$fill <- recycle_values(var_fill_color, nrow(variables))
  # variables$label_color <- recycle_values(var_label_color, nrow(variables))
  # variables$label_size <- recycle_values(var_label_size, nrow(variables))
  # variables$plot_label_size <- NULL

  mainflow_aesthetics = data.frame(defaults[grepl("main_flow",names(defaults))])

  #column names for main/interaction/external should all be the same so they can be combined
  newcolnames <- gsub("main_","",colnames(mainflow_aesthetics))
  colnames(mainflow_aesthetics) <- newcolnames
  mains <- subset(diagram_list$flows, type == "main")
  mains <- dplyr::bind_cols(mains, mainflow_aesthetics)

  interactionflow_aesthetics = data.frame(defaults[grepl("interaction_flow",names(defaults))])
  colnames(interactionflow_aesthetics) <- newcolnames
  ints <- subset(diagram_list$flows, type == "interaction")
  ints <- dplyr::bind_cols(ints, interactionflow_aesthetics)

  externalflow_aesthetics = data.frame(defaults[grepl("external_flow",names(defaults))])
  colnames(externalflow_aesthetics) <- newcolnames
  external <- subset(diagram_list$flows, type == "external")
  external <- dplyr::bind_cols(external, externalflow_aesthetics)

  # recombine flows data frame with aesthetics as columns
  flows <- dplyr::bind_rows(mains, ints, external)

  # mains$on <- recycle_values(main_flow_on, nrow(mains))
  # mains$color <- recycle_values(main_flow_color, nrow(mains))
  # mains$linetype <- recycle_values(main_flow_linetype, nrow(mains))
  # mains$size <- recycle_values(main_flow_size, nrow(mains))
  # mains$label_color <- recycle_values(main_flow_label_color, nrow(mains))
  # mains$label_size <- recycle_values(main_flow_label_size, nrow(mains))

  # ints$color <- recycle_values(interaction_flow_color, nrow(ints))
  # ints$linetype <- recycle_values(interaction_flow_linetype, nrow(ints))
  # ints$size <- recycle_values(interaction_flow_size, nrow(ints))
  # ints$label_color <- recycle_values(interaction_flow_label_color, nrow(ints))
  # ints$label_size <- recycle_values(interaction_flow_label_size, nrow(ints))
  #
  # exts <- subset(flows, type == "external")
  # exts$color <- recycle_values(external_flow_color, nrow(exts))
  # exts$linetype <- recycle_values(external_flow_linetype, nrow(exts))
  # exts$size <- recycle_values(external_flow_size, nrow(exts))
  # exts$label_color <- recycle_values(external_flow_label_color, nrow(exts))
  # exts$label_size <- recycle_values(external_flow_label_size, nrow(exts))


  return(list(variables = variables, flows = flows))
}

