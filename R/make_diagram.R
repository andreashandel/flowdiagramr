#' Make a ggplot2 model diagram.
#'
#' @description
#' `make_diagram()` generates a **ggplot2** object based on the data frames
#' made with \code{\link{prepare_diagram}}. The function only applies
#' aesthetics that are not associated with x, y locations. Colors, linetypes,
#' and other graphical options can be set by the user.
#'
#' @param diagram_list A required list of data frames returned from the
#'     \code{prepare_diagram} function. See that function for details
#'     about this object.
#' @param diagram_settings An optional list of diagram aesthetic settings. The
#'     following elements are supported and default values are provided:
#' \itemize{
#' \item `var_outline_color`: A character string or vector of character strings
#'     specifying the color of variable outlines. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' \item `var_fill_color`: A character string or vector of character strings
#'     specifying the fill color of variables. If a vector, the colors will be
#'     recycled in the order of the variables in the supplied data frame.
#' \item `var_text_color`: A character string or vector of character strings
#'     specifying the text color for variable labels. If a vector, the colors will
#'     be recycled in the order of the variables in the supplied data frame.
#' \item `var_text_size`: A numeric scalar specifying the text size for variable
#'     labels. Default value is NA and the values in the `variables` data frame
#'     returned by \code{\link{prepare_diagram}} are used. If a non-NA value is
#'     supplied here, the values in the `variables` data frame are not used and
#'     are replaced by the value specified here.
#'
#' \item `main_flow_on`: A logical indicating if the main flow arrows should be plotted.
#' \item `main_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the values will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `main_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for main flows (non-interaction flows). This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_size`: A numeric scalar or vector specifying the line size for the
#'     main flows (non-interaction flows). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_label_on`: A logical indicating if the labels for the main
#'     flows should be plotted.
#' \item `main_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for main flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `main_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for main flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#'
#' \item `interaction_flow_on`: A logical indicating if the interaction flow arrows should be plotted.
#' \item `interaction_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the values will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `interaction_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for interaction flows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_size`: A numeric scalar or vector specifying the line size for the
#'     interaction flows (non-interaction flows). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_label_on`: A logical indicating if the labels for the interaction
#'     flows should be plotted.
#' \item `interaction_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for interaction flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `interaction_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for interaction flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#'
#' \item `external_flow_on`: A logical indicating if the external flow arrows should be plotted.
#' \item `external_flow_color`: A character string or vector of character strings
#'     specifying the text color for non-interaction flow arrows.
#'     If a vector, the values will be recycled in the order of the flows
#'     in the supplied data frame.
#' \item `external_flow_linetype`: Either a numeric scalar/vector or a character scalar/vector
#'     specifying the linetype for external flows. This
#'     argument is passed to the \code{linetype} argument in ggplot2. From
#'     the ggplot2 documentation: "The linetype aesthetic can be specified
#'     with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed,
#'     3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a
#'     discrete variable, or a string of an even number (up to eight) of
#'     hexadecimal digits which give the lengths in consecutive positions in
#'     the string." Default is 1 (solid). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_size`: A numeric scalar or vector specifying the line size for the
#'     external flows (non-interaction flows). If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_label_on`: A logical indicating if the labels for the external
#'     flows should be plotted.
#' \item `external_flow_label_color`: A character string or vector of character strings
#'     specifying the text color for external flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#' \item `external_flow_label_size`: A scalar or numeric vector
#'     specifying the text size for external flow labels. If a vector, the values will
#'     be recycled in the order of the flows in the supplied data frame.
#`
#' \item `with_grid` A logical indicating whether to return the ggplot
#'     with a grid. Default is FALSE. The grid can be helpful if you
#'     want/need to move items around.
#' }
#'
#'
#' @return A ggplot2 object.
#' @import ggplot2
#' @export
#'
#' @examples
#' varlabels <- c("S","I","R")
#' varnames <- c("Susceptible","Infected","Recovered")  # optional
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' varlocations <-  matrix(data = c("S", "", "R", "", "I", "" ),
#'                         nrow = 2, ncol = 3, byrow = TRUE)
#' mymodel <- list(varlabels = varlabels, varnames = varnames,
#' flows = flows, varlocations = varlocations)
#' diagram_list <- prepare_diagram(model_list = mymodel)
#'
#' # make diagram without grid
#' diagram <- make_diagram(diagram_list)
#'
#' # make diagram with grid
#' diagram_with_grid <- make_diagram(diagram_list, diagram_settings = list(with_grid = TRUE))

make_diagram <- function (diagram_list,
                          diagram_settings = list(
                            var_outline_color = NA,
                            var_fill_color = "#6aa4c8",
                            var_text_color = "white",
                            var_text_size = NA,
                            main_flow_on = TRUE,
                            main_flow_color = "grey25",
                            main_flow_linetype = "solid",
                            main_flow_size = 0.7,
                            main_flow_label_on = TRUE,
                            main_flow_label_color = "black",
                            main_flow_label_size = 5,
                            interaction_flow_on = TRUE,
                            interaction_flow_color = "grey25",
                            interaction_flow_linetype = "dashed",
                            interaction_flow_size = 0.7,
                            interaction_flow_label_on = TRUE,
                            interaction_flow_label_color = "black",
                            interaction_flow_label_size = 5,
                            external_flow_on = TRUE,
                            external_flow_color = "grey25",
                            external_flow_linetype = "solid",
                            external_flow_size = 0.7,
                            external_flow_label_on = TRUE,
                            external_flow_label_color = "black",
                            external_flow_label_size = 5,
                            with_grid = FALSE)
                          ) {
  # TODO error checking

  # assign default settings to be updated by user
  defaults <- eval(formals(make_diagram)$diagram_settings)

  # update defaults with user settings
  defaults[names(diagram_settings)] <- diagram_settings

  # assign settings list to objects
  for(i in 1:length(defaults)) {
    assign(names(defaults)[i], defaults[[i]])
  }

  # unlist the data frames to objects
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  if(interaction_flow_label_on == FALSE) {
    # This removes interaction segments and puts the flow label
    # back with the physical flow.
    flows <- move_interaction_label(flows)
  }

  # if text size is not provided (NA), then use text sizes in the data
  # frames. otherwise, override and use the provided sizes for all.
  if(is.na(var_text_size)) {
    var_text_size <- variables$plot_label_size
  } else {
    var_text_size <- recycle_values(var_text_size, nrow(variables))
  }

  # recycle values as needed
  variables$color <- recycle_values(var_outline_color, nrow(variables))
  variables$fill <- recycle_values(var_fill_color, nrow(variables))
  variables$label_color <- recycle_values(var_text_color, nrow(variables))
  variables$label_size <- recycle_values(var_text_size, nrow(variables))
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
  flows <- rbind(mains, ints, exts)
  flows$arrowsize <- 0.25  # default arrow size

  # turn off flows completely by setting linetype to blank as needed
  if(main_flow_on == FALSE) {
    flows[flows$type == "main", "linetype"] <- "blank"
    flows[flows$type == "main", "arrowsize"] <- 0
  }
  if(interaction_flow_on == FALSE) {
    flows[flows$type == "interaction", "linetype"] <- "blank"
    flows[flows$type == "interaction", "arrowsize"] <- 0
  }
  if(external_flow_on == FALSE) {
    flows[flows$type == "external", "linetype"] <- "blank"
    flows[flows$type == "external", "arrowsize"] <- 0
  }

  # set label to "" to suppress label if requested
  # also don't show label if the flow itself is turned off
  flows$math <- flows$label
  if(main_flow_on == FALSE || main_flow_label_on == FALSE) {
    flows[flows$type == "main", "label"] <- ""
  }
  if(interaction_flow_on == FALSE || interaction_flow_label_on == FALSE) {
    flows[flows$type == "interaction", "label"] <- ""
  }
  if(external_flow_on == FALSE || external_flow_label_on == FALSE) {
    flows[flows$type == "external", "label"] <- ""
  }


  # get the ggplot2 code as text
  code <- get_code()

  # evaluate the ggplot2 code using current environment args
  theplot <- eval(parse(text = code))

  return(theplot)
}
