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
#' \item `var_label_on`: A logical indicating if the labels for the variables
#'     should be plotted.
#' \item `var_label_color`: A character string or vector of character strings
#'     specifying the text color for variable labels. If a vector, the colors will
#'     be recycled in the order of the variables in the supplied data frame.
#' \item `var_label_text`: A character vector the same length as the number
#'     of variables in the `variables` data frame. If provided, these values
#'     update the variable labels provided to \code{\link{prepare_diagram}}.
#' \item `var_label_size`: A numeric scalar specifying the text size for variable
#'     labels. Note that any value supplied here overwrites
#'     entries in the list structure returned by \code{\link{prepare_diagram}}.
#'     Specifically, if you set this parameter when calling \code{\link{prepare_diagram}}
#'     with \code{use_varnames = TRUE}, the value is used to compute box size,
#'     but then the actual size of the label as provided here is applied.
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
#' @return A ggplot2 object.
#' @examples
#' mymodel = list(varlabels = c("S","I","R"),
#'                flows = list(S_flows = c("-b*S*I"),
#'                             I_flows = c("b*S*I","-g*I"),
#'                             R_flows = c("g*I") ) )
#' diagram_list <- prepare_diagram(model_list = mymodel)
#'
#' # make diagram without grid
#' diagram <- make_diagram(diagram_list)
#'
#' # make diagram with grid
#' diagram_with_grid <- make_diagram(diagram_list, diagram_settings = list(with_grid = TRUE))
#' @import ggplot2
#' @export
#'

make_diagram <- function (diagram_list,
                          diagram_settings = list(
                            var_outline_color = NA,
                            var_fill_color = "#6aa4c8",
                            var_label_text = NA,
                            var_label_on = TRUE,
                            var_label_color = "white",
                            var_label_size = NA,
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
  # TODO Compare default prepare diagram output to that provided
  # and issue warning if styling is changed AND diagram_settings changed
  # to tell user that diagram_settings takes priority. Will require prepare_diagram
  # to return the inputs.

  # unlist the data frames to objects
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  # assign the plot_label column, can be updated below if specified
  variables$plot_label <- variables$label


  # assign default settings to be updated by user
  defaults <- eval(formals(make_diagram)$diagram_settings)

  # check user inputs provided in diagram_settings, if user supplies a non-recognized argument, stop
  nonrecognized_inputs <- setdiff(names(diagram_settings),  names(defaults))
  if (length(nonrecognized_inputs>0) )
  {
    stop('These elements of diagram_settings are not recognized: ', nonrecognized_inputs)
  }

  # update defaults with user settings
  defaults[names(diagram_settings)] <- diagram_settings

  # check whether defaults are updated, except for with_grid
  with_grid <- defaults$with_grid
  defaults$with_grid <- NULL
  def2 <- eval(formals(make_diagram)$diagram_settings)
  def2$with_grid <- NULL
  check <- all.equal(def2, defaults)

  # if the two lists are different, user wants to update the settings,
  # so we do so. otherwise we can just use the settings already in the
  # dataframes.
  if(check[1] != TRUE) {
    # assign settings list to objects
    for(i in 1:length(defaults)) {
      assign(names(defaults)[i], defaults[[i]])
    }

    # if(interaction_flow_label_on == FALSE) {
    #   # This removes interaction segments and puts the flow label
    #   # back with the physical flow.
    #   flows <- move_interaction_label(flows)
    # }

    # if the var_label_text is not provided (NA), then use the labels
    # in the data frame. otherwise, override.
    if(anyNA(var_label_text)) {
      variables$plot_label <- variables$label
    } else {
      #original version
      #variables$plot_label <- gsub(" ", "\n", var_label_text)
      #new version, don't automatically put words on new lines
      variables$plot_label <- var_label_text
    }

    # if text size is not provided (NA), then use text sizes in the data
    # frames. otherwise, override and use the provided sizes for all.
    if(is.na(var_label_size)) {
      var_label_size <- variables$label_size
    } else {
      var_label_size <- recycle_values(var_label_size, nrow(variables))
    }

    # setup linetypes mapping from numeric to text
    ltys <- data.frame(code = 0:6,
                       text = c("blank", "solid", "dashed",
                                "dotted", "dotdash", "longdash",
                                "twodash"))

    # recycle values as needed
    variables$color <- recycle_values(var_outline_color, nrow(variables))
    variables$fill <- recycle_values(var_fill_color, nrow(variables))
    variables$label_color <- recycle_values(var_label_color, nrow(variables))
    variables$label_size <- recycle_values(var_label_size, nrow(variables))
    variables$plot_label_size <- NULL

    mains <- subset(flows, type == "main")
    mains$color <- recycle_values(main_flow_color, nrow(mains))
    if(is.numeric(main_flow_linetype)){
      main_flow_linetype <- subset(ltys, code == main_flow_linetype)[,"text"]
    }
    mains$linetype <- recycle_values(main_flow_linetype, nrow(mains))
    mains$size <- recycle_values(main_flow_size, nrow(mains))
    mains$label_color <- recycle_values(main_flow_label_color, nrow(mains))
    mains$label_size <- recycle_values(main_flow_label_size, nrow(mains))

    ints <- subset(flows, type == "interaction")
    ints$color <- recycle_values(interaction_flow_color, nrow(ints))
    if(is.numeric(interaction_flow_linetype)){
      interaction_flow_linetype <- subset(ltys, code == interaction_flow_linetype)[,"text"]
    }
    ints$linetype <- recycle_values(interaction_flow_linetype, nrow(ints))
    ints$size <- recycle_values(interaction_flow_size, nrow(ints))
    ints$label_color <- recycle_values(interaction_flow_label_color, nrow(ints))
    ints$label_size <- recycle_values(interaction_flow_label_size, nrow(ints))

    exts <- subset(flows, type == "external")
    exts$color <- recycle_values(external_flow_color, nrow(exts))
    if(is.numeric(external_flow_linetype)){
      external_flow_linetype <- subset(ltys, code == external_flow_linetype)[,"text"]
    }
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
  }

  # get the ggplot2 code as text
  code <- get_code()

  # evaluate the ggplot2 code using current environment args
  theplot <- eval(parse(text = code))

  return(theplot)
}
