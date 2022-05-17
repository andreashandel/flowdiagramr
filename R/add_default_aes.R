#' Add default aestethic settings to the variables and flows data frames.
#' Helper function called by prepare_diagram
#'
#' @param variables The variables data frame.
#' @param flows The flows data frame.
#' @return A list of the two data frames.
#' @export

add_default_aes <- function(variables, flows) {

  # the default settings for variables
  variables$outline_color <- "black"
  variables$fill_color <- "#6aa4c8"
  variables$label_text <- variables$name
  variables$label_color <- "white"
  variables$label_size <- 10

  # the default settings for flows
  flows$line_color <- "grey25"
  flows$line_size <- 0.7
  flows$linetype <- ifelse(flows$type == "interaction", "longdash", "solid")
  flows$linetype <- ifelse(flows$type == "external", "dotted", flows$linetype)
  flows$label_text <- flows$name
  flows$name <- flows$orig_name
  flows$orig_name <- NULL
  flows$label_color <- "black"
  flows$label_size <- 5
  flows$show_label <- TRUE
  flows$arrow_size <- 0.25
  flows$show_arrow <- TRUE

  return(list(variables = variables,
              flows = flows))
}
