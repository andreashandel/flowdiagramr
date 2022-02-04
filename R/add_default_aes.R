#' Add default aestethic settings to the variables and flows data frames.
#' Helper function called by prepare_diagram
#'
#' @param variables The variables data frame.
#' @param flows The flows data frame.
#' @return A list of the two data frames.
#' @export

add_default_aes <- function(variables, flows) {

  # the default settings for variables
  variables$color <- "black"
  variables$fill <- "#6aa4c8"
  variables$label_text <- variables$name
  variables$label_color <- "white"
  variables$label_size <- 10

  # the default settings for flows
  flows$color <- "grey25"
  flows$size <- 0.7
  lines <- data.frame(
    type = c("main", "external", "interaction"),
    linetype = c("solid", "solid", "dashed")
  )
  flows <- merge(flows, lines)
  flows$label_color <- "black"
  flows$label_size <- 5
  flows$arrow_size <- 0.25

  return(list(variables = variables,
              flows = flows))
}
