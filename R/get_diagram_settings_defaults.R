#' Internal function to store default diagram settings
#'
#' @noRd

get_diagram_settings_defaults <- function() {
  defaults <- list(
    label_flows = TRUE,
    external_flows = TRUE,
    interaction_label = TRUE,
    with_grid = FALSE,
    node_outline_color = "black",
    node_fill_color = "white",
    node_text_color = "black",
    node_text_size = 8,
    flow_text_color = "black",
    flow_text_size = 3,
    main_arrow_color = "black",
    main_arrow_linetype = "solid",
    main_arrow_size = 0.5,
    interaction_arrow_color = "black",
    interaction_arrow_linetype = "dashed",
    interaction_arrow_size = 0.5)

  return(defaults)
}
