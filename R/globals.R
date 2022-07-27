#' @title Define some global variables to make CRAN checks happy
#'
#' @description define global variables
#' @noRd


# adding an empty function to stop roxygen from complaining
globals <- function() {

}

#this bit of code is needed to prevent NOTE messages on CRAN checks
#most of those are from the ggplot commands
utils::globalVariables( c("external_flow_color", "external_flow_label_color", "external_flow_label_on", "external_flow_label_size", "external_flow_linetype", "external_flow_on", "external_flow_size", "from", "gpar", "grid.circle", "grid.points", "id", "interaction_flow_color", "interaction_flow_label_color", "interaction_flow_label_on", "interaction_flow_label_size", "interaction_flow_linetype", "interaction_flow_on", "interaction_flow_size", "label", "linkfrom", "linkto", "main_flow_color", "main_flow_label_color", "main_flow_label_on",
    "main_flow_label_size", "main_flow_linetype", "main_flow_on", "main_flow_size", "out_interaction", "to", "type", "var_fill_color", "var_label_color", "var_outline_color", "x", "xmid", "y", "ymid", "direct_interaction", "name", "label_text", "xlabel", "xmax", "xmin", "ylabel", "ymax", "ymin", "xend", "xstart", "yend", "ystart", "allspace", "orig_name"))
