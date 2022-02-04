#' Updates specified columns in a data frame with new values from a list.
#'
#' @description Helper function that updates all settings columns in a
#' flows or variables data frame based on the new values in diagram_settings.
#' @param n The number of variables or flows.
#' @param diagram_settings The diagram_settings list.
#' @param list_names The names of the list elements relevant for the current operation.
#' @param column_names The column names that map to each list name in diagram_settings.
#'     Must be in the correct order.

make_new_settings_df <- function(n,
                                 diagram_settings,
                                 list_names,
                                 column_names) {
  seq_to_max <- 1:n
  var_mat <- sapply(diagram_settings[list_names], "[", i = seq_to_max)
  if(is.vector(var_mat)) {
    var_mat <- t(as.matrix(var_mat))
  }
  var_df <- as.data.frame(var_mat)
  new_var_settings <- fill_down_rows(var_df)
  colnames(new_var_settings) <- column_names
  return(new_var_settings)
}
