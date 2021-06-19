#' Checks for coherence between the varlocation matrix and the input list.
#'
#' @param input_list A list.
#' @param varlocation_matrix A matrix
#' @noRd

check_varlocation_matrix <- function(input_list, varlocation_matrix) {
  ret_message <- paste0("The model_list object contains variable (nodes) not ",
                        "contained in the varlocation matrix, or vice versa.")

  in_vars <- sort(input_list$varlabels)
  mat_vars <- sort(nodes_matrix[nodes_matrix != ""])
  flag <- isTRUE(all.equal.character(in_vars, mat_vars))
  if(flag == FALSE) {
    stop(ret_message)
  }
}
