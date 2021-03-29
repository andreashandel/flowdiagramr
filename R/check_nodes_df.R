#' Checks for coherence between the node data frame and the input list.
#'
#' @param input_list A list.
#' @param nodes_df A data frame.
#' @noRd

check_nodes_df <- function(input_list, nodes_df) {
  ret_message <- paste0("The inputs list contains variable (nodes) not ",
                        "contained in the nodes data frame, or vice versa.")

  in_vars <- sort(input_list$varlabels)
  df_vars <- sort(nodes_df$label)
  flag <- isTRUE(all.equal.character(in_vars, df_vars))
  if(flag == FALSE) {
    stop(ret_message)
  }
}
