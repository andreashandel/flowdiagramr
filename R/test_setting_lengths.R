#' Test whether settings are the correct length.
#'
#' @description Helper function called within update_settings to test
#' whether the number of values provided for each setting are either
#' of length 1 or the length of the flows or variables.
#'
#' @param diagram_settings The diagram_settings list.
#' @param names The relevant list names for the test.
#' @param n The number of values allowed, other than 1.
#' @return A logical.
#' @export

test_setting_lengths <- function(diagram_settings, names, n) {
  msg <- NULL
  if(length(grep("var_|main_|external_|interaction_", names)) > 0) {
    for(dovar in names) {
      tmp <- diagram_settings[[dovar]]
      if(!length(tmp) %in% c(1, n)) {
        msg <- paste("Length of", dovar, "must be either 1 or", n)
      }
    }
  } else {
    for(dovar in names) {
      tmp <- diagram_settings[[dovar]]
      if(!length(tmp) %in% c(n)) {
        msg <- paste("Length of", dovar, "must be ", n)
      }
    }
  }

  return(msg)
}
