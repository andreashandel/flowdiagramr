#' Recycle values to certain length
#'
#' @param x The values to potentially recycle.
#' @param n The desired length (i.e., number of items).
#' @export

recycle_values <- function(x, n) {
  if(length(x) != n) {
    x <- rep_len(x, n)
  }
  return(x)
}
