#' Add implicit plus signs to flows.
#'
#' @param flows A list of flows.
#' @return A list of flows with explicit plus signs.
#' @noRd

add_plus_signs <- function(flows) {
  for(i in 1:length(flows)) {
    tmp <- flows[[i]]

    for(j in 1:length(tmp)) {
      f <- tmp[j]
      s <- gsub("(\\+|-).*","\\1",f)

      # if the start of the string is not plus or minus, make a plus
      # note that this assumes all minuses are explicit
      if(!(s %in% c("+", "-"))) {
        s <- "+"
      } else {
        s <- ""  # if it is a plus or minus, make this add-on blank
      }

      # add the sign (or blank string) to the flow
      flows[[i]][j] <- paste0(s, f)
    }  # end variable i, flow j
  }  # end variable i flows

  return(flows)
}
