#' Add explicit plus signs to flows. Used by prepare_diagram.
#'
#' @param flows A list of flows. Each list element is a vector of flows for a specific variable.
#' @return The same list of flows that was sent into the function, but with explicit plus signs in front of positive flow terms.
#' @noRd

add_plus_signs <- function(flows) {
  for(i in 1:length(flows)) {
    tmp <- flows[[i]] #pull out vector of flows for each variable

    for(j in 1:length(tmp)) {
      fl <- tmp[j]
      pattern = "(\\+|-).*"  #find first plus or minus sign in string
      replacement = "\\1"  #the first occurrence of the pattern
      si <- gsub(pattern = pattern, replacement = replacement, fl) #pull out first + or - sign from string

      # if the start of the string is not plus or minus, make a plus
      # note that this assumes all minuses are explicit
      if(!(si %in% c("+", "-"))) {
        si <- "+"
      } else {
        si <- ""  # if it is a plus or minus, don't add anything in front
      }

      # add the sign (or empty string) to the flow
      flows[[i]][j] <- paste0(si, fl)
    }  # end variable i, flow j
  }  # end variable i flows

  return(flows)
}
