#' Convert modelbuilder list object to list for modeldiagram
#'
#' @param mbmodel A modelbuilder object.
#' @return A list.
#' @export

make_diagram_inputs <- function(mbmodel) {
  m <- mbmodel  # reassign for easy coding/typing

  # extract variable labels and names
  varlabels <- unlist(lapply(m$var, "[[", 1))  # labels
  varnames <- unlist(lapply(m$var, "[[", 2))  # names

  # extract flows to/from each variable
  flows <- list()  # empty storage list
  for(i in 1:length(m$var)) {
    f <- m$var[[i]]$flows  # flows for variable i
    fname <- paste0(varlabels[i], "_flows")  # flow label for variable i
    flows[[fname]] <- f  # store in the list
  }

  # return a list
  return(
    list(varlabels = varlabels, varnames = varnames, flows = flows)
  )

}
