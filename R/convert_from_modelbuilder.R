#' Convert modelbuilder list object to list for modeldiagram
#'
#' @description
#' `convert_from_modelbuilder()` takes a **modelbuilder** model object and
#' converts it to the list format used by **flowdiagramr**.
#'
#' @param mbmodel A **modelbuilder** model object.
#'
#' @return A list object that can be used as input for the
#' \code{\link{prepare_diagram}} function. The list contains three elements:
#' \itemize{
#'   \item{\code{varlabels}}: A vector of variable labels. Typically
#'   single uppercase characters. Must start with uppercase.
#'   \item{\code{varnames}}: A vector of variables names. Full names
#'   that correspond with the variable labels.
#'   \item{\code{flows}}: A list of flows. The elements are vectors of
#'   flows into and out of each variable.
#' }
#'
#' @export

convert_from_modelbuilder <- function(mbmodel) {
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
