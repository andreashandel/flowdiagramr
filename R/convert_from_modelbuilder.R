#' Convert modelbuilder list object to list for modeldiagram
#'
#' @description
#' `convert_from_modelbuilder()` takes a model, stored in an Rds file,
#' that was built with the **modelbuilder** package, and
#' converts it to the format used by **flowdiagramr**.
#'
#' @param mbmodel A **modelbuilder** model object.
#'
#' @return A list object consisting of the two components used by
#' \code{\link{prepare_diagram}} function. The list contains two elements:
#' \itemize{
#'   \item{\code{model_list}}: A list containing variable labels and flows.
#'   This has the format needed for the first argument of \code{\link{prepare_diagram}}.
#'   single uppercase characters. Must start with uppercase.
#'   \item{\code{model_settings}}: A list containing the variable names.
#'   Also sets \code{use_varnames = TRUE}. This has the format needed
#'   for the second, optional argument of \code{\link{prepare_diagram}}.
#' }
#'
#' @examples
#' \dontrun{
#' #loading a model file that was made with **modelbuilder**
#' mbmodel <- readRDS('SIR_model.Rds')
#' mbmodel_structure <- convert_from_modelbuilder(mbmodel)
#' }

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

  model_list <- list(variables = varlabels, flows = flows)
  return(model_list)
}
