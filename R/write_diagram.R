#' Write the ggplot2 code to an R file.
#'
#' @param directory File directory in which to save the R file. Defualt
#'     location is the current working directory.
#' @param filename Name of the file, must end in '.R'. Default name is
#'     'diagram_ggplot_code.R'.
#' @return A message telling the user where the file is.
#' @export


write_diagram <- function(directory = NULL,
                               filename = NULL) {

  # save to current working directory if the not specified
  if(is.null(directory)) {
    directory <- getwd()
  }

  # give the file a generic name
  if(is.null(filename)) {
    filename <- "diagram_ggplot_code.R"
  }

  # create the full path output directory
  outfile <- paste0(directory, "/", filename)

  # read in the code from internal function that just
  # returns the character object.
  code <- get_code()

  # write the code to file
  cat(code, file = outfile)

  # report the file location on return
  message <- paste("Your file was saved here:", normalizePath(outfile))
  return(message)
}
