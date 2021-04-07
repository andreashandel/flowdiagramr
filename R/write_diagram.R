#' Generate a reproducible R script to make the diagram.
#'
#' @description
#' `write_diagram()` makes a stand-alone R script to reproduce a diagram,
#' including the model inputs, the data frames from
#' \code{\link{prepare_diagram}}, and the **ggplot2** code.
#'
#' @param input_list A **flowdiagramr** input list. See
#'     \code{\link{prepare_diagram}}. If `input_list` is provided, then
#'     `input_structure` cannot be provided.
#' @param input_structure A **flowdiagramr** input structure, resulting from
#'     a call to \code{\link{prepare_diagram}}. If `input_structure` is
#'     provided, then `input_list` cannot be provided.
#' @param directory File directory in which to save the R file. Defualt
#'     location is the current working directory.
#' @param filename Name of the file, must end in '.R'. Default name is
#'     'diagram_ggplot_code.R'.
#' @return A message telling the user where the file is.
#' @import fs
#' @export


write_diagram <- function(input_list = NULL,
                          input_structure = NULL,
                          directory = NULL,
                          filename = NULL) {

  # save to current working directory if the not specified
  if(is.null(directory)) {
    directory <- getwd()
  }

  # give the file a generic name
  if(is.null(filename)) {
    filename <- "diagram_code.R"
  }

  lib_block <- "library(ggplot2) \n library(flowdiagramr)"

  # Get graphing arguments
  args <- as.list(formals(make_diagram))
  args <- args[2:length(args)]

  args_block <- character(length(args))
  for(i in 1:length(args_block)) {
    args_block[i] <- paste(names(args)[[i]], "<-", args[[i]])
  }

  args_block <- paste(args_block, collapse = "\n")


  gg_block <- get_code()

  print_block <- "print(outplot)"

  # generate code block for input_list, of provided
  if(!is.null(input_list)) {
    input_block <- paste("input_list <-", deparse1(input_list))
    prep_block <- "input_structure <- prepare_diagram(input_list)"
    unlist_block <- paste("nodes <- input_structure$nodes",
                          "horizontal_edges <- input_structure$horizontal_edges",
                          "vertical_edges <- input_structure$vertical_edges",
                          "curved_edges <- input_structure$curved_edges",
                          "feedback_edges <- input_structure$feedback_edges",
                          sep = "\n")

    outcode <- paste(lib_block,
                     input_block,
                     prep_block,
                     unlist_block,
                     args_block,
                     gg_block,
                     print_block, sep = "\n\n")

  } else {
    df_block <- character(length(input_structure))
    for(i in 1:length(input_structure)) {
      dfname <- names(input_structure)[i]
      start <- paste(dfname, "<- data.frame(")
      end <- ")"
      tmp <- input_structure[[i]]
      dtmp <- character(length(ncol(tmp)))
      for(j in 1:ncol(tmp)) {
        cname <- colnames(tmp)[j]
        dtmp[j] <- paste(cname, "=", deparse(tmp[ , j]))
      }
      dftmp <- paste(start, "\n", paste(dtmp, collapse = ",\n"), "\n", end)
      df_block[i] <- dftmp
    }

    df_block <- paste(df_block, collapse = "\n")

    outcode <- paste(lib_block,
                     df_block,
                     args_block,
                     gg_block,
                     print_block, sep = "\n\n")
  }

  # create the full path output directory
  outfile <- paste0(directory, "/", filename)

  # write the code to file
  cat(outcode, file = outfile)

  # report the file location on return
  message <- paste("Your file was saved here:", path_real(outfile))
  return(message)
}
