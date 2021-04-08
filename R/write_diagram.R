#' Generate a reproducible R script to make the diagram.
#'
#' @description
#' `write_diagram()` makes a stand-alone R script to reproduce a diagram,
#' including the model inputs, the data frames from
#' \code{\link{prepare_diagram}}, and the **ggplot2** code. The R script
#' is intended to run "as-is", meaning there is code to make the objects
#' the user sends as arguments (either the `model` or the
#' `diagram_list`).
#'
#' @param model_list A **flowdiagramr** input list. See
#'     \code{\link{prepare_diagram}}. If `model_list` is provided, then
#'     `diagram_list` cannot be provided.
#' @param diagram_list A **flowdiagramr** input structure, resulting from
#'     a call to \code{\link{prepare_diagram}}. If `diagram_list` is
#'     provided, then `model` cannot be provided.
#' @param directory File directory in which to save the R file. Defualt
#'     location is the current working directory.
#' @param filename Name of the file, must end in '.R'. Default name is
#'     'diagram_code.R'.
#' @return A message telling the user where the file is.
#' @import fs
#' @export


write_diagram <- function(model_list = NULL,
                          diagram_list = NULL,
                          directory = NULL,
                          filename = NULL) {

  # make sure only model or diagram_list is provided
  if(!is.null(model_list) & !is.null(diagram_list)) {
    stop(paste0("Please provide either the model or the diagram_list,",
                " but not both."))
  }

  # save to current working directory if the not specified
  if(is.null(directory)) {
    directory <- getwd()
  }

  # give the file a generic name
  if(is.null(filename)) {
    filename <- "diagram_code.R"
  }

  # The R script for writing out is built as a series of blocks
  # that are concatenated at the very end of the function.

  # Load libraries block ---
  lib_block <- paste0("library(ggplot2)",
                      "\n",
                      "library(flowdiagramr)")


  # Graphing aesthetics block ---
  # The ggplot aesthetic arguments need to be defined, here we just
  # pull the defaults from the function and then make them look like code

  # Get graphing arguments
  args <- as.list(formals(make_diagram))
  args <- args[2:length(args)]

  # Make a character vectore to hold all the aes assignments
  args_block <- character(length(args))
  for(i in 1:length(args_block)) {
    argtext <- args[[i]]
    if(is.logical(argtext) | is.numeric(argtext)) {
      args_block[i] <- paste0(names(args)[[i]], " <- ", args[[i]])
    } else {
      args_block[i] <- paste0(names(args)[[i]], " <- '", args[[i]], "'")
    }
  }

  # Collapse the aes args block with line breaks
  args_block <- paste(args_block, collapse = "\n")


  # ggplot2 code block ---
  gg_block <- get_code()  # gets the code used by flowdiagramr


  # Printing block ---
  print_block <- "print(outplot)"


  # Model structure block ---
  # If model is provided, we simply deparse the list and then make
  # the necessary data frames. This is all stored as text blocks that
  # are collapsed with line breaks

  # generate code block for model, of provided
  if(!is.null(model_list)) {
    input_block <- paste("model_list <-", deparse1(model_list))
    prep_block <- "diagram_list <- prepare_diagram(model_list = model_list)"
    unlist_block <- paste("nodes <- diagram_list$nodes",
                          "horizontal_edges <- diagram_list$horizontal_edges",
                          "vertical_edges <- diagram_list$vertical_edges",
                          "curved_edges <- diagram_list$curved_edges",
                          "feedback_edges <- diagram_list$feedback_edges",
                          sep = "\n")

    # Entire script if model provided
    outcode <- paste(lib_block,
                     input_block,
                     prep_block,
                     unlist_block,
                     args_block,
                     gg_block,
                     print_block, sep = "\n\n")

  } else {  # if diagram_list provided, break out the data frames from
            # the list and define them using data.frame().
    df_block <- character(length(diagram_list))
    for(i in 1:length(diagram_list)) {
      dfname <- names(diagram_list)[i]
      start <- paste(dfname, "<- data.frame(")
      end <- ")"
      tmp <- diagram_list[[i]]
      dtmp <- character(length(ncol(tmp)))
      for(j in 1:ncol(tmp)) {
        cname <- colnames(tmp)[j]
        dtmp[j] <- paste(cname, "=", deparse(tmp[ , j]))
      }
      dftmp <- paste(start, "\n", paste(dtmp, collapse = ",\n"), "\n", end)
      df_block[i] <- dftmp
    }

    df_block <- paste(df_block, collapse = "\n")

    # Entire script if diagram_list provided.
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
