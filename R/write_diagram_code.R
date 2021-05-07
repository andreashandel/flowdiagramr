#' Generate a reproducible R script to make the diagram.
#'
#' @description
#' Generates code in the form of a stand-alone R script to reproduce a diagram,
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
#' @param make_diagram_settings A named list of diagram aesthetics. See
#'    \code{\link{make_diagram}} documentation. Default is `NULL` and the
#'    default values from \code{\link{make_diagram}} are used.
#' @param directory File directory in which to save the R file. Defualt
#'     location is the current working directory.
#' @param filename Name of the file, must end in '.R'. Default name is
#'     'diagram_code.R'.
#' @return A message telling the user where the file is.
#' @import fs
#' @export


write_diagram_code <- function(model_list = NULL,
                               diagram_list = NULL,
                               make_diagram_settings = NULL,
                               directory = NULL,
                               filename = NULL)
{

  # make sure at least one of model_list or diagram_list is provided
  if(is.null(model_list) & is.null(diagram_list)) {
    stop("Please provide at least one of the model list or the diagram list as input")
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
  defaults <- eval(formals(make_diagram)$diagram_settings)

  if(!is.null(make_diagram_settings)) {
    defaults[names(make_diagram_settings)] <- make_diagram_settings
  }
  args <- defaults

  # Make a character vector to hold all the aes assignments
  args_block <- character(length(args))
  for(i in 1:length(args_block)) {
    argtext <- args[[i]]
    if(is.logical(argtext) | is.numeric(argtext)) {
      # if logical or numeric, does not need quotes
      args_block[i] <- paste0(names(args)[[i]], " <- ", args[[i]])
    } else {
      # otherwise the argument is a string and needs quotes
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
  # are collapsed with line breaks.

  if(!is.null(model_list)) {
    input_block <- paste("model_list <-", deparse1(model_list))

    if(is.null(diagram_list)) {
      prep_block <- "diagram_list <- prepare_diagram(model_list = model_list)"
      unlist_block <- paste("nodes <- diagram_list$nodes",
                            "horizontal_edges <- diagram_list$horizontal_edges",
                            "vertical_edges <- diagram_list$vertical_edges",
                            "curved_edges <- diagram_list$curved_edges",
                            "feedback_edges <- diagram_list$feedback_edges",
                            sep = "\n")
    } else {
      msg <- paste0("# Since a user-supplied diagram_list is provided,\n",
                    "# the default one created by prepare_diagram() is not used")
      prep_block <- paste(msg,
                          "# diagram_list <- prepare_diagram(model_list = model_list)",
                          sep = "\n")
      unlist_block <- paste("# nodes <- diagram_list$nodes",
                            "# horizontal_edges <- diagram_list$horizontal_edges",
                            "# vertical_edges <- diagram_list$vertical_edges",
                            "# curved_edges <- diagram_list$curved_edges",
                            "# feedback_edges <- diagram_list$feedback_edges",
                            sep = "\n")
    }
  }

  if(!is.null(diagram_list)) {
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
      dftmp <- paste0(start, "\n  ", paste(dtmp, collapse = ",\n  "), "\n", end)
      df_block[i] <- dftmp
    }

    df_block <- paste(df_block, collapse = "\n\n")
  }

  # Entire script
  if(!is.null(model_list) & is.null(diagram_list)) {
    # if just the model_list is provided, include the list prepping blocks
    outcode <- paste(lib_block,
                     input_block,
                     prep_block,
                     unlist_block,
                     args_block,
                     gg_block,
                     print_block, sep = "\n\n")
  } else if(is.null(model_list) & !is.null(diagram_list)) {
    # If just the diagram_list is provided, just include the data frames blocks
    outcode <- paste(lib_block,
                     df_block,
                     args_block,
                     gg_block,
                     print_block, sep = "\n\n")
  } else {
    # If both are provided, return all blocks
    outcode <- paste(lib_block,
                     input_block,
                     prep_block,
                     unlist_block,
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
  message <- paste("Your file was saved here:", fs::path_real(outfile))
  return(message)
}
