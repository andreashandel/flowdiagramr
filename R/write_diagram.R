#' Generate a reproducible R script to make the diagram.
#'
#' @description
#' `write_diagram()` generates code in the form of a stand-alone R script to
#' produce a diagram. By editing the generated code, the user can
#' make manual adjustments to the diagram.
#'
#' @param model_list A **flowdiagramr** input list. See
#'     \code{\link{prepare_diagram}}. Also see `Details` below.
#' @param model_settings A named list of model settings.  See
#'     \code{\link{prepare_diagram}}. Also see `Details` below.
#' @param diagram_list A **flowdiagramr** input structure, resulting from
#'     a call to \code{\link{prepare_diagram}}. See `Details` below.
#' @param diagram_settings A named list of diagram aesthetics. See
#'    \code{\link{make_diagram}} documentation. Default is `NULL` and the
#'    default values from \code{\link{make_diagram}} are used.
#' @param directory File directory in which to save the produced R file.
#'     Default location is the current working directory.
#' @param filename Name of the file, must end in '.R'. Default name is
#'     'diagram_code.R'.
#' @param always_overwrite A logical indicating if you want to skip being asked
#' if you want to overwrite an already existing file.
#' Default is FALSE. Change to TRUE at own risk.
#' @return R code written to a file as specified by settings.
#' Also, a message is returned telling the user where the file is.
#' @details You need to supply at least one of `model_list`
#' or `diagram_list`. If you supply both, `model_list` is included in the
#' resulting R file, but it is not used. Including it can be good just so
#' you have the complete model specification in one script.
#' @import fs
#' @importFrom utils menu
#' @export
#'
#' @examples
#' \dontrun{
#' varlabels <- c("S","I","R")
#' varnames <- c("Susceptible","Infected","Recovered")  # optional
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' varlocations <-  matrix(data = c("S", "", "R", "", "I", "" ),
#'                         nrow = 2, ncol = 3, byrow = TRUE)
#' model_list <- list(varlabels = varlabels, varnames = varnames,
#' flows = flows, varlocations = varlocations)
#' diagram_list <- prepare_diagram(model_list = model_list)
#'
#' # generate R code from model_list
#' write_diagram(model_list = model_list)
#'
#' # generate R code from diagram_list
#' write_diagram(diagram_list = diagram_list)
#'
#' #' # generate R code from both
#' write_diagram(model_list = model_list, diagram_list = diagram_list)
#' }


write_diagram <- function(model_list = NULL,
                          model_settings = NULL,
                          diagram_list = NULL,
                          diagram_settings = NULL,
                          directory = "./",
                          filename = 'diagram_code.R',
                          always_overwrite = FALSE
                          )
{

  # make sure at least one of model_list or diagram_list is provided
  if(is.null(model_list) & is.null(diagram_list)) {
    stop("Please provide at least one of the model list or the diagram list as input")
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

  if(!is.null(diagram_settings)) {
    defaults[names(diagram_settings)] <- diagram_settings
  }
  args <- defaults

  # Make a character vector to hold all the aes assignments
  args_block <- character(length(args))
  for(i in 1:length(args_block)) {
    argtext <- args[[i]]
    if(length(argtext) > 1) {
      if(is.character(argtext[1])) {
        argtext <- paste0("c('", paste(argtext, collapse = "', '"), "')")
      } else {
        argtext <- paste0("c(", paste(argtext, collapse = ", "), ")")
      }

      args_block[i] <- paste0(names(args)[[i]], " <- ", argtext)

    } else {
      if(is.logical(argtext) | is.numeric(argtext)) {
          # if logical or numeric, does not need quotes
          args_block[i] <- paste0(names(args)[[i]], " <- ", argtext)
        } else {
          # otherwise the argument is a string and needs quotes
          args_block[i] <- paste0(names(args)[[i]], " <- '", argtext, "'")
        }
    }
  }

  # Collapse the aes args block with line breaks
  args_block <- paste(args_block, collapse = "\n")

  # Recycle aesthetics as needed
  rec_block <- paste(
    "var_outline_color <- flowdiagramr:::recycle_values(var_outline_color, nrow(variables))",
    "var_fill_color <- flowdiagramr:::recycle_values(var_fill_color, nrow(variables))",
    "var_label_color <- flowdiagramr:::recycle_values(var_label_color, nrow(variables))",
    "main_flow_label_color <- flowdiagramr:::recycle_values(main_flow_label_color, nrow(flows))",
    sep = "\n"
    )


  # ggplot2 code block ---
  gg_block <- get_code()  # gets the code used by flowdiagramr

  # Plotting and saving block ---
  plot_save_block <- "# These lines plot or save the generated diagram. \n# Uncomment them if you want to perform either action. \n# plot(diagram_plot) \n# ggsave('diagram_plot.png',diagram_plot)"

  # Model structure block ---
  # If model is provided, we simply deparse the list and then make
  # the necessary data frames. This is all stored as text blocks that
  # are collapsed with line breaks.

  if(!is.null(model_list)) {
    input_block <- paste("model_list <-", deparse1(model_list))
    input_settings_block <- paste("model_settings <-", deparse1(model_settings))

    if(is.null(diagram_list)) {
      prep_block <- "diagram_list <- prepare_diagram(model_list = model_list, model_settings = model_settings)"
      unlist_block <- paste("variables <- diagram_list$variables",
                            "flows <- diagram_list$flows",
                            sep = "\n")
    } else {
      msg <- paste0("# Since a user-supplied diagram_list is provided,\n",
                    "# the default one created by prepare_diagram() is not used")
      prep_block <- paste(msg,
                          "# diagram_list <- prepare_diagram(model_list = model_list, model_settings = model_settings)",
                          sep = "\n")
      unlist_block <- paste("# variables <- diagram_list$variables",
                            "# flows <- diagram_list$flows",
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
                     input_settings_block,
                     prep_block,
                     unlist_block,
                     args_block,
                     rec_block,
                     gg_block,
                     plot_save_block,
                     sep = "\n\n")
  } else if(is.null(model_list) & !is.null(diagram_list)) {
    # If just the diagram_list is provided, just include the data frames blocks
    outcode <- paste(lib_block,
                     df_block,
                     args_block,
                     rec_block,
                     gg_block,
                     plot_save_block,
                     sep = "\n\n")
  } else {
    # If both are provided, return all blocks
    outcode <- paste(lib_block,
                     input_block,
                     input_settings_block,
                     prep_block,
                     unlist_block,
                     df_block,
                     args_block,
                     rec_block,
                     gg_block,
                     plot_save_block,
                     sep = "\n\n")
  }



  # create the full path output directory
  outfile <- paste0(directory, "/", filename)

  # check if file exists, if so, ask user whether to overwrite or not
  # if user set always_overwrite to TRUE, ignore the check
  check <- file.exists(outfile)
  if(check == TRUE & always_overwrite != TRUE) {
    ans <- menu(c("Yes", "No"), title = "Specified file already exists. Do you want to overwrite?")
    if(ans == 1) {
      # write the code to file
      cat(outcode, file = outfile)
    } else {
      return("Code not written to file.")
    }
  } else {
    # write the code to file
    cat(outcode, file = outfile)
  }


  # report the file location on return
  message <- paste("Your file was saved here:", fs::path_real(outfile))
  return(message)
}
