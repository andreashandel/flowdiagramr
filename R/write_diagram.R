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
                          filename = "diagram_code.R",
                          always_overwrite = FALSE
                          )
{

  # make sure at least one of model_list or diagram_list is provided
  if(is.null(model_list) & is.null(diagram_list)) {
    stop("Please provide at least one of the model list or the diagram list as input")
  }

  # The R script for writing out is built as a series of blocks
  # that are concatenated at the very end of the function.

  # Linetypes block
  lty_block <- paste(
    "# setup linetypes mapping from numeric to text",
    'ltys <- data.frame(code = 0:6,
                   text = c("blank", "solid", "dashed",
                            "dotted", "dotdash", "longdash",
                            "twodash"))',
    sep = "\n")

  # Load libraries block ---
  lib_block <- paste0("library(ggplot2)",
                      "\n",
                      "library(flowdiagramr)")

  # Get model settings
  mod_defs <- eval(formals(prepare_diagram)$model_settings)
  if(!is.null(model_settings)) {
    mod_defs[names(model_settings)] <- model_settings
  }
  model_settings <- mod_defs



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
  var_rec_block <- paste(
    "# recycle values as needed",
    "variables$color <- flowdiagramr:::recycle_values(var_outline_color, nrow(variables))",
    "variables$fill <- flowdiagramr:::recycle_values(var_fill_color, nrow(variables))",
    "variables$label_color <- flowdiagramr:::recycle_values(var_label_color, nrow(variables))",
    "variables$label_size <- flowdiagramr:::recycle_values(var_label_size, nrow(variables))",
    "variables$plot_label_size <- NULL",
    sep = "\n"
  )

  main_rec_block <- paste(
    'mains <- subset(flows, type == "main")',
    "mains$color <- flowdiagramr:::recycle_values(main_flow_color, nrow(mains))",
    "if(is.numeric(main_flow_linetype)) {",
      '  main_flow_linetype <- subset(ltys, code == main_flow_linetype)[,"text"]',
    "}",
    "mains$linetype <- flowdiagramr:::recycle_values(main_flow_linetype, nrow(mains))",
    "mains$size <- flowdiagramr:::recycle_values(main_flow_size, nrow(mains))",
    "mains$label_color <- flowdiagramr:::recycle_values(main_flow_label_color, nrow(mains))",
    "mains$label_size <- flowdiagramr:::recycle_values(main_flow_label_size, nrow(mains))",
    sep = "\n"
  )

  ints_rec_block <- paste(
    'ints <- subset(flows, type == "interaction")',
    "ints$color <- flowdiagramr:::recycle_values(interaction_flow_color, nrow(ints))",
    "if(is.numeric(interaction_flow_linetype)) {",
    '  interaction_flow_linetype <- subset(ltys, code == interaction_flow_linetype)[,"text"]',
    "}",
    "ints$linetype <- flowdiagramr:::recycle_values(interaction_flow_linetype, nrow(ints))",
    "ints$size <- flowdiagramr:::recycle_values(interaction_flow_size, nrow(ints))",
    "ints$label_color <- flowdiagramr:::recycle_values(interaction_flow_label_color, nrow(ints))",
    "ints$label_size <- flowdiagramr:::recycle_values(interaction_flow_label_size, nrow(ints))",
    sep = "\n"
  )

  exts_rec_block <- paste(
    'exts <- subset(flows, type == "external")',
    'exts$color <- flowdiagramr:::recycle_values(external_flow_color, nrow(exts))',
    'if(is.numeric(external_flow_linetype)){',
      '  external_flow_linetype <- subset(ltys, code == external_flow_linetype)[,"text"]',
    '}',
    "exts$linetype <- flowdiagramr:::recycle_values(external_flow_linetype, nrow(exts))",
    "exts$size <- flowdiagramr:::recycle_values(external_flow_size, nrow(exts))",
    "exts$label_color <- flowdiagramr:::recycle_values(external_flow_label_color, nrow(exts))",
    "exts$label_size <- flowdiagramr:::recycle_values(external_flow_label_size, nrow(exts))",
    sep = "\n"
  )

  # Final aesthetics block
  final_aes <- paste(
    "# recombine flows data frame with aesthetics as columns",
   " flows <- rbind(mains, ints, exts)",
    "flows$arrowsize <- 0.25  # default arrow size",
    "\n",
    "# turn off flows completely by setting linetype to blank as needed",
    "if(main_flow_on == FALSE) {",
      '  flows[flows$type == "main", "linetype"] <- "blank"',
      '  flows[flows$type == "main", "arrowsize"] <- 0',
    "}",
    "if(interaction_flow_on == FALSE) {",
      '  flows[flows$type == "interaction", "linetype"] <- "blank"',
      '  flows[flows$type == "interaction", "arrowsize"] <- 0',
    "}",
    "if(external_flow_on == FALSE) {",
      ' flows[flows$type == "external", "linetype"] <- "blank"',
      ' flows[flows$type == "external", "arrowsize"] <- 0',
    "}",
    "\n",
    '# set label to "" to suppress label if requested',
    '# also do not show label if the flow itself is turned off',
    "flows$math <- flows$label",
    "if(main_flow_on == FALSE || main_flow_label_on == FALSE) {",
      '  flows[flows$type == "main", "label"] <- ""',
    "}",
    "if(interaction_flow_on == FALSE || interaction_flow_label_on == FALSE) {",
      '  flows[flows$type == "interaction", "label"] <- ""',
    "}",
    "if(external_flow_on == FALSE || external_flow_label_on == FALSE) {",
      '  flows[flows$type == "external", "label"] <- ""',
    "}",
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
        dtmp[j] <- paste(cname, "=", deparse1(tmp[ , j]))
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
                     lty_block,
                     args_block,
                     var_rec_block,
                     main_rec_block,
                     ints_rec_block,
                     exts_rec_block,
                     final_aes,
                     gg_block,
                     plot_save_block,
                     sep = "\n\n")
  } else if(is.null(model_list) & !is.null(diagram_list)) {
    # If just the diagram_list is provided, just include the data frames blocks
    outcode <- paste(lib_block,
                     df_block,
                     lty_block,
                     args_block,
                     var_rec_block,
                     main_rec_block,
                     ints_rec_block,
                     exts_rec_block,
                     final_aes,
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
                     lty_block,
                     args_block,
                     var_rec_block,
                     main_rec_block,
                     ints_rec_block,
                     exts_rec_block,
                     final_aes,
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
