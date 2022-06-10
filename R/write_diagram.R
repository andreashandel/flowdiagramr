#' Generate a reproducible R script to make the diagram.
#'
#' @description
#' `write_diagram()` generates code in the form of a stand-alone R script to
#' produce a diagram. By editing the generated code, the user can
#' make manual adjustments to the diagram.
#'
#' @param diagram_list A **flowdiagramr** input structure, resulting from
#'     a call to \code{\link{prepare_diagram}}. See `Details` below.
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


write_diagram <- function(diagram_list = NULL,
                          directory = "./",
                          filename = "diagram_code.R",
                          always_overwrite = FALSE
                          )
{

  # make sure at least one of model_list or diagram_list is provided
  if(is.null(diagram_list)) {
    stop("The diagram list is a required input and was not provided.")
  }

  # create a text block that loads libraries
  lib_block <- paste("## load libraries ----",
                     "library(ggplot2)",
                     "library(flowdiagramr)",
                     sep = "\n")

  # create a text block that makes the data frames from diagram_list
  df_block <- 2  # this is always 2, one for variables and one for flows
  for(i in 1:2) {
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

  # create a text block of the ggplot2 code
  gg_block <- ' ## ggplot2 code ----
###
# make the diagram with ggplot2
###
# Start with an empty ggplot2 canvas. The coord_equal function ensures
# that the x and y coordinates are displayed in equal proportions to
# on another (that is, it makes sure that the squares look like squares).
# All layers are added sequentially onto this blank canvas.
diagram_plot <- ggplot() +
  coord_equal(clip = "off")


# LAYER 1: STATE VARIABLES
# plot the states variable nodes as rectangles

# The variables data frame is used to create rectangles, with size determined
# by the xmin, xmax, ymin, and ymax values in the nodes data frame. The
# outline color of the rectangles is defined by var_outline_color; the
# inside color (fill) of the rectangles is defined by var_fill_color.
# The color variables can be a single value or a vector, giving different
# colors to different rectangles/nodes/state variables. If a vector, the
# color and fill vectors must have a length that is equal to the number
# of rows in the nodes data frame (one value for each row).

# create the nodes/boxes/variables
# these are just empty rectangles with no text
for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +  # add new stuff to blank canvas
    geom_rect(
      data = variables[i, ],  # one row of the data frame
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),  # location information
      color = variables[i, "outline_color"],  # border color
      fill = variables[i, "fill_color"]  # internal, fill color
    )
}

# add label text, which goes on top of boxes based on location information
for(i in 1:nrow(variables)) {
  diagram_plot <- diagram_plot +  # add text to boxes
    geom_text(
      data = variables[i, ],
      aes(x = xlabel, y = ylabel, label = label_text),
      size = variables[i, "label_size"],
      color = variables[i, "label_color"]
    )
}

## add in all the flows
# start with the lines/arrows
for(i in 1:nrow(flows)) {
  if(flows[i, "show_arrow"] == TRUE) {
    diagram_plot <- diagram_plot +  # add the lines to the plot with boxes
      geom_curve(  # always use geom_curve, which is straight when cuvature = 1
        data = flows[i, ],
        aes(x = xstart,
            y = ystart,
            xend = xend,
            yend = yend),
        linetype = flows[i, "line_type"],
        arrow = arrow(length = unit(flows[i, "arrow_size"],"cm"), type = "closed"),
        color = flows[i, "line_color"],
        arrow.fill = flows[i, "line_color"],
        lineend = "round",
        size = flows[i, "line_size"],
        curvature = flows[i, "curvature"],
        ncp = 1000  # controls smoothness of curve, larger number = more smooth
      )
  }
}

for(i in 1:nrow(flows)) {
  if(flows[i, "show_label"] == TRUE) {
    diagram_plot <- diagram_plot +  # now add the flow labels to the canvas
      geom_text(
        data = flows[i, ],
        aes(x = xlabel, y = ylabel, label = label_text),
        size = flows[i, "label_size"],
        color = flows[i, "label_color"])
  }
}

# If with_grid == FALSE (default) then void out the theme
# otherwise keep the grey background with grid
# the grid can be useful for updating positions of items
with_grid <- FALSE  # default is false
if(with_grid == FALSE) {
  diagram_plot <- diagram_plot +
    theme_void()  # makes an empty plot theme with no axes, grids, or ticks
} else {
  # The else here may seem silly, but otherwise the returned plot is NULL
  diagram_plot <- diagram_plot  # just returns default ggplot2 theme
}
  '

  # Plotting and saving block ---
  plot_save_block <- "# These lines plot or save the generated diagram. \n# Uncomment them if you want to perform either action. \n# plot(diagram_plot) \n# ggsave('diagram_plot.png',diagram_plot)"


  # Concatenate all blocks to generate a stand-alone script
  outcode <- paste(
    lib_block,
    df_block,
    gg_block,
    plot_save_block,
    sep = "\n\n\n"
  )


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
