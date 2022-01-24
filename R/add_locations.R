#' Add x,y location information
#' Helper function called by prepare_diagram.
#' Likely not useful for direct calling.
#'
#' @param vardf The nodes (variables) data frame.
#' @param varlocations The varlocations matrix.
#' @param varbox_x_size Vector of box width for each variable.
#' @param varbox_y_size Vector for box height for each variable.
#' @param varspace_x_size Vector for horizontal spacing.
#' @param varspace_y_size Vector for vertical spacing.
#' @return The original variables data frame with location information added.
#' @details `varlocations` needs to be a matrix.
#'          `varbox_x_size` and `varbox_y_size` need to be vectors
#'          of length corresponding number of variables.
#'          `varspace_x_size` and `varspace_y_size` need to be vectors
#'          of length corresponding to cols/rows in varlocations minus 1.
#'          `prepare_diagram` ensures inputs are provided in the required form.
#' @export

add_locations <- function(vardf, varlocations, varbox_x_size,
                          varbox_y_size, varspace_x_size,
                          varspace_y_size) {

  # get number of variables that will be plotted
  nvars <- length(which(vardf$name != ""))  # number of variables with a name


  # create the locations
  num_rows <- nrow(varlocations)  # number of rows for the grid
  num_cols <- ncol(varlocations)  # number of columns for the grid
  positions <- 1:(num_rows*num_cols)  # total number of positions needed
  ids <- varlocations[positions]  # matrix element ids associated with the positions on the grid

  # make row ids, which start in bottom left so we invert the row id integers
  # such that they are counting up, e.g., the first row of varlocations is
  # defined in on our grid as the upper most row
  rids <- rep(num_rows:1, length(positions)/num_rows)

  # make column ids, these go left to right, so in typical numeric order
  cids <- rep(1:num_cols, each = length(positions)/num_cols)
  idmap <- data.frame(row_id = rids,
                      col_id = cids,
                      pos = positions,
                      name = ids)

  newvariables <- list()  # create an empty list to store the grid coordinates

  # the process is:
  #   1. Loop over row ids and then column ids
  #   2. Starting location is 0,0 on the grid for the bottom-left corner of first "box"
  #   3. Create a temporary dataframe containg the x and y min,max
  #      locations for each box based on the start positiions.
  #   4. Bind the coordinates dataframe
  #   5. Iteratively updates y starts (each row) and x starts (each column)
  for(ri in sort(unique(rids))){
    ystart <- (ri-1) + varspace_y_size[ri]  # update y start position based on row id
    xstart <- 0  # xstart is always zero for the first column in a row
    for(ci in sort(unique(cids))) {
      tmp <- data.frame(row_id = ri,
                        col_id = ci,
                        xmin = NA,
                        xmax = NA,
                        ymin = NA,
                        ymax = NA)
      tmp$xmin <- xstart
      tmp$xmax <- xstart + varbox_x_size[ci]
      tmp$ymin <- ystart
      tmp$ymax <- ystart + varbox_y_size[ri]
      xstart <-  tmp$xmax + varspace_x_size[ci]
      newvariables <- rbind(newvariables, tmp)
    } #end loop over columns
  } #end loop over rows

  # now we map the variable names to the coordinates based on row
  # and column ids
  tmpvars <- subset(idmap, name != "")  # ignore empty boxes on the grid
  # merge by row and column ids
  tmpvars <- merge(newvariables, tmpvars, by = c("row_id", "col_id"))

  # subset to just the columns of interest for further processing and output
  tmpvars <- tmpvars[, c("name", "xmin", "xmax", "ymin", "ymax")]

  # now merge into the variables dataframe
  vardf <- merge(vardf, tmpvars, by = "name", all.x = TRUE)

  # calculate midpoints for label locations
  # simple means for now that get updated as needed with further processing
  vardf$xlabel <- rowMeans(vardf[ , c("xmin", "xmax")])
  vardf$ylabel <- rowMeans(vardf[ , c("ymin", "ymax")])

  # order by the assigned numeric ids
  vardf <- vardf[order(vardf$id), ]
  return(vardf)  # return the variables data frame for further processing
}
