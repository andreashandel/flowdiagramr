#' Add x,y location informations
#'
#' @param variables The nodes (variables) data frame.
#' @param varlocations The varlocations matrix. Default is NULL.
#' @param varbox_x_scaling Scaler for box width, from `model_settings`.
#' @param varbox_y_scaling Scaler for box height, from `model_settings`.
#' @param varspace_x_scaling Scaler for horizontal spacing, from `model_settings`.
#' @param varspace_y_scaling Scaler for vertical spacing, from `model_settings`.
#' @return The variables data frame with location information.
#' @export

add_locations <- function(variables, varlocations = NULL, varbox_x_scaling,
                          varbox_y_scaling, varspace_x_scaling,
                          varspace_y_scaling) {

  # create a varlocations matrix if the user does not provide one.
  # this simplifies processing by having just one chunk of code that
  # relies on the varlocations matrix and works whether the user
  # provides the varlocations matrix or not.
  if(is.null(varlocations)) {
    # get variable names, defined as variables that have names that are not NA
    varnames <- variables$name[which(!is.na(variables$name))]
    nvars <- length(varnames)  # for defining the number of columns in the matrix
    # create the varlocations matrix, assuming everything is on one row
    varlocations <- matrix(data = varnames, ncol = nvars, nrow = 1)
  }

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
                      label = ids)
  #each row is 3 from the top of the other row: 2 spacing and 1 for size of box
  #this gets scaled by the varspace_y_scaling factor
  rowspace_y <- 3 * varspace_y_scaling  #default spacing times the factor
  bumpout_x <- 1 * varbox_x_scaling  #default spacing times the factor
  bumpout_y <- 1 * varbox_y_scaling  #default spacing times the factor
  space_x <- 2 * varspace_x_scaling  #default spacing times the factor
  newvariables <- list()  # create an empty list to store the grid coordinates

  # the process is:
  #   1. Loop over row ids and then column ids
  #   2. Starting location is 0,0 on the grid for the bottom-left corner of first "box"
  #   3. Create a temporary dataframe containg the x and y min,max
  #      locations for each box based on the start positiions.
  #   4. Bind the coordinates dataframe
  #   5. Iteratively updates y starts (each row) and x starts (each column)
  for(ri in sort(unique(rids))){
    ystart <- (ri-1) * rowspace_y  # update y start position based on row id
    xstart <- 0  # xstart is always zero for the first column in a row
    for(ci in sort(unique(cids))) {
      tmp <- data.frame(row_id = ri,
                        col_id = ci,
                        xmin = NA,
                        xmax = NA,
                        ymin = NA,
                        ymax = NA)
      tmp$xmin <- xstart
      tmp$xmax <- xstart + bumpout_x
      tmp$ymin <- ystart
      tmp$ymax <- ystart + bumpout_y
      xstart <- xstart + bumpout_x + space_x
      newvariables <- rbind(newvariables, tmp)
    }
  }

  # now we map the variable names to the coordinates based on row
  # and column ids
  tmpvars <- subset(idmap, label != "")  # ignore empty boxes on the grid
  # merge by row and column ids
  tmpvars <- merge(newvariables, tmpvars, by = c("row_id", "col_id"))

  # subset to just the columns of interest for further processing and output
  tmpvars <- tmpvars[, c("label", "xmin", "xmax", "ymin", "ymax")]

  # now merge into the variables dataframe
  variables <- merge(variables, tmpvars, by = "label", all.x = TRUE)

  # calculate midpoints for label locations
  # simple means for now that get updated as needed with further processing
  variables$xlabel <- rowMeans(variables[ , c("xmin", "xmax")])
  variables$ylabel <- rowMeans(variables[ , c("ymin", "ymax")])

  # order by the assigned numeric ids
  variables <- variables[order(variables$id), ]
  return(variables)  # return the variables data frame for further processing
}
