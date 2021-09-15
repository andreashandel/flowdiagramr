#' Add x,y location informations
#'
#' @param variables The nodes (variables) data frame.
#' @param varlocations The varlocations matrix. Default is NULL.
#' @param varbox_x_scaling Scaler for box width.
#' @param varbox_y_scaling Scaler for box height.
#' @param varspace_x_scaling Scaler for horizontal spacing.
#' @param varspace_y_scaling Scaler for vertical spacing.
#' @return The variables data frame with location information.
#' @export

add_locations <- function(variables, varlocations = NULL, varbox_x_scaling,
                          varbox_y_scaling, varspace_x_scaling,
                          varspace_y_scaling) {

  # create a varlocations matrix if the user does not provide one.
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

  # make row ids
  rids <- rep(num_rows:1, length(positions)/num_rows)
  cids <- rep(1:num_cols, each = length(positions)/num_cols)
  idmap <- data.frame(row_id = rids,
                      col_id = cids,
                      pos = positions,
                      label = ids)
  rowspace_y <- 3 #each row is 3 from the top of the other row: 2 spacing and 1 for size of box
  bumpout_x <- 1 * varbox_x_scaling
  bumpout_y <- 1 * varbox_y_scaling
  space_x <- 2 * varspace_x_scaling
  newvariables <- list()
  for(ri in sort(unique(rids))){
    ystart <- (ri-1) * rowspace_y * varspace_y_scaling
    xstart <- 0
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

  tmpvars <- subset(idmap, label != "")
  tmpvars <- merge(newvariables, tmpvars, by = c("row_id", "col_id"))
  tmpvars <- tmpvars[, c("label", "xmin", "xmax", "ymin", "ymax")]
  variables <- merge(variables, tmpvars, by = "label", all.x = TRUE)
  # calculate midpoints for label locations, in general
  variables$xlabel <- rowMeans(variables[ , c("xmin", "xmax")])
  variables$ylabel <- rowMeans(variables[ , c("ymin", "ymax")])

  variables <- variables[order(variables$id), ]
  return(variables)

}
