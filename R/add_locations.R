#' Add x,y location information
#' Helper function called by prepare_diagram.
#' Likely not useful for direct calling.
#'
#' @param variables The variables data frame.
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
#' @importFrom stats na.omit
#' @export

add_locations <- function(variables,
                          varlocations,
                          varbox_x_size,
                          varbox_y_size,
                          varspace_x_size,
                          varspace_y_size) {

  #### extract dimensions of the grid and number of variables
  num_rows <- nrow(varlocations)
  num_cols <- ncol(varlocations)
  num_vars <- length(which(varlocations != ""))

  #### add variable names to the varbox vector for matching
  names(varbox_x_size) <- variables$name
  names(varbox_y_size) <- variables$name

  #### box sizes on grid
  # to start, make a matrix of the size of each variable, one for
  # x size and one for y size
  xsize_mat <- matrix(data = NA,  # default size
                      nrow = num_rows,
                      ncol = num_cols,
                      byrow = TRUE)
  xsize_mat[match(names(varbox_x_size), varlocations)] <- varbox_x_size

  ysize_mat <- matrix(data = NA,
                      nrow = num_rows,
                      ncol = num_cols,
                      byrow = TRUE)
  ysize_mat[match(names(varbox_y_size), varlocations)] <- varbox_y_size

  #### x mins, mids, and maxs
  # create a vector of distances, assuming start point left edge of box 1 at 0.
  # This is a vector that starts with the first box size and then alternates
  # between box size and spacing between boxes.
  # First, process for situations with more than one column in varlocations
  if(ncol(varlocations) > 1) {
    # take the maximum xsize to form the grid so that everything is centered
    x_size <- max(xsize_mat, na.rm = TRUE)

    # define the number of widths needed as number of columns (boxes) plus
    # the number of columns minus 1 (spaces between boxes)
    vec_length_x <- num_cols + (num_cols-1)
    dist_vector <- vector(class(varbox_x_size), vec_length_x)
    dist_vector[c(TRUE, FALSE)] <- x_size
    dist_vector[c(FALSE, TRUE)] <- varspace_x_size

    # take cumulative sum of the distance vector to get distance from 0
    x_from_zero <- cumsum(dist_vector)

    # every odd element is the end of a box, which we can use to fill
    # in the locations matrix elements by row with x-max locations
    xmids <- matrix(x_from_zero[c(TRUE, FALSE)],
                    nrow = nrow(varlocations),
                    ncol = ncol(varlocations),
                    byrow = TRUE)

    # xmin is xmid minus 1/2 of the size of the box
    # xmax is xmid plus 1/2 of the  size of the box
    xmins <- xmids - xsize_mat/2
    xmaxs <- xmids + xsize_mat/2
  } else {
    # if one column, then the x locations are simply a function of the
    # x box size, again starting with left edge at 0
    xmins <- matrix(data = 0, nrow = num_rows, ncol = 1)
    xmids <- matrix(data = varbox_x_size/2, nrow = num_rows, ncol = 1)
    xmaxs <- matrix(data = varbox_x_size, nrow = num_rows, ncol = 1)
  }



  #### y mins, mids, and maxs
  ## First process is if there is more than one row
  if(num_rows > 1) {
    # take the maximum ysize to form the grid so that everything is centered
    y_size <- max(ysize_mat, na.rm = TRUE)

    # add row(s) in ysize_mat for spacing in y direction
    space_rows <- matrix(varspace_y_size, num_rows-1, num_cols)
    ymat <- matrix(data = NA, nrow = num_rows + nrow(space_rows), num_cols)
    ymat[c(TRUE, FALSE), ] <- y_size
    ymat[c(FALSE, TRUE), ] <- space_rows

    # reverse of the cumulate sum within columns is the y-distance from 0
    y_from_zero <- ymat
    y_from_zero[] <- apply(ymat, MARGIN = 2, cumsum)[nrow(ymat):1, ]

    # make sure it is a matrix

    # the odd rows are the tops of each box
    ymaxs <- y_from_zero[c(TRUE, FALSE), ]

    # ymid is ymax minus 1/2 of the y size of the box
    # ymin is ymax minus y size of the box
    ymids <- ymaxs - ysize_mat/2
    ymins <- ymaxs - ysize_mat
  } else {
    # if one row, then the y locations are simply a function of the y box size
    ymins <- matrix(data = 0, nrow = 1, ncol = num_cols)
    ymids <- matrix(data = varbox_y_size/2, nrow = 1, ncol = num_cols)
    ymaxs <- matrix(data = varbox_y_size, nrow = 1, ncol = num_cols)
  }

  #### insert the location information in the data frame based on variable name
  variables$xmin <- xmins[match(variables$name, varlocations)]
  variables$xmid <- xmids[match(variables$name, varlocations)]
  variables$xmax <- xmaxs[match(variables$name, varlocations)]
  variables$ymin <- ymins[match(variables$name, varlocations)]
  variables$ymid <- ymids[match(variables$name, varlocations)]
  variables$ymax <- ymaxs[match(variables$name, varlocations)]

  #### rename mids as label positions
  names(variables)[names(variables) %in% c("xmid", "ymid")] <- c("xlabel", "ylabel")

  #### return the variables data frame
  return(variables)
}
