#' Add x,y location informations
#'
#' @param variables The nodes (variables) data frame.
#' @param varlocations The varlocations matrix. Default is NULL.
#' @param varbox_x_scaling Scaler for box width.
#' @param varbox_y_scaling Scaler for box height.
#' @param varspace_x_scaling Scaler for horizontal spacing.
#' @param varspace_y_scaling Scaler for vertical spacing.
#' @return The variables data frame with location information.
#' @noRd

add_locations <- function(variables, varlocations = NULL, varbox_x_scaling,
                          varbox_y_scaling, varspace_x_scaling,
                          varspace_y_scaling) {

  if(is.null(varlocations)) {
    newvariables <- list()
    for(rid in unique(variables$row)) {
      tmp <- subset(variables, row == rid)
      tmp$xmin <- NA
      tmp$xmax <- NA
      tmp$ymin <- NA
      tmp$ymax <- NA
      xstart <- 0
      rowspace_y <- -3 #each row is -3 from the bottom of the other row: 2 spacing and 1 for size of box
      ystart <- (rid-1) * rowspace_y * varspace_y_scaling
      bumpout_x <- 1 * varbox_x_scaling
      bumpout_y <- 1 * varbox_y_scaling
      space_x <- 2 * varspace_x_scaling
      for(i in 1:nrow(tmp)) {
        tmp[i, "xmin"] <- xstart
        tmp[i, "xmax"] <- xstart + bumpout_x
        tmp[i, "ymin"] <- ystart
        tmp[i, "ymax"] <- ystart + bumpout_y

        # update location settings, just x within a row
        xstart <- xstart + bumpout_x + space_x
      }
      newvariables <- rbind(newvariables, tmp)
    }
    variables <- newvariables
    rm(newvariables)

    # calculate midpoints for label locations, in general
    variables$xlabel <- rowMeans(variables[ , c("xmin", "xmax")])
    variables$ylabel <- rowMeans(variables[ , c("ymin", "ymax")])
  } else {
    num_rows <- nrow(varlocations)
    num_cols <- ncol(varlocations)
    positions <- 1:(num_rows*num_cols)
    ids <- varlocations[positions]
    rids <- rep(num_rows:1, length(positions)/2)
    cids <- rep(1:num_cols, each = length(positions)/3)
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
  }

  variables <- variables[order(variables$id), ]
  return(variables)

}
