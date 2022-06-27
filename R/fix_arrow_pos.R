#' Adjust arrow positions to avoid overlaps.
#' helper function for prepare_diagram that is only applied to
#' arrows from nowhere or to nowhere (e.g., birth or death arrows).
#'
#' @param flows The flows data frame.
#' @export

fix_arrow_pos <- function(flows) {
  # subset flows down to the flows that are into or out of the system
  sdf <- subset(flows, (is.na(from) | is.na(to)) & interaction == FALSE)

  # pull just the location information
  v <- sdf[ , c("xmin", "ymin", "xmax", "ymax")]

  # find duplicates in any column
  reps <- duplicated(v) | duplicated(v, fromLast = TRUE)

  # if there are duplicates, apply a small offset to move the first
  # encounter to the left and the second encounter to the right.
  # note that this is limited to just two instances for now.
  if(length(reps > 0)) {
    tmp <- sdf[reps, ]
    tmp$allspace <- paste0(tmp$xmin, tmp$xmax, tmp$ymin, tmp$ymax)
    unique_ones <- unique(tmp$allspace)
    out <- data.frame()
    for(i in 1:length(unique_ones)) {
      tmptmp <- subset(tmp, allspace == unique_ones[i])
      off <- 0.25
      tmptmp[1, "xmin"] <- tmptmp[1, "xmin"] - off
      tmptmp[1, "xmax"] <- tmptmp[1, "xmax"] - off
      tmptmp[2, "xmin"] <- tmptmp[2, "xmin"] + off
      tmptmp[2, "xmax"] <- tmptmp[2, "xmax"] + off
      out <- rbind(out, tmptmp)
    }
    out$allspace <- NULL
    out <- rbind(sdf[!reps, ], out)
  } else {
    out <- sdf
  }

  # remove rows with all NAs
  tokeep <- which(!(is.na(out$to) & is.na(out$from)))
  out <- out[tokeep, ]

  # only replace rows in the full dataframe that we have potentially adjusted
  toreplace <- match(rownames(flows), rownames(sdf))
  toreplace <- which(!is.na(toreplace))
  flows[toreplace, ] <- out
  return(flows)

}
