#' Updates the numeric to/from ids to the variable label
#' @noRd

update_tofroms <- function(targ, nodes) {
  labs <- nodes[ , c("id", "label")]
  if(nrow(targ) > 0) {
    for(i in 1:nrow(targ)) {
      tid <- targ[i, "to"]
      fid <- targ[i, "from"]
      tlab <- subset(labs, id == tid)$label
      flab <- subset(labs, id == fid)$label
      if(length(tlab) == 0) {
        tlab <- NA
      }
      if(length(flab) == 0) {
        flab <- NA
      }
      targ[i, "to"] <- tlab
      targ[i, "from"] <- flab
    }
  }
  return(targ)
}
