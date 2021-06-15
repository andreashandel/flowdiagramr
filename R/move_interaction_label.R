#' Moves interaction label to main flow label.
#'
#' @param flows The flows data frame.
#' @return The flows data frame.
#' @noRd

move_interaction_label <- function(flows) {

  ints <- subset(flows, interaction == TRUE)

  for(i in 1:nrow(ints)) {
    vps <- get_vars_pars(ints[i, "label"])
    vars <- vps[which(vps %in% LETTERS)]
    ints[i, "to"] <- ints[i, "from"]
    ints[i, "from"] <- vars[which(vars != ints[i, "to"])]

    to <- ints[i, "to"]
    from <- ints[i, "from"]
    id <- which(flows$to == to & flows$from == from)
    flows[id, "label"] <- ints[i, "label"]
  }

  flows <- subset(flows, interaction == FALSE)
  return(flows)
}
