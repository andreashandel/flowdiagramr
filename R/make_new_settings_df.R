#' Updates specified columns in a data frame with new values from a list.
#'
#' @description Helper function that updates all settings columns in a
#' flows or variables data frame based on the new values in diagram_settings.
#' @param n The number of variables or flows.
#' @param diagram_settings The diagram_settings list.

make_new_settings_df <- function(n,
                                 diagram_settings) {
  # create a matrix from the diagram_settings list
  # this is coerced to a data frame that is used in the main function
  df <- as.data.frame(diagram_settings)
  if(nrow(df) != n) {
    tmp <- df
    tmp[] <- NA
    out <- do.call("rbind", replicate(n - nrow(df), tmp, simplify = FALSE))
    df <- rbind(df, out)
  }

  # fill down the rows if na after first element
  for(i in 1:ncol(df)) {
    tmp <- df[,i]
    tmp[is.na(tmp)] <- tmp[1]
    df[,i] <- tmp
  }

  # update column name
  cnames <- colnames(df)
  new_names <- unlist(strsplit(cnames, "var_|main_flow_|external_flow_|interaction_flow_"))
  new_names <- new_names[which(new_names != "")]
  colnames(df) <- new_names

  return(df)
}
