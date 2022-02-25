#' Helper function to perform basic checks of the variables
#' and flows data frames. Returns errors if found.
#'
#' @param diagram_list The list of `variables` and `flows` data frames.
#' @return Either `NULL` or an error message.

check_dataframes <- function(diagram_list) {
  # unlist the data frames
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  # check the variables data frame
  # 1. check column names
  allowed_var_columns <- c("name", "id", "xmin", "xmax", "ymin", "ymax",
                           "xlabel", "ylabel", "label_text", "outline_color",
                           "fill_color", "label_color", "label_size")
  test <- setdiff(colnames(variables), allowed_var_columns)
  if(length(test) > 0) {
    msg <- paste0("The variables data frame contains the following invalid column names: ",
                  paste(test, collapse = ", "))
    return(msg)
  }

  # 2. Check structures of columns
  var_str_map <- data.frame(
    column = allowed_var_columns,
    struct =  c("chr", "num", "num", "num", "num", "num", "num", "num",
                "chr", "chr", "chr", "chr", "num")
  )
  for(i in 1:ncol(variables)) {
    x <- capture.output(str(variables[,i]))  # this captures a string of the str output
    val <- substr(x, 2, 4)  # first space is always blank, then 3 letters for the str
    targ <- var_str_map[var_str_map$column == colnames(variables)[i], "struct"]
    test <- val != targ
    if(test) {
      msg <- paste0("The column `", colnames(variables)[i], "` is ", val, " but must be ", targ, ".")
      return(msg)
    }
  }


}
