#' Helper function to perform basic checks of the variables
#' and flows data frames. Returns errors if found.
#'
#' @param diagram_list The list of `variables` and `flows` data frames.
#' @return Either `NULL` or an error message.

check_dataframes <- function(diagram_list) {
  # unlist the data frames
  variables <- diagram_list$variables
  flows <- diagram_list$flows

  ###
  # Check the variables data frame
  ###
  # 1. check column names
  allowed_var_columns <- c("name", "id", "xmin", "xmax", "ymin", "ymax",
                           "xlabel", "ylabel", "label_text", "outline_color",
                           "fill_color", "label_color", "label_size")
  # `test` will be the column names that are in `variables`
  #    but not in `allowed_var_columns`
  test <- setdiff(colnames(variables), allowed_var_columns)
  if(length(test) > 0) {
    msg <- paste0("The variables data frame contains the following invalid column names: ",
                  paste(test, collapse = ", "))
    return(msg)
  }

  # 2. Check structures of columns
  # this is a map between the columns names and their allowed structures
  var_str_map <- data.frame(
    column = allowed_var_columns,
    struct =  c("chr", "num", "num", "num", "num", "num", "chr", "chr",
                "chr", "chr", "chr", "chr", "num")
  )
  for(i in 1:ncol(variables)) {
    x <- utils::capture.output(utils::str(variables[,i]))  # this captures a string of the str output
    val <- substr(x, 2, 4)  # first space is always blank, then 3 letters for the str
    # find what the structure should be: `targ`
    targ <- var_str_map[var_str_map$column == colnames(variables)[i], "struct"]
    # test whether the `val` structure is different from the target (`targ`)
    test <- val != targ
    if(test) {
      msg <- paste0("The column `", colnames(variables)[i], "` is ", val, " but must be ", targ, ".")
      return(msg)
    }
  }

  # 3. Check that the color values are valid. Must be in `colors()` or a HEX code.
  used_colors <- c(
    variables[ , "outline_color"],
    variables[ , "fill_color"],
    variables[ , "label_color"]
  )
  nonrcolors_used <- which(!(used_colors %in% grDevices::colors()))
  if(length(nonrcolors_used) > 0) {
    # is it a HEX?
    # get first character, which will be "#" for HEX codes
    val <- substr(used_colors[nonrcolors_used], 1, 1)
    badid <- which(val != "#")
    badcolors <- used_colors[nonrcolors_used][badid]
    if(length(badcolors) > 0) {
      msg <- paste0("The folowing colors in `variables` are not a valid color name or a HEX code: ", badcolors)
      return(msg)
    }
  }


  ###
  # Check the flows data frame.
  ###
  allowed_flow_columns <- c("type", "id", "from", "to", "label",
                            "xmin", "xmax", "ymin", "ymax",
                            "xlabel", "ylabel", "curvature", "math",
                            "label_text", "color", "size", "linetype",
                            "label_color", "label_size",
                            "arrow_size", "show_arrow")
  flow_str_map <- data.frame(
    column = allowed_flow_columns,
    struct =  c("chr", "int", "chr", "chr", "chr", "num", "num", "num",
                "num", "num", "num", "num", "chr", "chr", "chr", "num",
                "chr", "chr", "num", "num", "logi")
  )
  # 1. check column names
  # `test` will be the column names that are in `flows`
  #    but not in `allowed_var_columns`
  test <- setdiff(colnames(flows), allowed_flow_columns)
  if(length(test) > 0) {
    msg <- paste0("The flows data frame contains the following invalid column names: ",
                  paste(test, collapse = ", "))
    return(msg)
  }

  # 2. Check structures of columns
  for(i in 1:ncol(flows)) {
    x <- utils::capture.output(utils::str(flows[,i]))  # this captures a string of the str output
    val <- substr(x, 2, 4)  # first space is always blank, then 3 letters for the str
    # find what the structure should be: `targ`
    targ <- flow_str_map[flow_str_map$column == colnames(flows)[i], "struct"]
    # test whether the `val` structure is different from the target (`targ`)
    test <- val != targ
    if(test) {
      msg <- paste0("The column `", colnames(flows)[i], "` is ", val, " but must be ", targ, ".")
      return(msg)
    }
  }

  # 3. Check that the color values are valid. Must be in `colors()` or a HEX code.
  used_colors <- c(
    flows[ , "color"],
    flows[ , "label_color"]
  )
  nonrcolors_used <- which(!(used_colors %in% grDevices::colors()))
  if(length(nonrcolors_used) > 0) {
    # is it a HEX?
    # get first character, which will be "#" for HEX codes
    val <- substr(used_colors[nonrcolors_used], 1, 1)
    badid <- which(val != "#")
    badcolors <- used_colors[nonrcolors_used][badid]
    if(length(badcolors) > 0) {
      msg <- paste0("The folowing colors in `flows` are not a valid color name or a HEX code: ", badcolors)
      return(msg)
    }
  }

  # 4. Check that the linetype values are valid. Must be one of seven allowed.
  linetypes_allowed <- c("blank", "solid", "dashed", "dotted", "dotdash",
                         "longdash", "twodash")
  linetypes_used <- c(
    flows[ , "linetype"]
  )
  nonrlines_used <- which(!(linetypes_used %in% linetypes_allowed))
  if(length(nonrlines_used) > 0) {
    badlines <- linetypes_used[nonrlines_used]
    msg <- paste0("The following linetypes in `flows` are not valid: ", badlines)
    return(msg)
  }
}
