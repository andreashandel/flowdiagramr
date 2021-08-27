


# merge with nodes to get new "from" positions
ext <- merge(ext, ndf[, c("id", "xmin", "xmax", "ymin", "ymax")],
              by.x = "from", by.y = "id")

# loop over the exts and apply xmin based on direction of flow
for (i in 1:nrow(ext)) {
  tmp <- ext[i,]
  dir <- tmp$from - tmp$to
  if(sign(dir) == -1) {
    # if going left to right, then set xmin to the xmax of the from node
    tmp$xmin <- tmp$xmax

    # ymin is the middle of the node
    tmp$ymin <- mean(c(tmp$ymax, tmp$ymin))
  }
  if(sign(dir) == 1) {
    # if going right to left, then set xmin to the xmin of the from node
    tmp$xmin <- tmp$xmin

    # ymin is the middle of the node
    tmp$ymin <- mean(c(tmp$ymax, tmp$ymin))
  }
  ext[i, ] <- tmp
}

# remove the xmax and ymax columns to be redefined
ext[, c("xmax", "ymax")] <- NULL

# get the arrows that are influenced by the external interactions
extarrows <- subset(edf, label == "")

# get just the columns we need
extarrows <- extarrows[ , c("from", "xlabel", "ylabel")]

# merge to get the max positions
ext <- merge(ext, extarrows, by.x = "to", by.y = "from")
ext$xmax <- ext$xlabel
ext$ymax <- ext$ylabel
