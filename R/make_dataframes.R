#' Create data frames for plotting from model elements.
#'
#' @param input_list A list of model elements. Currently only accepts
#'     a modelbuilder list object. At a minimum, the list must contain
#'     two elements with names \code{vars} and \code{flows}.
#' @return A list of data frames.
#' @export

make_dataframes <- function(input_list) {
  # TODO error checking


  # Extract relevant details from the input_list and make a matrix
  # of variables X flows for iterating and indexing the nodes and
  # connections.
  nvars <- length(input_list$var)  #number of variables/compartments in model
  varnames <- unlist(lapply(input_list$var, "[[", 1))
  vartext <- unlist(sapply(input_list$var, '[', 2)) #extract variable text as vector
  allflows <- sapply(input_list$var, '[', 4) #extract flows

  #turns flow list into matrix, adding NA, found it online,
  #not sure how exactly it works
  flowmat <- t(sapply(allflows, `length<-`, max(lengths(allflows))))
  flowmatred <- sub("\\+|-","",flowmat)   #strip leading +/- from flows
  signmat <- gsub("(\\+|-).*","\\1",flowmat) #extract only the + or - signs from flows so we know the direction

  # Create a node data frame
  ndf <- data.frame(
    id = 1:nvars,  # number of nodes
    label = varnames,  # labels of nodes
    row = 1  # hard code for 1 row, can be updated below
  )

  # Split variables by rows if stratification implied by numbers
  strats <- gsub("[^0-9.]", "",  varnames)
  strats <- ifelse(strats == "", 1, strats)
  ndf$row <- as.numeric(strats)


  # Create the edge data frame by looping through the variables
  # and associated flows.
  edf <- list()  #an empty list to be coerced to a data frame via rbind
  for(i in 1:nrow(flowmatred)) {
    varflowsfull = flowmat[i,] #all flows with sign for current variable
    varflows = flowmatred[i,] #all flows for current variable
    varflowsigns = signmat[i,] #signs of flows for current variable
    varflows = varflows[!is.na(varflows)] #remove NA entries

    for(j in 1:length(varflows)) {
      currentflowfull = varflowsfull[j] #loop through all flows for variable
      currentflow = varflows[j] #loop through all flows for variable
      currentsign = varflowsigns[j] #loop through all flows for variable

      # Find the variables for which the current flow appears, i.e., what
      # other rows of the matrix does it show up in.
      connectvars <- unname(which(flowmatred == currentflow, arr.ind = TRUE)[,1])

      # Extract the variable names
      varspars <- unique(get_vars_pars(currentflowfull))
      varfirsts <- substr(varspars, start = 1, stop = 1)  #get first letters
      vars <- varspars[which(varfirsts %in% LETTERS)]

      # If the flow does not show up in any other rows BUT starts with
      # a plus sign, then the donating node will be the state variable
      # in the flow
      if(currentsign == "+") {
        if(length(connectvars) == 1 & length(vars) == 0) {
          connectvars <- i
        }
        if(length(connectvars) == 1 & length(vars) >= 1){
          if(!varnames[i] %in% vars) {
            connectvars <- i
          }
          if(varnames[i] %in% vars) {
            connectvars <- c(i, i)
          }
        }
        if(length(connectvars) > 1) {
          connectvars <- connectvars
        }
      }

      # If current sign is negative, it is an outflow and goes either the
      # connectvar that is not equal to the current variable id (indexed by i)
      # or it goes to NA (this happens when there is an unspecified death
      # compartment, for example).
      if(currentsign == "-") {
        if(length(connectvars) == 1) {
          cn <- NA  #placeholder for unspecified compartment (deaths, typically)
        } else {
          cn <- connectvars[connectvars!=i]
        }

        tmp <- data.frame(from = i,
                          to = cn,
                          label = currentflow,
                          interaction = FALSE)

        edf <- rbind(edf, tmp)
      }

      # If the current sign is positive AND the flow only shows up in
      # one row of the flow matrix, then this is an inflow external to the
      # system or as a function of the current variable itself. Currently,
      # we assume these arise from the variable itself, but we can extend
      # this functionality later on.
      if(currentsign == "+" & length(connectvars) == 1) {
        if(connectvars == i) {
          tmp <- data.frame(from = NA,
                            to = i,
                            label = currentflow,
                            interaction = FALSE)
          edf <- rbind(edf, tmp)
        }
      }
      if(currentsign == "+" & length(connectvars) == 2) {
        if(length(unique(connectvars)) == 1) {
          tmp <- data.frame(from = i,
                            to = i,
                            label = currentflow,
                            interaction = FALSE)
        } else {
          tmp <- data.frame(from = connectvars[connectvars!=i],
                            to = i,
                            label = currentflow,
                            interaction = FALSE)
        }
        edf <- rbind(edf, tmp)
      }

      # interaction flag if two variables are in the flow
      if(length(vars) > 1) {
        edf[nrow(edf), "interaction"] <- TRUE
      }
    }  #end flow loop
  }  #end variable loop

  # Keep only distinct rows
  edf <- unique(edf)

  # Break edges apart into:
  #   direct flows
  #   interactions to meet at edges
  #   the flows resulting from interactions
  edf$link <- NA  #empty column for interaction flows, but needed for binding
  ints <- subset(edf, interaction == TRUE)
  edf <- subset(edf, interaction == FALSE)
  intflows <- ints
  intflows$label <- ""
  intflows <- unique(intflows)
  intflows$interaction <- FALSE

  # Redefine the interaction from nodes
  for(i in 1:nrow(ints)) {
    tmp <- ints[i, ]
    v <- get_vars_pars(tmp$label)
    vf <- substr(v, start = 1, stop = 1)  #get first letters
    v <- v[which(vf %in% LETTERS)]
    ids <- subset(ndf, label %in% v)[ , "id"]
    ints[i, "from"] <- ids[2]
    ints[i, "to"] <- NA
    ints[i, "link"] <- ids[1]
  }

  # Recombine the edge data frame
  edf <- rbind(edf, ints, intflows)

  # Make dummy compartment for all flows in and out of the system.
  # Out of the system first
  outdummies <- NULL
  # numnas <- length(edf[is.na(edf$to), "to"])
  numnas <- length(edf[is.na(edf$to) & edf$interaction == FALSE, "to"])
  if(numnas > 0) {
    outdummies <- as.numeric(paste0("999", c(1:numnas)))
    edf[is.na(edf$to) & edf$interaction == FALSE, "to"] <- outdummies
  }

  # In to the system second
  indummies <- NULL
  # numnas <- length(edf[is.na(edf$from), "from"])
  numnas <- length(edf[is.na(edf$from) & edf$interaction == FALSE, "from"])
  if(numnas > 0) {
    indummies <- as.numeric(paste0("-999", c(1:numnas)))
    edf[is.na(edf$from) & edf$interaction == FALSE, "from"] <- indummies
  }

  # Make dummy compartment for "links" in interactions
  linkdummies <- NULL
  numlinks <- length(edf[is.na(edf$to) & edf$interaction == TRUE, "to"])
  if(numlinks > 0) {
    linkdummies <- as.numeric(paste0("555", c(1:numlinks)))
    linkdummies <- mean(c(edf[is.na(edf$to) & edf$interaction == TRUE, "from"],
                          edf[is.na(edf$to) & edf$interaction == TRUE, "link"]))
    edf[is.na(edf$to) & edf$interaction == TRUE, "to"] <- linkdummies
  }


  # Add dummy compartments to nodes dataframe
  if(is.numeric(outdummies) | is.numeric(indummies) | is.numeric(linkdummies)) {
    exnodes <- data.frame(id = c(outdummies, indummies, linkdummies),
                          label = "",
                          row = 1)  # TODO
    ndf <- rbind(ndf, exnodes)
  }

  # Add x and y locations for the nodes
  ndf <- ndf[order(ndf$id), ]
  ndf$x <- NA
  ndf$y <- NA
  for(rid in unique(ndf$row)) {
    ndf[which(ndf$row == rid), "x"] <- 1:nrow(ndf[which(ndf$row == rid), ])*3
    ndf[which(ndf$row == rid), "y"] <- as.numeric(rid) * -2
  }

  # update inflow node positions from nowhere
  inflownodes <- subset(ndf, id < -9990)$id
  for(id in inflownodes) {
    newxyid <- edf[which(edf$from == id), "to"]
    newxy <- ndf[which(ndf$id == newxyid), c("x", "y")]
    newxy$y <- newxy$y + 2
    ndf[which(ndf$id == id), c("x", "y")] <- newxy
  }

  # update outflow node positions to nowhere
  outflownodes <- subset(ndf, id > 9990)$id
  for(id in outflownodes) {
    newxyid <- edf[which(edf$to == id), "from"]
    newxy <- ndf[which(ndf$id == newxyid), c("x", "y")]
    newxy$y <- newxy$y - 2
    ndf[which(ndf$id == id), c("x", "y")] <- newxy
  }

  # update node positions that overlap
  xys <- ndf[ , c("x", "y")]
  overlapids <- which(duplicated(xys) | duplicated(xys, fromLast = TRUE))
  numoverlap <- length(overlapids)
  if(numoverlap > 0) {
    newxs <- seq(0.1, 1.9, by = 0.15)
    newxids <- c(6,5,4,3,2,1,0,1,2,3,4,5,6) + 1
    xmults <- newxs[which(newxids==numoverlap)]
    for(i in 1:numoverlap) {
      oldx <- ndf[overlapids[i], "x"]
      newx <- oldx * xmults[i]
      ndf[overlapids[i], "x"] <- newx
    }
  }


  # Create segment coordinates
  edf <- merge(edf, ndf[ , c("x", "y", "id")], by.x = "from", by.y = "id")
  edf <- merge(edf, ndf[ , c("x", "y", "id")], by.x = "to", by.y = "id",
               suffixes = c("start", "end"))
  edf$xmid <- with(edf, (xend + xstart) / 2)
  edf$ymid <- with(edf, (yend + ystart) / 2) + 0.25
  edf$diff <- with(edf, abs(to-from))

  # split up the edges into constituent parts:
  # - curved segments
  # - straight (horizontal) segments
  # - vertical segments
  # - feedback segments (curved back onto same node)
  cdf <- subset(edf, (diff > 1 & diff < 9000) & (to != from) | interaction == TRUE)
  sdf <- subset(edf, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  vdf <- subset(sdf, abs(diff) >= 9990)
  sdf <- subset(sdf, abs(diff) < 9990)
  fdf <- subset(sdf, to == from)
  sdf <- subset(sdf, to != from)

  # Set default curvature if cdf has data
  if(nrow(cdf) > 0) {
    cdf$curvature <- 0.25

    # add in row info
    cdf <- merge(cdf, ndf[ , c("id", "row")], by.x = "to", by.y = "id")
    cdf$row <- as.numeric(cdf$row)

    # Update curvature based on row, if only 2 rows
    if(max(as.numeric(ndf$row)) > 1 & max(as.numeric(ndf$row)) <= 2) {
      cdf$curvature <- ifelse(cdf$row == 1, 0.25, -0.25)

      # also update ystart and yend
      cdf$ystart <- ifelse(cdf$row == 2, cdf$ystart-1, cdf$ystart)
      cdf$yend <- cdf$ystart
    }

    # add curvature midpoint for accurate label placement
    cdf$labelx <- NA
    cdf$labely <- NA
    for(i in 1:nrow(cdf)) {
      tmp <- cdf[i, ]
      mids <- calc_control_points(x1 = tmp$xstart,
                                  y1 = tmp$ystart,
                                  x2 = tmp$xend,
                                  y2 = tmp$yend,
                                  angle = 90,
                                  curvature = tmp$curvature,
                                  ncp = 1)
      cdf[i, "labelx"] <- mids$x
      cdf[i, "labely"] <- mids$y
    }

    # curves need to move up 0.5 units to connect with tops/bottoms
    # of node rectangles
    cdf$ystart <- cdf$ystart + 0.5
    cdf$yend <- cdf$yend + 0.5
    cdf$ymid <- cdf$ymid + 0.5
    cdf$labely <- cdf$labely + 0.5

    # if curve is for an interaction term, then yend needs to be moved
    # back down by 0.5 to meet up with the edge rather than the node
    cdf[cdf$interaction == TRUE, "yend"] <- cdf[cdf$interaction == TRUE, "yend"] - 0.5

    # add y offset to curve labels according to row
    for(i in 1:nrow(cdf)) {
      tmp <- cdf[i, ]
      offset <- ifelse(cdf[i, "row"] == 2, -0.2, 0.2)
      cdf[i, "labely"] <- cdf[i, "labely"] + offset
    }
  }


  # test to make sure splits are unique and sum up to original data frame
  test <- nrow(vdf) + nrow(sdf) + nrow(cdf) + nrow(fdf) == nrow(edf)
  if(!test) {
    stop(paste0("Edges data frame is not splitting appropriately.\n",
                "       Contact package maintainer."))
  }

  # now drop "hidden" nodes without labels
  ndf <- subset(ndf, label != "")

  # rename data frames for exporting
  nodes <- ndf
  horizontal_edges <- subset(sdf, select = -c(diff))
  vertical_edges <- subset(vdf, select = -c(diff))
  curved_edges <- subset(cdf, select = -c(diff))
  feedback_edges <- subset(fdf, select = -c(diff))

  dfs <- list(nodes = nodes,
              horizontal_edges = horizontal_edges,
              vertical_edges = vertical_edges,
              curved_edges = curved_edges,
              feedback_edges = feedback_edges)
  f <- make_diagram(dfs)
  f


  return(list(nodes = nodes,
              horizontal_edges = horizontal_edges,
              vertical_edges = vertical_edges,
              curved_edges = curved_edges,
              feedback_edges = feedback_edges))
}
