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
    id = 1:nvars,  #number of nodes
    label = varnames  #labels of nodes
  )


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
      vars <- varspars[which(varspars %in% LETTERS)]

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

      # if(length(connectvars) == 1 & currentsign == "+") {
      #   varspars <- get_vars_pars(currentflow)
      #   var <- varspars[which(varspars %in% LETTERS)]
      #   cnnew <- which(varnames == var)
      #   if(length(cnnew) == 1) {
      #     connectvars <- c(connectvars, cnnew)
      #   } else {
      #     connectvars <- c(i, i)
      #   }
      # }

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
                          label = currentflow)

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
                            label = currentflow)
          edf <- rbind(edf, tmp)
        }
      }
      if(currentsign == "+" & length(connectvars) == 2) {
        if(length(unique(connectvars)) == 1) {
          tmp <- data.frame(from = i,
                            to = i,
                            label = currentflow)
        } else {
          tmp <- data.frame(from = connectvars[connectvars!=i],
                            to = i,
                            label = currentflow)
        }
        edf <- rbind(edf, tmp)
      }
    }  #end flow loop
  }  #end variable loop

  # Make dummy compartment for all flows in and out of the system.
  # Out of the system first
  outdummies <- NULL
  numnas <- length(edf[is.na(edf$to), "to"])
  if(numnas > 0) {
    outdummies <- as.numeric(paste0("999", c(1:numnas)))
    edf[is.na(edf$to), "to"] <- outdummies
  }

  # In to the system second
  indummies <- NULL
  numnas <- length(edf[is.na(edf$from), "from"])
  if(numnas > 0) {
    indummies <- as.numeric(paste0("-999", c(1:numnas)))
    edf[is.na(edf$from), "from"] <- indummies
  }


  # Add dummy compartments to nodes dataframe
  if(is.numeric(outdummies) | is.numeric(indummies)) {
    exnodes <- data.frame(id = c(outdummies, indummies),
                          label = "")
    ndf <- rbind(ndf, exnodes)
  }

  # Keep only distinct rows
  edf <- unique(edf)

  # Add x and y locations for the nodes
  ndf <- ndf[order(ndf$id), ]
  ndf$x <- 1:nrow(ndf)*3
  ndf$y <- 1

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
  cdf <- subset(edf, (diff > 1 & diff < 9000) & (to != from))
  sdf <- subset(edf, diff <= 1 | diff >= 9000)
  vdf <- subset(sdf, abs(diff) >= 9990)
  sdf <- subset(sdf, abs(diff) < 9990)
  fdf <- subset(sdf, to == from)
  sdf <- subset(sdf, to != from)

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

  return(list(nodes = nodes,
              horizontal_edges = horizontal_edges,
              vertical_edges = vertical_edges,
              curved_edges = curved_edges,
              feedback_edges = feedback_edges))
}
