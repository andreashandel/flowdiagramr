#' Create data frames for plotting from model elements.
#'
#' @param input_list A list of model elements. The list must contain at least
#'     two elements with names \code{varlabels} and \code{flows}. The
#'     \code{flows} list must contain a sub-list for each variable in
#'     the \code{varlabels} vector. For example, if the user specifies
#'     two variables in \code{input_list$varlabels}, then
#'     \code{input_list$flows} must contain two sub-lists, each containing
#'     a character vector of flows into and out of the node. Currently,
#'     this function assumes that the \code{input_list$varlabels} sub-lists
#'     are in the same order as the \code{input_list$varlabels} vector. See
#'     examples. The \code{input_list} can contain any other elements that
#'     the user might deem useful (e.g., metadata/comments), but only the
#'     \code{varlabels} and \code{flows} are used by this function.
#' @param nodes_df A data frame with user-specified node locations. The data
#'     frame must contain the following columns: \code{id}, \code{label},
#'     \code{x}, and \code{y}. An internal function will add the necessary
#'     \code{row} column based on the values for \code{y}. See vignettes for
#'     examples of the data frame structure.
#'
#' @return A list of five data frames:
#' \itemize{
#'   \item \code{nodes}: A data frame containing the node (state variable) ids,
#'   labels, and positions. Position is defined with the \code{x} and
#'   \code{y} columns, which define the center of the node squares. The
#'   ggplot2 function \code{geom_tile} is used for placement of nodes and
#'   the height and width are set to 1 to generate a perfect square centered
#'   on \code{x} and \code{y}. The \code{row} column indicates which row
#'   the node is placed on; this is also evident from the \code{y} values.
#'   \code{id} is a numeric id for the node, which shows up in the edge
#'   data frames for defining connects (see below).
#'
#'   \item \code{horizontal_edges}: A data frame containing name and position
#'   information for straight flows from one node to an adjacent node. The
#'   data frame contains nine (9) columns:
#'   \itemize{
#'     \item{\code{to}}: The node id to which the arrow will point. That is, the node
#'     receiving the flow.
#'     \item{\code{from}}: The node id from which the arrow originate. That is, the
#'     node donating the flow.
#'     \item{\code{label}}: The label of the flow. Typically a mathematical expression.
#'     \item{\code{xstart}}: The starting horizontal position of the arrow.
#'     \item{\code{ystart}}: The starting veritcal position of the arrow.
#'     \item{\code{xend}}: The ending horizontal position of the arrow.
#'     \item{\code{yend}}: The ending vertical position of the arrow.
#'     \item{\code{xmid}}: The horizontal midpoint of the arrow. Used for label
#'     placement.
#'     \item{\code{ymid}}: The vertical midpoint of the arrow. Used for label
#'     placement.
#'   }
#'
#'   \item \code{vertical_edges}: A data frame containing name and position
#'   information for flows that arrive from out of the system or leave the
#'   system. That is, flows that either (1) do not come from a node specified
#'   in the nodes data frame or (2) exit the system without connecting to
#'   another node. The data frame contains the same columns as
#'   \code{horizontal_edges}. Note that either the \code{to} or \code{from}
#'   value will be NA in each row (if rows are present). These flows arrive
#'   or leave at 45 degree angles, despite the name of the data frame implying
#'   a vertical entrance or exit.
#'
#'   \item {curved_edges}: A data frame containing name and position
#'   information for, typically, two types of curved arrows: (1) interaction
#'   arrows that point toward a horizontal arrow or (2) a physical flow
#'   that would normally be a horizontal arrow but must bypass at least
#'   one node. The data frame has eleven (11) columns:
#'   \itemize{
#'     \item{\code{to}}: The node id to which the arrow will point. That is, the node
#'     receiving the flow.
#'     \item{\code{from}}: The node id from which the arrow originate. That is, the
#'     node donating the flow.
#'     \item{\code{label}}: The label of the flow. Typically a mathematical expression.
#'     \item{interaction}: A logical indicating whether the flow represents
#'     an interaction between two or more nodes. If \code{TRUE}, the arrow
#'     is drawn as dashed by default.
#'     \item{\code{xstart}}: The starting horizontal position of the arrow.
#'     \item{\code{ystart}}: The starting veritcal position of the arrow.
#'     \item{\code{xend}}: The ending horizontal position of the arrow.
#'     \item{\code{yend}}: The ending vertical position of the arrow.
#'     \item{\code{curvature}}: The amount of curvature applied to arrow.
#'     Higher numbers indicate more curvature; 0 = straight line.
#'     \item{\code{row}}: The row on which the arrow is connecting nodes;
#'      this is also evident from the \code{ystart} and \code{yend} values.
#'     \item{\code{labelx}}: Horizontal position of label.
#'     \item{\code{labely}}: Vertical position of label.
#'   }
#'
#'   \item{\code{feedback_edges}}: A data frame containing name and position
#'   information for arrows indicating a feedback into the same node. The
#'   data frame contains the same columns as the \code{horizontal_edges}
#'   data frame. Note that the \code{to} and \code{from} columns should have
#'   the same values for feedback edges.
#'
#' }
#'
#' @examples
#' varlabels <- c("S","I","R")
#' varnames <- c("Susceptible","Infected","Recovered")  # optional
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("+b*S*I","-g*I"),
#'               R_flows = c("+g*I"))
#' mymodel <- list(varlabels = varlabels, varnames = varnames, flows = flows)
#' prepare_diagram(input_list = mymodel)
#'
#' @export


prepare_diagram <- function(input_list, nodes_df = NULL) {
  # TODO error checking

  # Make sure the nodes_df contains all the state variables included
  # in the input_list and no other variables.
  if(!is.null(nodes_df)) {
    # returns fatal error if variables do not match
    check_nodes_df(input_list, nodes_df)
  }


  # Extract relevant details from the input_list and make a matrix
  # of variables X flows for iterating and indexing the nodes and
  # connections.
  nvars <- length(input_list$varlabels)  #number of variables/compartments in model
  varnames <- input_list$varlabels

  if(!is.null(input_list$varnames)) {
    vartext <- input_list$varnames
  }

  flows <- input_list$flows

  #turns flow list into matrix, adding NA, found it online,
  #not sure how exactly it works
  flowmat <- t(sapply(flows, `length<-`, max(lengths(flows))))
  flowmatred <- sub("\\+|-","",flowmat)   #strip leading +/- from flows
  signmat <- gsub("(\\+|-).*","\\1",flowmat) #extract only the + or - signs from flows so we know the direction


  if(is.null(nodes_df)) {
    # Create a node data frame
    ndf <- data.frame(
      id = 1:nvars,  # number of nodes
      label = varnames,  # labels of nodes
      row = 1  # hard code for 1 row, will be updated below, if necessary
    )

    # Split variables by rows if stratification implied by numbers
    strats <- gsub("[^0-9.]", "",  varnames)
    strats <- ifelse(strats == "", 1, strats)
    ndf$row <- as.numeric(strats)
  } else {
    ndf <- add_rowid(nodes_df)
  }


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
                          interaction = FALSE,
                          out_interaction = FALSE)

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
                            interaction = FALSE,
                            out_interaction = FALSE)
          edf <- rbind(edf, tmp)
        }
      }
      if(currentsign == "+" & length(connectvars) == 2) {
        if(length(unique(connectvars)) == 1) {
          tmp <- data.frame(from = i,
                            to = i,
                            label = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE)
        } else {
          tmp <- data.frame(from = connectvars[connectvars!=i],
                            to = i,
                            label = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE)
        }
        edf <- rbind(edf, tmp)
      }

      # interaction flag if two variables are in the flow
      if(length(vars) > 1 & length(unique(connectvars)) > 1) {
        edf[nrow(edf), "interaction"] <- TRUE
      }
      if(length(vars) > 1 & length(unique(connectvars)) <= 1) {
        edf[nrow(edf), "out_interaction"] <- TRUE
      }

    }  #end flow loop
  }  #end variable loop

  # Keep only distinct rows
  edf <- unique(edf)

  # Duplicate rows with out_interaction == TRUE
  repdf <- subset(edf, out_interaction == TRUE)
  if(nrow(repdf) != 0) {
    repdf$interaction <- TRUE
    repdf$out_interaction <- NULL
    edf[which(edf$out_interaction == TRUE), "label"] <- ""
    edf$out_interaction <- NULL
    edf <- rbind(edf, repdf)
  }

  # remove out_interaction completely
  edf$out_interaction <- NULL


  # Break edges apart into:
  #   direct flows
  #   interactions to meet at edges
  #   the flows resulting from interactions
  edf$link <- NA  #empty column for interaction flows, but needed for binding
  ints <- subset(edf, interaction == TRUE)
  edf <- subset(edf, interaction == FALSE)

  if(nrow(ints) > 0) {
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

      if(is.na(ints[i, "to"])){
        ints[i, "link"] <- NA
      } else if(ints[i, "to"] == ints[i, "from"]) {
        ints[i, "link"] <- NA
      } else {
        ints[i, "link"] <- tmp$from
      }

      ints[i, "from"] <- ids[which(ids != tmp$from)]
      ints[i, "to"] <- NA
    }

    # Recombine the edge data frame
    edf <- rbind(edf, ints, intflows)
  }

  # Keep only distinct rows
  edf <- unique(edf)


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
  numlinks <- length(edf[is.na(edf$to) &
                           edf$interaction == TRUE &
                           !is.na(edf$link), "to"])
  if(numlinks > 0) {
    linkdummies <- as.numeric(paste0("555", c(1:numlinks)))
    edf[is.na(edf$to) & edf$interaction == TRUE, "to"] <- linkdummies
  }


  # Add dummy compartments to nodes dataframe
  if(is.numeric(outdummies) | is.numeric(indummies) | is.numeric(linkdummies)) {
    exnodes <- data.frame(id = c(outdummies, indummies, linkdummies),
                          label = "",
                          row = 1)  # TODO
    exnodes[setdiff(names(ndf), names(exnodes))] <- NA
    ndf <- rbind(ndf, exnodes)
  }

  # Add x and y locations for the nodes
  if(is.null(nodes_df)) {
    ndf <- ndf[order(ndf$id), ]
    ndf$x <- NA
    ndf$y <- NA
    for(rid in unique(ndf$row)) {
      ndf[which(ndf$row == rid), "x"] <- 1:nrow(ndf[which(ndf$row == rid), ])*3
      ndf[which(ndf$row == rid), "y"] <- as.numeric(rid) * -2
    }
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

  # update invisible interaction link nodes
  linknodes <- subset(ndf, id > 5550 & id < 9990)$id
  for(id in linknodes) {
    start <- edf[which(edf$to == id), "link"]
    end <- edf[which(edf$to == id), "from"]
    newx1 <- ndf[which(ndf$id == start), "x"]
    newx2 <- ndf[which(ndf$id == end), "x"]
    newx <- (newx1+newx2)/2
    newy <- ndf[which(ndf$id == start), "y"]
    ndf[which(ndf$id == id), c("x", "y")] <- c(newx, newy)
  }

  # update node positions that overlap
  # xys <- ndf[ , c("x", "y")]
  # overlapids <- which(duplicated(xys) | duplicated(xys, fromLast = TRUE))
  # numoverlap <- length(overlapids)
  # if(numoverlap > 0) {
  #   newxs <- seq(0.1, 1.9, by = 0.15)
  #   newxids <- c(6,5,4,3,2,1,0,1,2,3,4,5,6) + 1
  #   xmults <- newxs[which(newxids==numoverlap)]
  #   for(i in 1:numoverlap) {
  #     oldx <- ndf[overlapids[i], "x"]
  #     newx <- oldx * xmults[i]
  #     ndf[overlapids[i], "x"] <- newx
  #   }
  # }

  # Subset out interactions to in/out flows
  extints <- subset(edf, interaction == TRUE & is.na(link))
  if(nrow(extints) > 0) {
    for(i in 1:nrow(extints)) {
      tmp <- extints[i, ]
      v <- get_vars_pars(tmp$label)
      vf <- substr(v, start = 1, stop = 1)  #get first letters
      v <- v[which(vf %in% LETTERS)]
      ids <- subset(ndf, label %in% v)[ , "id"]
      id <- ids[which(ids != tmp$from)]
      extints[i, "to"] <- id
    }
  }


  # Create segment coordinates
  edf <- merge(edf, ndf[ , c("x", "y", "id")], by.x = "from", by.y = "id")
  edf <- merge(edf, ndf[ , c("x", "y", "id")], by.x = "to", by.y = "id",
               suffixes = c("start", "end"))
  edf$xmid <- with(edf, (xend + xstart) / 2)
  edf$ymid <- with(edf, (yend + ystart) / 2) + 0.25
  edf$diff <- with(edf, abs(to-from))

  # Get midpoints of in/out segments for extints "to"

  ## OK. Need to merge in nodes to get the start positions and
  ## the edges to get the ending position (midpoints of edges with no
  ## no label)
  if(nrow(extints) > 0) {
    extlinks <- subset(edf, label == "")
    extints <- merge(extints, ndf[,c("x", "y", "id")], by.x = "from", by.y = "id")
    colnames(extints)[which(colnames(extints) == "x")] <- "xstart"
    colnames(extints)[which(colnames(extints) == "y")] <- "ystart"
    extints$xend <- NA
    extints$yend <- NA
    for(i in 1:nrow(extints)) {
      tmp1 <- extints[i, ]
      tmp1[ , c("xend", "yend")] <- NULL
      tmp2 <- extlinks[which(tmp1$to == extlinks$from), ]
      if(tmp2$to == tmp2$from) {
        tmp3 <- merge(tmp1, tmp2[, c("xend", "yend", "from")],
                         by.x = "to", by.y = "from")
        tmp3$yend <- tmp3$yend + 0.75
        tmp3$xend <- tmp3$xend + 0.17
      } else {
        tmp3 <- merge(tmp1, tmp2[, c("xmid", "ymid", "from")],
                      by.x = "to", by.y = "from")
      }
      colnames(tmp3) <- c("to", "from", "label", "interaction", "link",
                             "xstart", "ystart", "xend", "yend")
      extints[i, ] <- tmp3
    }
    # extints <- merge(extints, extlinks[, c("xmid", "ymid", "from")],
    #                  by.x = "to", by.y = "from")
    # colnames(extints) <- c("to", "from", "label", "interaction", "link",
    #                        "xstart", "ystart", "xend", "yend")
    extints$xmid <- with(extints, (xend + xstart) / 2)
    extints$ymid <- with(extints, (yend + ystart) / 2) + 0.25
    extints$diff <- with(extints, abs(to-from))

    edf <- rbind(edf, extints)
  }


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

  # Set the curvature using internal function
  if(nrow(cdf) > 0) {
    cdf <- set_curvature(cdf, ndf)
  }

  # test to make sure splits are unique and sum up to original data frame
  test <- nrow(vdf) + nrow(sdf) + nrow(cdf) + nrow(fdf) == nrow(edf)
  if(!test) {
    stop(paste0("Edges data frame is not splitting appropriately.\n",
                "       Contact package maintainer."))
  }

  # now drop "hidden" nodes without labels
  ndf <- subset(ndf, label != "")

  # update vertical edges to go in and out at angles
  vdf <- make_vdf_angled(vdf)

  # update vertical edges to avoid overlaps
  vdf <- fix_arrow_pos(vdf)

  # set to/from columns to NA if value is not in node dataframe
  sdf <- set_node_to_na(sdf, ndf)
  vdf <- set_node_to_na(vdf, ndf)
  cdf <- set_node_to_na(cdf, ndf)
  fdf <- set_node_to_na(fdf, ndf)

  # rename data frames for exporting
  nodes <- ndf
  horizontal_edges <- subset(sdf, select = -c(diff, interaction, link))
  vertical_edges <- subset(vdf, select = -c(diff, interaction, link))
  curved_edges <- subset(cdf, select = -c(diff, link, ymid, xmid))
  feedback_edges <- subset(fdf, select = -c(diff, link, interaction))


  return(list(nodes = nodes,
              horizontal_edges = horizontal_edges,
              vertical_edges = vertical_edges,
              curved_edges = curved_edges,
              feedback_edges = feedback_edges))
}
