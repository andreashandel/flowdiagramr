#' Create data frames for plotting from model elements.
#'
#' @description
#' This function takes as input a (typically) compartmental model
#' consisting of variables/compartments and flows
#' and creates a list of data frames with label and
#' position information for plotting a flow diagram.
#' The resulting object is used as an input to
#' \code{\link{make_diagram}}, which creates a **ggplot2** based
#' diagram. Attempts to make good decisions regarding the placement of nodes,
#' flows (arrow segment), and labels are made. However, complex models with
#' complex diagrams will likely need user modification. This is documented
#' in the vignettes.
#'
#' IMPORTANT. All variables must start with an upper case letter (e.g.,
#' S, Si, or Aa). All parameters must start with a lower case letter (e.g.,
#' b, bBmax, kS, ks). All variables and parameters MUST be separated by
#' math notation (e.g., +, -, *, /).
#'
#' EXAMPLE. The following includes a parameter *b* and two variables, *S*
#' and *I*: `b*S*I`. The following includes a parameter *s* and two
#' variables, *Bg* and *Ia*: `Bg*s*Ia`.
#'
#' @param model_list A list of model elements. The list must contain at least
#'     two elements with names \code{varlabels} and \code{flows}. The
#'     \code{flows} list must contain a sub-list for each variable in
#'     the \code{varlabels} vector. For example, if the user specifies
#'     two variables in \code{model_list$varlabels}, then
#'     \code{model_list$flows} must contain two sub-lists, each containing
#'     a character vector of flows into and out of the node. Currently,
#'     this function assumes that the \code{model_list$varlabels} sub-lists
#'     are in the same order as the \code{model_list$varlabels} vector. See
#'     examples. The \code{model_list} can contain the optional argument
#'     \code{varnames} which can contain the full text for each
#'     compartment/variable and \code{varlocations} which contains a matrix
#'     that specifies the locations of the compartments/variables on an x-y
#'     grid with their desired x (columns) and y (row) locations. See examples
#'     for more.
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
#'     \item{\code{to}}: The node to which the arrow will point. That is, the node
#'     receiving the flow.
#'     \item{\code{from}}: The node from which the arrow originate. That is, the
#'     node donating the flow.
#'     \item{\code{label}}: The label of the flow. Typically a mathematical expression.
#'     \item{\code{xstart}}: The starting horizontal position of the arrow.
#'     \item{\code{ystart}}: The starting veritcal position of the arrow.
#'     \item{\code{xend}}: The ending horizontal position of the arrow.
#'     \item{\code{yend}}: The ending vertical position of the arrow.
#'     \item{\code{labelx}}: Horizontal position (midpoint) of label.
#'     \item{\code{labely}}: Vertical position (midpoint) of label.
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
#'   \item \code{curved_edges}: A data frame containing name and position
#'   information for, typically, two types of curved arrows: (1) interaction
#'   arrows that point toward a horizontal arrow or (2) a physical flow
#'   that would normally be a horizontal arrow but must bypass at least
#'   one node. The data frame has eleven (10) columns. The data frame contains
#'   the same 9 columns as the `horizontal_edges` data frame, with one addition:
#'   \itemize{
#'     \item{\code{curvature}}: The amount of curvature applied to arrow.
#'     Higher numbers indicate more curvature; 0 = straight line.
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
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' varlocations <-  matrix(data = c("S", "", "R", "", "I", "" ),
#'                         nrow = 2, ncol = 3, byrow = TRUE)
#' mymodel <- list(varlabels = varlabels, varnames = varnames,
#' flows = flows, varlocations = varlocations)
#' prepare_diagram(model_list = mymodel)
#'
#' @export


prepare_diagram <- function(model_list) {

  # check user inputs for necessary elements
  check <- check_model_list(model_list)
  if(check$bad == TRUE) {
    stop(check$msg)
  }

  # assign the nodes matrix if provided
  if(!is.null(model_list$varlocations)) {
    nodes_matrix <- model_list$varlocations
  } else {
    nodes_matrix <- NULL
  }

  # Make sure the nodes_df contains all the state variables included
  # in the model_list and no other variables.
  if(!is.null(nodes_matrix)) {
    # returns fatal error if variables do not match
    check_nodes_matrix(model_list, nodes_matrix)
  }


  # Extract relevant details from the model_list and make a matrix
  # of variables-by-flows for iterating and indexing the nodes and
  # connections. Variables will go along rows and flows along columns.

  #number of variables/compartments in model
  nvars <- length(model_list$varlabels)

  #labels for the nodes and what we expect to show up in the flow math
  varnames <- model_list$varlabels

  #set longvarnames to the full length names, if provided, otherwise
  #set to NA for  storage in data frame
  if(!is.null(model_list$varnames)) {
    longvarnames <- model_list$varnames

    # replace spaces with line breaks to create two (or more) lined
    # names that are centered in the box
    longvarnames <- gsub(" ", "\n", longvarnames)
  } else {
    # store as NAs if not provided because we need this column in
    # the nodes data frame
    longvarnames <- rep(NA, length(varnames))
  }

  #extract the flows list
  flows <- model_list$flows

  #add implicit + signs to make explicit before additional parsing
  flows <- flowdiagramr:::add_plus_signs(flows)

  #turns flow list into matrix, adding NA, found it online,
  #not sure how exactly it works (from AH and modelbuilder code base)
  #variables are along rows and flows along columns.
  flowmat <- t(sapply(flows, `length<-`, max(lengths(flows))))

  #strip leading +/- from flows and replace with no space
  flowmatred <- sub("\\+|-","",flowmat)

  #extract only the + or - signs from flows so we know the direction
  signmat <- gsub("(\\+|-).*","\\1",flowmat)

  #define nodes data frame structure if not provided by user
  # Create a node data frame
  ndf <- data.frame(
    id = 1:nvars,  # numeric id for nodes
    label = varnames,  # labels for nodes
    name = longvarnames,  # long names for labels
    row = 1  # hard code for 1 row, will be updated below, if necessary
  )


  # Split variables by rows if stratification implied by numbers at
  # the end of state variables. For example, two "S" compartments labeled
  # "S1" and "S2" will be split across rows, assuming some stratification.
  # Note that stratification up to 9 is currently supported.

  #find any characters that are NOT numbers (0-9) and replace any
  #non-number characters with blanks
  strats <- gsub("[^0-9.]", "",  varnames)
  #add implicit 1 if no strats
  strats <- ifelse(strats == "", 1, strats)
  #convert to numeric and make the stratifications encoded as rows
  ndf$row <- as.numeric(strats)


  # Create the edge data frame by looping through the variables
  # and associated flows.
  edf <- list()  #an empty list to be coerced to a data frame via rbind

  #start loop over variables (rows in the flowmatred matrix)
  for(i in 1:nrow(flowmatred)) {
    varflowsfull <- flowmat[i, ] #all flows with sign for current variable
    varflows <- flowmatred[i, ] #all flows for current variable
    varflowsigns <- signmat[i, ] #signs of flows for current variable

    #remove NA entries because these only show up to match the
    #matrix dimensions needed given the variable with the largest
    #number of flows in/out.
    varflows <- varflows[!is.na(varflows)]

    #start loop over all the flows in/out of the current variable (node)
    for(j in 1:length(varflows)) {
      currentflowfull <- varflowsfull[j] #loop through all flows for variable
      currentflow <- varflows[j] #loop through all flows for variable
      currentsign <- varflowsigns[j] #loop through all flows for variable

      # Find the variables for which the current flow appears, i.e., what
      # other rows of the matrix does it show up in.
      connectvars <- unname(which(flowmatred == currentflow, arr.ind = TRUE)[,1])

      # Extract the variable names in the flow expression
      varspars <- unique(get_vars_pars(currentflowfull))
      varfirsts <- substr(varspars, start = 1, stop = 1)  #get first letters

      #vars is now a vector of the variables that are in the flow math
      vars <- varspars[which(varfirsts %in% LETTERS)]  #variables are UPPERCASE

      #extract the numeric ids for the variables in this flow
      varsids <- ndf[which(ndf$label %in% vars), "id"]

      # add a connecting var if the expression is only in one row but
      # the flow math contains another state variable (node)
      if(length(varsids) == 1){
        if(length(unique(connectvars)) == 1) {
          if(unique(connectvars) != varsids) {
            connectvars <- c(connectvars, varsids)

            # also create a flag for adding interaction
            flag <- TRUE
          }
        }
      }


      # Assign connecting variables for inflows (+ flows)
      if(currentsign == "+") {
        # If the flow does not show up in any other rows (connectvars == 1)
        # and there are no variables in the flow math, then the only connecting
        # variable is the current (i) variable
        if(length(connectvars) == 1 & length(vars) == 0) {
          connectvars <- i
        }

        # If the flow does not show up in any other rows (connectvars == 1)
        # and there is at least one variable in the flow math, then the
        # connecting variable(s) will either be the current variable once
        # (indicating an inflow like births) or the current variable twice
        # (indicating a feedback flow)
        if(length(connectvars) == 1 & length(vars) >= 1){

          # if the current (i) variable does not show up in the flow math
          # then the connecting variable is just the current variable once,
          # indicating a independent inflow from out of the system (e.g., birth)
          if(!varnames[i] %in% vars) {
            connectvars <- i
          }

          # is the the current (i) variables shows up in the flow math, then
          # the connecting variables are the current variable twice, indicating
          # a feedback loop
          if(varnames[i] %in% vars) {
            connectvars <- c(i, i)
          }
        }

        # If there are more than one unique connecting variables, then
        # the connecting variables are simply those defined above by
        # searching the matrix of flows and/or the variables in the expression
        if(length(connectvars) > 1) {
          connectvars <- connectvars
        }
      }


      # If current sign is negative, it is an outflow and goes either to the
      # connectvar that is not equal to the current variable id (indexed by i)
      # or it goes to NA (this happens when there is an unspecified death
      # compartment, for example).
      if(currentsign == "-") {
        if(length(connectvars) == 1) {
          cn <- NA  #placeholder for unspecified compartment (deaths, typically)
        } else {
          cn <- connectvars[connectvars!=i]
        }

        # Create a data frame with all the necessary segment information
        tmp <- data.frame(from = i,
                          to = cn,
                          label = currentflow,
                          interaction = FALSE,
                          out_interaction = FALSE,
                          direct_interaction = FALSE)

        # Bind to edge data frame for flows
        edf <- rbind(edf, tmp)
      }

      # If the current sign is positive AND the flow only shows up in
      # one row of the flow matrix, then this is an inflow external to the
      # system or as a function of the current variable itself.
      if(currentsign == "+" & length(connectvars) == 1) {
        # These are typically births/imports
        if(connectvars == i) {
          tmp <- data.frame(from = NA,
                            to = i,
                            label = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE,
                            direct_interaction = FALSE)
          edf <- rbind(edf, tmp)
        }
      }

      # If the current sign is positive and the length of connecting variables
      # is equal to two, then it is :
      #   a feedback loop (1 unique connecting variable)
      #   a physical flow between two unique variables
      #   an interaction flow between to unique variables
      if(currentsign == "+" & length(connectvars) == 2) {
        # These are feedbacks of somekind
        if(length(unique(connectvars)) == 1) {
          tmp <- data.frame(from = i,
                            to = i,
                            label = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE,
                            direct_interaction = FALSE)
        } else {
          # These are physical flows between two variables
          tmp <- data.frame(from = connectvars[connectvars!=i],
                            to = i,
                            label = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE,
                            direct_interaction = FALSE)

          # update interaction flag if flag exists
          if(exists("flag")) {
            tmp$direct_interaction <- TRUE

            # remove flag to make null again
            rm(flag)
          }
        }
        edf <- rbind(edf, tmp)
      }

      # interaction flag if two variables are in the flow
      if(length(vars) > 1) {
        if(length(unique(connectvars)) > 1) {
          # this means that the flow connects two variables and both
          # are present in the flow math
          edf[nrow(edf), "interaction"] <- TRUE
        } else {
          # this means that the flow comes from or goes to somewhere out
          # of the system, and only 1 variable is included in the
          # flow math. this is designated as an "out_interaction"
          edf[nrow(edf), "out_interaction"] <- TRUE
        }
      }

    }  #end flow loop
  }  #end variable loop

  # Keep only distinct rows; duplication occurs because one variable's
  # inflow can be another variable's outflow, but we only want these once
  # in the data frame for edges (segments/arrows/flows).
  edf <- unique(edf)

  # Parse the meaning of duplicate labels. Usually this is a complex mix
  # of a direct, physical flows and interactions from several other
  # state variables. We assume that the "main" flow among the "auxilliary"
  # duplicate flows is the one that traverses left-to-right (e.g., 1 to 2)
  # with the smallest gap and has no interaction flags.
  dups <- as.matrix(table(edf$label))  # tally the occurences of each flow
  dupids <- rownames(dups)[which(dups[,1] > 1)]  # grab the one with >1 occurence
  if(length(dupids) > 0) {
    flowdups <- subset(edf, label %in% dupids)  # take a subset of the edge data frame
    edf <- subset(edf, !(label %in% dupids))  # restrict edf to non-duplicate flows
    flowdups <- subset(flowdups, sign(to-from) == 1)  # keep left-to-right flows
    flowdups <- subset(flowdups, interaction == FALSE &
                         out_interaction == FALSE &
                         direct_interaction == FALSE)  # drop interactions
    if(nrow(flowdups) == 0) {
      stop(paste0("There are duplicate flows across variables that failed to\n",
                  "parse easily. Are there '+' signs where you intended\n",
                  "'-' signs, or vice versa."))
    }
    diffs <- with(flowdups, to - from)  # calc difference between nodes
    mainid <- which(diffs == min(diffs))  # keep the minimum node diff as main flow
    maindup <- flowdups[mainid, ]  # extract just the main flow for physical flow
    intdup <- flowdups[mainid, ]  # extract again for interaction flow, which is parsed later on
    intdup$interaction <- TRUE  # set interaction flag to TRUE
    edf <- rbind(edf, maindup, intdup)
  }

  # Duplicate rows with out_interaction == TRUE to assign the interaction
  # flag and then remove the out_interaction flag. This is done to
  # achieve appropriate labeling. We want the physical flow to have no label
  # and for the interaction arrow to carry to the label.
  repdf <- subset(edf, out_interaction == TRUE)
  if(nrow(repdf) != 0) {  # avoids errors if no rows
    repdf$interaction <- TRUE  # set this to TRUE for linetypes
    repdf$out_interaction <- NULL  # remove this now
    edf[which(edf$out_interaction == TRUE), "label"] <- ""  # take away the label for the physical flow
    edf$out_interaction <- NULL  # remove this now
    edf <- rbind(edf, repdf)  # tack them together
  }

  # remove out_interaction completely now that interaction is
  # appropriately flagged with correct labeling
  edf$out_interaction <- NULL


  # Break edges apart into:
  #   direct flows
  #   interactions to meet at edges
  #   the flows resulting from interactions
  # All flows are treated seperately because their start and end positions
  # depend on state variables in different ways.

  edf$linkto <- NA  #empty column for interaction flows, but needed for binding
  edf$linkfrom <- NA  #empty column for interaction flows, but needed for binding
  ints <- subset(edf, interaction == TRUE)
  edf <- subset(edf, interaction == FALSE)

  # If there are interactions, then duplicate them and reassign the to/from
  # columns such that we have two segments for each interaction flagged
  # row: (1) the physical flow with from/to for donating and receiving
  # varables and (2) an interaction flow with an NA for the to column
  # and from is the non-donating variable in the flow math. A new "link"
  # column is added to identify which variable is linking the interaction
  # (the link is the "from" variable in the physical flow).
  if(nrow(ints) > 0) {  # avoids errors if no interactions
    intflows <- ints  # duplicate
    intflows$label <- ""  # strip the label from the physical flow
    intflows <- unique(intflows)  # just keep unique flows
    intflows$interaction <- FALSE  # reset interaction to false b/c a main flow now

    # Redefine the from, to, and link columns for the interaction
    # arrows. "to" is NA until updated to meet at the center
    # of the physical flow arrow.
    for(i in 1:nrow(ints)) {
      tmp <- ints[i, ]
      v <- get_vars_pars(tmp$label)  #strips away math, leaving just letters
      vf <- substr(v, start = 1, stop = 1)  #get first letters
      v <- v[which(vf %in% LETTERS)]  #subset to upper case VARIABLES
      ids <- subset(ndf, label %in% v)[ , "id"]  #extract the relevant numeric ids

      if(is.na(ints[i, "to"])){
        # If the receiving node is NA, then this is an interaction
        # with a feedback flow, meaning the "link node" is also NA.
        ints[i, "linkfrom"] <- NA
        ints[i, "linkto"] <- NA
      } else if(ints[i, "to"] == ints[i, "from"]) {
        # If the to and from nodes are tha same, this is a feedback
        # flow that does not require a link, so NAs.
        ints[i, "linkfrom"] <- NA
        ints[i, "linkto"] <- NA
      } else {
        # In all other cases, the "link from" node will be the current
        # "from" node and the "link to" node will be the current "to" node.
        ints[i, "linkfrom"] <- tmp$from
        ints[i, "linkto"] <- tmp$to
      }

      # Redefine the "from" node as the other node in this interaction.
      # PACKAGE CURRENTLY CANNOT HANDLE MULTIPLE INTERACTIONS IN
      # A SINGLE FLOW -- CAN ONLY HAVE TWO VARIABLES PRESENT
      ints[i, "from"] <- ids[which(ids != tmp$from)]
      ints[i, "to"] <- NA  # set NA for "to" node for all interactions
    }

    # Recombine the edge data frame
    edf <- rbind(edf, ints, intflows)
  }

  # Keep only distinct rows
  edf <- unique(edf)


  # Make dummy compartment for all flows in and out of the system.
  # Dummy compartments are given ids that start with three numbers
  # that identify the type of dummy:
  #   999* = dummy compartments for flows out of the system (e.g., death pool)
  #   -999* = dummy comparments for flows in the system (e.g., birth pool)
  #   555* = dummy compartments for interaction links
  # These are just used to create empty nodes for arrows to originate from
  # or go to.

  # Out of the system
  outdummies <- NULL
  numnas <- length(edf[is.na(edf$to) & edf$interaction == FALSE, "to"])
  if(numnas > 0) {
    outdummies <- as.numeric(paste0("999", c(1:numnas)))
    edf[is.na(edf$to) & edf$interaction == FALSE, "to"] <- outdummies
  }

  # In to the system
  indummies <- NULL
  numnas <- length(edf[is.na(edf$from) & edf$interaction == FALSE, "from"])
  if(numnas > 0) {
    indummies <- as.numeric(paste0("-999", c(1:numnas)))
    edf[is.na(edf$from) & edf$interaction == FALSE, "from"] <- indummies
  }

  # Make dummy compartment for "links" in interactions
  linkdummies <- NULL
  numlinks <- length(edf[is.na(edf$to) &
                           edf$interaction == TRUE &
                           !is.na(edf$linkto), "to"])
  if(numlinks > 0) {
    linkdummies <- as.numeric(paste0("555", c(1:numlinks)))
    edf[is.na(edf$to) & edf$interaction == TRUE, "to"] <- linkdummies
  }


  # Add dummy compartments to nodes dataframe
  if(is.numeric(outdummies) | is.numeric(indummies) | is.numeric(linkdummies)) {
    exnodes <- data.frame(id = c(outdummies, indummies, linkdummies),
                          label = "",
                          name = NA,
                          row = 1)
    exnodes[setdiff(names(ndf), names(exnodes))] <- NA
    ndf <- rbind(ndf, exnodes)
  }


  # Add midpoint locations for nodes
  # Here we just iterate over the nodes and take their position in the
  # data frame rows and multiply by 3 (e.g., 1*3, 2*3, 3*3) to get
  # arbitrary x positions. y positions take the row id and multiply by
  # negative 2, meaning that additional rows always go below the row that
  # was previously defined.
  # If the nodes_matrix is provided, then the same procedure is applied, but
  # based on the row and column positions provided by the user.
  if(is.null(nodes_matrix)) {
    ndf <- ndf[order(ndf$id), ]
    ndf$x <- NA
    ndf$y <- NA
    for(rid in unique(ndf$row)) {
      ndf[which(ndf$row == rid), "x"] <- (1:nrow(ndf[which(ndf$row == rid), ])*3)-3
      ndf[which(ndf$row == rid), "y"] <- (as.numeric(rid) * -2)+2
    }
  } else {
    ny <- (1:nrow(nodes_matrix) * -2)+2
    nx <- (1:ncol(nodes_matrix) * 3)-3
    for(nid in varnames) {
      pos <- which(nodes_matrix == nid, arr.ind = TRUE)
      ndf[which(ndf$label == nid), "x"] <- nx[pos[1, 2]]
      ndf[which(ndf$label == nid), "y"] <- ny[pos[1, 1]]
    }
  }

  # Add xmin/max and ymin/max columns for node rectangles
  # I use a 0.5 offset in both directions, creating a 1x1 sized square.
  xoff <- 0.5  # default
  yoff <- 0.5  # default
  ndf$xmin <- with(ndf, x - xoff)
  ndf$xmax <- with(ndf, x + xoff)
  ndf$ymin <- with(ndf, y - yoff)
  ndf$ymax <- with(ndf, y + yoff)


  # update inflow node positions from nowhere (e.g. births)
  inflownodes <- subset(ndf, id < -9990)$id
  for(id in inflownodes) {
    newxyid <- edf[which(edf$from == id), "to"]
    newxy <- ndf[which(ndf$id == newxyid), c("x", "y")]
    newxy$y <- newxy$y + 2  # above the variable
    ndf[which(ndf$id == id), c("x", "y")] <- newxy

    # set min/max to midpoint for ease because these are not actually
    # drawn, therefore rectangle boundaries do not need to be accurate
    ndf[which(ndf$id == id), c("xmin", "ymin")] <- newxy
    ndf[which(ndf$id == id), c("xmax", "ymax")] <- newxy
  }

  # update outflow node positions to nowhere
  outflownodes <- subset(ndf, id > 9990)$id
  for(id in outflownodes) {
    newxyid <- edf[which(edf$to == id), "from"]
    newxy <- ndf[which(ndf$id == newxyid), c("x", "y")]
    newxy$y <- newxy$y - 2  # below the variable
    ndf[which(ndf$id == id), c("x", "y")] <- newxy

    # set min/max to midpoint for ease because these are not actually
    # drawn, therefore rectangle boundaries do not need to be accurate
    ndf[which(ndf$id == id), c("xmin", "ymin")] <- newxy
    ndf[which(ndf$id == id), c("xmax", "ymax")] <- newxy
  }

  # update invisible interaction link nodes, i.e., nodes that need to sit
  # at the midpoint of some other arrow, but not be drawn
  linknodes <- subset(ndf, id > 5550 & id < 9990)$id
  for(id in linknodes) {
    start <- edf[which(edf$to == id), "linkfrom"]
    end <- edf[which(edf$to == id), "linkto"]
    newx1 <- ndf[which(ndf$id == start), "x"]
    newx2 <- ndf[which(ndf$id == end), "x"]
    newx <- (newx1+newx2)/2  # midpoint of the physical arrow
    newy1 <- ndf[which(ndf$id == start), "y"]
    newy2 <- ndf[which(ndf$id == end), "y"]
    newy <- (newy1+newy2)/2  # midpoint of the physical arrow
    ndf[which(ndf$id == id), c("x", "y")] <- c(newx, newy)

    # set min/max to midpoint for ease because these are not actually
    # drawn, therefore rectangle boundaries do not need to be accurate
    ndf[which(ndf$id == id), c("xmin", "ymin")] <- c(newx, newy)
    ndf[which(ndf$id == id), c("xmax", "ymax")] <- c(newx, newy)
  }

  # Subset out interactions to in/out flows
  extints <- subset(edf, interaction == TRUE & is.na(linkto))
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


  # Create segment coordinates by merging with node locations
  edf <- merge(edf, ndf[ , c("x", "y", "id")], by.x = "from", by.y = "id")
  edf <- merge(edf, ndf[ , c("x", "y", "id")], by.x = "to", by.y = "id",
               suffixes = c("start", "end"))

  # label locations are mid points
  edf$xmid <- with(edf, (xend + xstart) / 2)
  edf$ymid <- with(edf, (yend + ystart) / 2) + 0.25  # label slightly above the arrrow
  edf$diff <- with(edf, abs(to-from))

  # Get midpoints of in/out segments for external interactions "to" locations
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

  # Add offsets to straight edges. The offset depends on the variation
  # in x and y. If xstart == xend, then this is a vertical alignment and
  # y offsets are applied. If ystart == yend, then this is a
  # horizontal alignment and x offsets are applied. If vertical, the midpoints
  # are also updated to move the label to the right of the arrow.
  if(nrow(sdf) != 0) {
    for(i in 1:nrow(sdf)) {
      if(sdf[i, "xstart"] == sdf[i, "xend"]) {
        sdf[i, "ystart"] <- sdf[i, "ystart"] - yoff
        sdf[i, "yend"] <- sdf[i, "yend"] + yoff
        sdf[i, "ymid"] <- (sdf[i, "yend"] + sdf[i, "ystart"]) / 2
        sdf[i, "xmid"] <- ((sdf[i, "xend"] + sdf[i, "xstart"]) / 2) + 0.25  # label to right
      } else {
        sdf[i, "xstart"] <- sdf[i, "xstart"] + xoff
        sdf[i, "xend"] <- sdf[i, "xend"] - xoff
      }
    }
  }

  # The same logic above applies to curved arrows with an interaction.
  # Add offsets to straight edges. The offset depends on the variation
  # in x and y. If xstart == xend, then this is a vertical alignment and
  # y offsets are applied. If ystart == yend, then this is a
  # horizontal alignment and x offsets are applied. If vertical, the midpoints
  # are also updated to move the label to the right of the arrow.
  if(nrow(cdf) != 0) {
    for(i in 1:nrow(cdf)) {
      if(cdf[i, "interaction"] == TRUE &
         cdf[i, "direct_interaction"] == FALSE) {
        if(cdf[i, "xstart"] == cdf[i, "xend"]) {
          cdf[i, "xstart"] <- cdf[i, "xstart"] + xoff
          cdf[i, "ystart"] <- cdf[i, "ystart"] - yoff
        }
      }
    }
  }

  # Set the curvature using internal function
  if(nrow(cdf) > 0) {
    cdf <- set_curvature(cdf, ndf)
  }

  # Update start and end points for curved arrows that bypass nodes,
  # these need to start/end from the top/bottom of the nodes. If the
  # arrow goes right to left, it will start and end on top of nodes. If
  # the arrow goes left to right, it will start and end on the bottom of
  # nodes. Similar logic applies if the diagram is positioned vertically
  # rather than horizontally.
  if(nrow(cdf) > 0) {
    for(i in 1:nrow(cdf)) {
      if(cdf[i, "interaction"] == FALSE &
         cdf[i, "direct_interaction"] == FALSE) {
        if(sign(cdf[i, "diff"]) == 1) {
          cdf[i, "xstart"] <- cdf[i, "xstart"] + xoff
          cdf[i, "xend"] <- cdf[i, "xend"] - xoff
          cdf[i, "ystart"] <- cdf[i, "ystart"] + yoff
          cdf[i, "yend"] <- cdf[i, "yend"] + yoff
          cdf[i, "labely"] <- cdf[i, "labely"] + yoff
        }
        if(sign(cdf[i, "diff"]) == -1) {
          cdf[i, "xstart"] <- cdf[i, "xstart"] - xoff
          cdf[i, "xend"] <- cdf[i, "xend"] + xoff
          cdf[i, "ystart"] <- cdf[i, "ystart"] - yoff
          cdf[i, "yend"] <- cdf[i, "yend"] - yoff
          cdf[i, "labely"] <- cdf[i, "labely"] - yoff
        }
      }
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

  # update vertical edges to go in and out at angles
  vdf <- make_vdf_angled(vdf)

  # update vertical edges to avoid overlaps
  vdf <- fix_arrow_pos(vdf)

  # set to/from columns to NA if value is not in node dataframe
  sdf <- set_node_to_na(sdf, ndf)
  vdf <- set_node_to_na(vdf, ndf)
  cdf <- set_node_to_na(cdf, ndf)
  fdf <- set_node_to_na(fdf, ndf)

  # remove rows with no location information
  sdf <- remove_na_rows(sdf)
  vdf <- remove_na_rows(vdf)
  cdf <- remove_na_rows(cdf)
  fdf <- remove_na_rows(fdf)

  # convert direct interaction to flag to regular interaction flag,
  # now only relevant for plotting
  sdf <- update_interactions(sdf)
  vdf <- update_interactions(vdf)
  cdf <- update_interactions(cdf)
  fdf <- update_interactions(fdf)

  # update all to and froms such that each is the variable label
  sdf <- update_tofroms(sdf, ndf)
  vdf <- update_tofroms(vdf, ndf)
  cdf <- update_tofroms(cdf, ndf)
  fdf <- update_tofroms(fdf, ndf)

  # rename data frames for exporting
  ndf$labelx <- ndf$x
  ndf$labely <- ndf$y
  nodes <- subset(ndf, select = -c(id, row, x, y))

  sdf$labelx <- sdf$xmid
  sdf$labely <- sdf$ymid
  horizontal_edges <- subset(sdf, select = -c(diff, linkto, linkfrom, xmid, ymid))


  vdf$labelx <- vdf$xmid
  vdf$labely <- vdf$ymid
  vertical_edges <- subset(vdf, select = -c(diff, interaction, linkto,
                                            linkfrom, xmid, ymid))

  curved_edges <- subset(cdf, select = -c(diff, linkto, linkfrom, ymid, xmid, row))

  fdf$labelx <- fdf$xmid
  fdf$labely <- fdf$ymid
  fdf$labely <- fdf$labely + 0.85  # this offset makes the label a little above the big curved arrow
  # last, adjust the xs and ys to get arrow above and feeding back into the node
  fdf$xstart <- fdf$xstart-0.25
  fdf$ystart <- fdf$ystart+0.5
  fdf$xend <- fdf$xend+0.25
  fdf$yend <- fdf$yend+0.5
  feedback_edges <-  subset(fdf, select = -c(diff, linkto, linkfrom, interaction,
                                             xmid, ymid))


  return(list(nodes = nodes,
              horizontal_edges = horizontal_edges,
              vertical_edges = vertical_edges,
              curved_edges = curved_edges,
              feedback_edges = feedback_edges))
}
