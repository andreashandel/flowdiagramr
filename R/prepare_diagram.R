#' Create data frames for plotting from model elements.
#'
#' @description
#' This function takes as input a (typically) compartmental model
#' consisting of variables/compartments and flows
#' and creates a list of data frames with label and
#' position information for plotting a flow diagram.
#' The resulting object is used as an input to
#' \code{\link{make_diagram}}, which creates a **ggplot2** based diagram.
#' Attempts to make decent decisions regarding the placement of nodes (boxes),
#' flows (arrows), and labels are made. However, complex models with
#' complex diagrams will likely need user modification. This is documented
#' in the vignettes.
#'
#' @param model_list A list of model elements. This list is required and
#' must contain these two elements:
#' \itemize{
#' \item `varlabels`: A character vector with labels for each variable.
#' \item `flows`: A list that is the same length as `varlabels`. Each sub-list
#'     element is a character vector of any length specifying the flows into
#'     and out of the variable. Note that **flowdiagramr** assumes that the
#'     order of `flows` and `varlabels` match.
#' \item IMPORTANT: All varlabel entries must start with an upper case letter,
#' followed by any combination of letters and numbers (e.g.,
#' S, Si, or Aml2). All parameters contained in the flows
#' must start with a lower case letter
#' followed by any combination of letters and numbers (e.g.,
#' b, bBmax, kS, p21S). All variables and parameters MUST be separated by
#' math notation (e.g., +, -, *, /).
#' \item See examples and details below and vignettes.
#' }
#'
#' @param model_settings A list of optional model settings. The following
#'     elements are supported and default values are provided:
#' \itemize{
#' \item `varnames`: Vector of strings containing labels, one  for each variable.
#'                   Default is `NULL`.
#' \item `varbox_x_scaling`: A scalar that changes the default width of
#'     variable boxes. For example, `varbox_x_scaling = 1.5` makes each box
#'     1.5 times the default width.
#' \item `varbox_y_scaling`: A scalar that changes the default height of
#'     variable boxes. For example, `varbox_y_scaling = 1.5` makes each box
#'     1.5 times the default height.
#' \item `varspace_x_scaling`: A scalar that changes the default spacing between
#'     variable boxes in the x dimension. For example, `varspace_x_scaling = 1.5`
#'     makes each box 1.5 times farther apart in the x dimension than the
#'     default spacing.
#' \item `varspace_y_scaling`: A scalar that changes the default spacing between
#'     variable boxes in the y dimension. For example, `varspace_y_scaling = 1.5`
#'     makes each box 1.5 times farther apart in the y dimension than the
#'     default spacing.
#' }
#'
#' @return A list of two data frames:
#' \itemize{
#'   \item `variables`: A data frame containing information for all variables.
#'   The data frame contains these columns:
#'
#'   \itemize{
#'     \item `label`: The variable label as provided in the model specification.
#'     \item `name`: If provided, the full text for each variable.
#'     \item `xmin`: Left edge location of box.
#'     \item `xmax`: Right edge location of box.
#'     \item `ymin`: Lower edge of location box.
#'     \item `ymax`: Upper edge of location box.
#'     \item `xlabel`: Horizontal position (midpoint) of label.
#'     \item `ylabel`: Vertical position (midpoint) of label.
#'     \item `plot_label`: The text to be written into the box.
#'     \item `color`: Default outline color for the box.
#'     \item `fill`: Default fill color for the box.
#'     \item `label color`: Default color for text label.
#'     \item `label_size`: Size of text to be written into the box.
#'   }
#'
#'   \item `flows`: A data frame containing information for all flows.
#'   The data frame contains these columns:
#'   \itemize{
#'     \item `to`: The variable to which the arrow will point. That is, the
#'     variable receiving the flow.
#'     \item `from`: The variable from which the arrow originate. That is, the
#'     variable donating the flow.
#'     \item `label`: The label of the flow. Typically a mathematical expression.
#'     \item `xmin`: The starting horizontal position of the arrow.
#'     \item `xmax`: The ending horizontal position of the arrow.
#'     \item `ymin`: The starting vertical position of the arrow.
#'     \item `ymax`: The ending vertical position of the arrow.
#'     \item `xlabel`: Horizontal position (midpoint) of label.
#'     \item `ylabel`: Vertical position (midpoint) of label.
#'     \item `curvature`: The amount of curvature applied to arrow.
#'     Higher numbers indicate more curvature; 0 = straight line.
#'     \item `type`: Type of flow. One of main, interaction, or external.
#'     \item `color`: Default color of the lines/arrows.
#'     \item `linetype`: Default linetype.
#'     \item `size`: Default size of the lines.
#'     \item `label_color`: Default label color.
#'     \item `label_size`: Default text size for label.
#'     \item `arrowsize`: Default arrow size.
#'     \item `math`: The math from the flows specified by the user. Is a
#'     duplicate of `label` so that user can update `label` as desired but
#'     retain the original math for reference.
#'   }
#' }
#' @details `varlabels` needs to be specified as a vector of model variables,
#' e.g., varlabels <- c("Pred","Prey").
#' `flows` need to be specified as a list, with each list entry containing the
#' flows/processes for each variable in the order in which the variables appear.
#' Flows need to be named according to VARLABEL_flows.
#' Example: flows <- list(Pred_flows = c(`r*Pred`, `-k1*Pred*Prey`),
#'                        Prey_flows = c(`g*Prey`, `-k2*Pred*Prey`) )
#' Each flow, i.e. each entry in the flow vector, needs to be a valid
#' mathematical expression made up of varlabels and parameters.
#' The rules are as described above.
#' As an example, the following includes a parameter *b* and two variables, *S*
#' and *I*: `b*S*I`. The following includes a parameter *s* and two
#' variables, *Bg* and *I2*: `Bg*s*I2`.
#' See more examples below and in the vignettes.
#'
#' @examples
#' #basic model specification
#' varlabels <- c("S","I","R")
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' mymodel <- list(varlabels = varlabels, flows = flows)
#' diag_list <- prepare_diagram(model_list = mymodel)
#' mydiag <- make_diagram(diag_list)
#'
#' #adding optional specifications
#' varnames <- c("Susceptible","Infected","Recovered")
#' varlocations <-  matrix(data = c("S", "", "R",
#'                                  "", "I", "" ),
#'                         nrow = 2, ncol = 3, byrow = TRUE)
#' mysettings <- list(varnames = varnames, use_varnames = TRUE,
#'                    var_label_size = 4, varlocations = varlocations)
#' diag_list <- prepare_diagram(model_list = mymodel, model_settings = mysettings)
#' mydiag <- make_diagram(diag_list)
#'
#' #another simple model
#' varlabels = c("Pat","Imm")
#' flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
#'                  Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
#' mymodel = list(varlabels = varlabels, flows = flows)
#' diag_list <- prepare_diagram(mymodel)
#' mydiag <- make_diagram(diag_list)
#'
#' #options to switch to vertical layout and adding names
#' varnames <- c("Pathogen","Immune Response")
#' varlocations <-  matrix(data = c("Pat", "Imm"),
#'                         nrow = 2, byrow = TRUE)
#' mysettings <- list(varnames = varnames, use_varnames = TRUE,
#'                    var_label_size = 4, varlocations = varlocations)
#' diag_list <- prepare_diagram(mymodel,mysettings)
#' mydiag <- make_diagram(diag_list)
#'
#' @export


prepare_diagram <- function(model_list,
                            model_settings = list(
                              varlocations = NULL,
                              varbox_x_scaling = 1,
                              varbox_y_scaling = 1,
                              varspace_x_scaling = 1,
                              varspace_y_scaling = 1)
                            ) {

  ######################################################################
  # check to make sure model_list is a properly specified model
  ######################################################################
  check <- check_model_list(model_list)
  if(check$bad == TRUE) {
    stop(check$msg)
  }

  # Extract model_settings to in scope objects
  for(i in 1:length(model_settings)) {
    assign(names(model_settings)[i], value = model_settings[[i]])
  }

  # check to make sure scaling parameters are of length 1 or length of the
  # the number of variables. hard error out if not
  if(length(varbox_x_scaling) != 1) {
    if(length(varbox_x_scaling) != length(model_list$varlabels)) {
      stop("varbox_x_scaling must be of length 1 or length of the number of variables")
    }
  }
  if(length(varbox_y_scaling) != 1) {
    if(length(varbox_y_scaling) != length(model_list$varlabels)) {
      stop("varbox_y_scaling must be of length 1 or length of the number of variables")
    }
  }
  if(length(varspace_x_scaling) != 1) {
    if(length(varspace_x_scaling) != length(model_list$varlabels)) {
      stop("varspace_x_scaling must be of length 1 or length of the number of variables")
    }
  }
  if(length(varspace_y_scaling) != 1) {
    if(length(varspace_y_scaling) != length(model_list$varlabels)) {
      stop("varspace_y_scaling must be of length 1 or length of the number of variables")
    }
  }

  # repeat the space scalings if needed
  # if(length(varbox_x_scaling) == 1) {
  #   varbox_x_scaling <- recycle_values(varbox_x_scaling, n = length(model_list$varlabels))
  # }
  # if(length(varbox_y_scaling) == 1) {
  #   varbox_y_scaling <- recycle_values(varbox_y_scaling, n = length(model_list$varlabels))
  # }
  # if(length(varspace_x_scaling) == 1) {
  #   varspace_x_scaling <- recycle_values(varspace_x_scaling, n = length(model_list$varlabels))
  # }
  # if(length(varbox_y_scaling) == 1) {
  #   varspace_y_scaling <- recycle_values(varspace_y_scaling, n = length(model_list$varlabels))
  # }

  # assign default settings to be updated by user
  defaults <- eval(formals(prepare_diagram)$model_settings)

  ######################################################################
  # check user inputs provided in model_settings, if user supplies a non-recognized argument, stop
  ######################################################################
  if (!is.null(model_settings))
  {
    nonrecognized_inputs <- setdiff(names(model_settings),  names(defaults))
    if (length(nonrecognized_inputs>0) )
    {
      stop('These elements of model_settings are not recognized: ', nonrecognized_inputs)
    }
    # update defaults with user settings
    defaults[names(model_settings)] <- model_settings
    model_settings <- defaults  # reassign
  }

  ######################################################################
  # Check if varlocation matrix is provided
  # Make sure the varlocations matrix entries match those in model_list
  ######################################################################
  varlocation_matrix <- model_settings$varlocations
  if(!is.null(model_settings$varlocations))
     {
       varlocnames = as.vector(model_settings$varlocations)
       varlocnames = varlocnames[varlocnames !=""] #remove empty entries
       if (!setequal(varlocnames, model_list$varlabels))
          {
            # returns fatal error if variables do not match
            stop("varlocation entries do not match varlabels in model_list.")
       }
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
  if(length(model_settings$varnames) > 1) {
    longvarnames <- model_settings$varnames

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
  #flows <- add_plus_signs(flows)
  flows <- add_plus_signs(flows)

  #turns flow list into matrix, adding NA, found it online,
  #not sure how exactly it works (from AH and modelbuilder code base)
  #variables are along rows and flows along columns.
  flowmat <- t(sapply(flows, `length<-`, max(lengths(flows))))

  # if there are just two variables and a single flow between them,
  # the flowmat is oriented incorrectly (nodes across columns). this
  # can be diagnosed by checking to see if flowmat has rownames. if not,
  # the then matrix needs to be transposed.
  if(is.null(rownames(flowmat))) {
    flowmat <- t(flowmat)
  }

  #strip leading +/- from flows and replace with no space
  flowmatred <- sub("\\+|-","",flowmat)

  #extract only the + or - signs from flows so we know the direction
  signmat <- gsub("(\\+|-).*","\\1",flowmat)

  #define nodes data frame structure if not provided by user
  # Create a node data frame
  variables <- data.frame(
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
  variables$row <- as.numeric(strats)


  # Create the edge data frame by looping through the variables
  # and associated flows.
  flows <- list()  #an empty list to be coerced to a data frame via rbind

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
      #varspars <- unique(get_vars_pars(currentflowfull))
      varspars <- unique(get_vars_pars(currentflowfull))
      varfirsts <- substr(varspars, start = 1, stop = 1)  #get first letters

      #vars is now a vector of the variables that are in the flow math
      vars <- varspars[which(varfirsts %in% LETTERS)]  #variables are UPPERCASE

      #extract the numeric ids for the variables in this flow
      varsids <- variables[which(variables$label %in% vars), "id"]

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
        flows <- rbind(flows, tmp)
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
          flows <- rbind(flows, tmp)
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
        flows <- rbind(flows, tmp)
      }

      # interaction flag if two variables are in the flow
      if(length(vars) > 1) {
        if(length(unique(connectvars)) > 1) {
          # this means that the flow connects two variables and both
          # are present in the flow math
          flows[nrow(flows), "interaction"] <- TRUE
        } else {
          # this means that the flow comes from or goes to somewhere out
          # of the system, and only 1 variable is included in the
          # flow math. this is designated as an "out_interaction"
          flows[nrow(flows), "out_interaction"] <- TRUE
        }
      }

    }  #end flow loop
  }  #end variable loop

  # Keep only distinct rows; duplication occurs because one variable's
  # inflow can be another variable's outflow, but we only want these once
  # in the data frame for edges (segments/arrows/flows).
  flows <- unique(flows)

  # Parse the meaning of duplicate labels. Usually this is a complex mix
  # of a direct, physical flows and interactions from several other
  # state variables. We assume that the "main" flow among the "auxilliary"
  # duplicate flows is the one that traverses left-to-right (e.g., 1 to 2)
  # with the smallest gap and has no interaction flags.
  dups <- as.matrix(table(flows$label))  # tally the occurences of each flow
  dupids <- rownames(dups)[which(dups[,1] > 1)]  # grab the one with >1 occurence
  if(length(dupids) > 0) {
    flowdups <- subset(flows, label %in% dupids)  # take a subset of the edge data frame
    flows <- subset(flows, !(label %in% dupids))  # restrict flows to non-duplicate flows
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
    flows <- rbind(flows, maindup, intdup)
  }

  # Duplicate rows with out_interaction == TRUE to assign the interaction
  # flag and then remove the out_interaction flag. This is done to
  # achieve appropriate labeling. We want the physical flow to have no label
  # and for the interaction arrow to carry to the label.
  repdf <- subset(flows, out_interaction == TRUE)
  if(nrow(repdf) != 0) {  # avoids errors if no rows
    repdf$interaction <- TRUE  # set this to TRUE for linetypes
    repdf$out_interaction <- NULL  # remove this now
    flows[which(flows$out_interaction == TRUE), "label"] <- ""  # take away the label for the physical flow
    flows$out_interaction <- NULL  # remove this now
    flows <- rbind(flows, repdf)  # tack them together
  }

  # remove out_interaction completely now that interaction is
  # appropriately flagged with correct labeling
  flows$out_interaction <- NULL


  # Break edges apart into:
  #   direct flows
  #   interactions to meet at edges
  #   the flows resulting from interactions
  # All flows are treated seperately because their start and end positions
  # depend on state variables in different ways.

  flows$linkto <- NA  #empty column for interaction flows, but needed for binding
  flows$linkfrom <- NA  #empty column for interaction flows, but needed for binding
  ints <- subset(flows, interaction == TRUE)
  flows <- subset(flows, interaction == FALSE)

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
      #v <- get_vars_pars(tmp$label)  #strips away math, leaving just letters
      v <- get_vars_pars(tmp$label)  #strips away math, leaving just letters
      vf <- substr(v, start = 1, stop = 1)  #get first letters
      v <- v[which(vf %in% LETTERS)]  #subset to upper case VARIABLES
      ids <- subset(variables, label %in% v)[ , "id"]  #extract the relevant numeric ids

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
    flows <- rbind(flows, ints, intflows)
  }

  # Keep only distinct rows
  flows <- unique(flows)


  # Make dummy compartment for all flows in and out of the system.
  # Dummy compartments are given ids that start with three numbers
  # that identify the type of dummy:
  #   999* = dummy compartments for flows out of the system (e.g., death pool)
  #   -999* = dummy comparments for flows into the system (e.g., birth pool)
  #   555* = dummy compartments for interaction links
  # These are just used to create empty nodes for arrows to originate from
  # or go to.

  # Out of the system
  outdummies <- NULL
  numnas <- length(flows[is.na(flows$to) & flows$interaction == FALSE, "to"])
  if(numnas > 0) {
    outdummies <- as.numeric(paste0("999", c(1:numnas)))
    flows[is.na(flows$to) & flows$interaction == FALSE, "to"] <- outdummies
  }

  # In to the system
  indummies <- NULL
  numnas <- length(flows[is.na(flows$from) & flows$interaction == FALSE, "from"])
  if(numnas > 0) {
    indummies <- as.numeric(paste0("-999", c(1:numnas)))
    flows[is.na(flows$from) & flows$interaction == FALSE, "from"] <- indummies
  }

  # Make dummy compartment for "links" in interactions
  linkdummies <- NULL
  numlinks <- length(flows[is.na(flows$to) &
                           flows$interaction == TRUE &
                           !is.na(flows$linkto), "to"])
  if(numlinks > 0) {
    linkdummies <- as.numeric(paste0("555", c(1:numlinks)))
    flows[is.na(flows$to) & flows$interaction == TRUE, "to"] <- linkdummies
  }


  # Add dummy compartments to nodes dataframe
  if(is.numeric(outdummies) | is.numeric(indummies) | is.numeric(linkdummies)) {
    exnodes <- data.frame(id = c(outdummies, indummies, linkdummies),
                          label = "",
                          name = NA,
                          row = 1)
    exnodes[setdiff(names(variables), names(exnodes))] <- NA
    variables <- rbind(variables, exnodes)
  }

  # Add location information
  variables <- add_locations(variables, varlocations, varbox_x_scaling,
                             varbox_y_scaling, varspace_x_scaling,
                             varspace_y_scaling)


  # Add midpoint locations for nodes
  # Here we just iterate over the nodes and take their position in the
  # data frame rows and multiply by 3 (e.g., 1*3, 2*3, 3*3) to get
  # arbitrary x positions. y positions take the row id and multiply by
  # negative 2, meaning that additional rows always go below the row that
  # was previously defined.
  # If the varlocation_matrix is provided, then the same procedure is applied, but
  # based on the row and column positions provided by the user.
  # if(is.null(varlocation_matrix)) {
  #   variables <- variables[order(variables$id), ]
  #   variables$x <- NA
  #   variables$y <- NA
  #   for(rid in unique(variables$row)) {
  #     variables[which(variables$row == rid), "x"] <- (1:nrow(variables[which(variables$row == rid), ])*3)-2.5
  #     variables[which(variables$row == rid), "y"] <- (as.numeric(rid) * -2)+2.5
  #   }
  # } else {
  #   ny <- (1:nrow(varlocation_matrix) * -2)+2
  #   nx <- (1:ncol(varlocation_matrix) * 3)-3
  #   for(nid in varnames) {
  #     pos <- which(varlocation_matrix == nid, arr.ind = TRUE)
  #     variables[which(variables$label == nid), "x"] <- nx[pos[1, 2]]
  #     variables[which(variables$label == nid), "y"] <- ny[pos[1, 1]]
  #   }
  # }
  #
  # # Add xmin/max and ymin/max columns for node rectangles
  # # I use a 0.5 offset in both directions, creating a 1x1 sized square.
  # xoff <- 0.5  # default
  # yoff <- 0.5  # default
  # variables$xmin <- with(variables, x - xoff)
  # variables$xmax <- with(variables, x + xoff)
  # variables$ymin <- with(variables, y - yoff)
  # variables$ymax <- with(variables, y + yoff)


  # update inflow node positions from nowhere (e.g. births)
  inflownodes <- subset(variables, id < -9990)$id
  for(id in inflownodes) {
    newxyid <- flows[which(flows$from == id), "to"]
    newxy <- variables[which(variables$id == newxyid), c("xmin", "xmax", "ymin", "ymax")]
    newxy$ymax <- newxy$ymax + (varspace_y_scaling * 2)  # above the variable
    newxy$ymin <- newxy$ymin + (varspace_y_scaling * 2)  # above the variable
    variables[which(variables$id == id), c("xmin", "xmax", "ymin", "ymax")] <- newxy

    newmids <- c((newxy$xmin+newxy$xmax)/2, (newxy$ymin+newxy$ymax)/2)
    variables[which(variables$id == id), c("xlabel", "ylabel")] <- newmids
  }

  # update outflow node positions to nowhere
  outflownodes <- subset(variables, id > 9990)$id
  for(id in outflownodes) {
    newxyid <- flows[which(flows$to == id), "from"]
    newxy <- variables[which(variables$id == newxyid), c("xmin", "xmax", "ymin", "ymax")]
    newxy$ymax <- newxy$ymax - (varspace_y_scaling * 2)  # below the variable
    newxy$ymin <- newxy$ymin - (varspace_y_scaling * 2)  # below the variable
    variables[which(variables$id == id), c("xmin", "xmax", "ymin", "ymax")] <- newxy

    newmids <- c((newxy$xmin+newxy$xmax)/2, (newxy$ymin+newxy$ymax)/2)
    variables[which(variables$id == id), c("xlabel", "ylabel")] <- newmids
  }

  # update invisible interaction link nodes, i.e., nodes that need to sit
  # at the midpoint of some other arrow, but not be drawn
  linknodes <- subset(variables, id > 5550 & id < 9990)$id
  for(id in linknodes) {
    start <- flows[which(flows$to == id), "linkfrom"]
    end <- flows[which(flows$to == id), "linkto"]
    newx1 <- variables[which(variables$id == start), "xlabel"]
    newx2 <- variables[which(variables$id == end), "xlabel"]
    newx <- (newx1+newx2)/2  # midpoint of the physical arrow
    newy1 <- variables[which(variables$id == start), "ylabel"]
    newy2 <- variables[which(variables$id == end), "ylabel"]
    newy <- (newy1+newy2)/2  # midpoint of the physical arrow
    variables[which(variables$id == id), c("xlabel", "ylabel")] <- c(newx, newy)

    # set min/max to midpoint for ease because these are not actually
    # drawn, therefore rectangle boundaries do not need to be accurate
    variables[which(variables$id == id), c("xmin", "ymin")] <- c(newx, newy)
    variables[which(variables$id == id), c("xmax", "ymax")] <- c(newx, newy)
  }

  # Subset out interactions to in/out flows
  extints <- subset(flows, interaction == TRUE & is.na(linkto))
  if(nrow(extints) > 0) {
    for(i in 1:nrow(extints)) {
      tmp <- extints[i, ]
      v <- get_vars_pars(tmp$label)
      vf <- substr(v, start = 1, stop = 1)  #get first letters
      v <- v[which(vf %in% LETTERS)]
      ids <- subset(variables, label %in% v)[ , "id"]
      id <- ids[which(ids != tmp$from)]
      extints[i, "to"] <- id
    }
  }


  # Create segment coordinates by merging with node locations
  flows <- merge(flows, variables[,c("xmin", "xmax", "ymin", "ymax", "xlabel", "ylabel", "id")],
               by.x = "from", by.y = "id")
  flows <- merge(flows, variables[,c("xmin", "xmax", "ymin", "ymax", "xlabel", "ylabel", "id")],
               by.x = "to", by.y = "id", suffixes = c("start", "end"))

  # add columns to be populated
  flows$xmin <- NA_real_
  flows$xmax <- NA_real_
  flows$ymin <- NA_real_
  flows$ymax <- NA_real_

  # update arrow start and end points based on relationship between to
  # and from positions
  for(i in 1:nrow(flows)) {
    tmp <- flows[i, ]
    if(tmp$yminstart == tmp$yminend & tmp$xminstart != tmp$xminend) {
      flows[i, "xmin"] <- tmp$xmaxstart
      flows[i, "xmax"] <- tmp$xminend
      flows[i, "ymin"] <- mean(c(tmp$yminstart, tmp$ymaxstart))
      flows[i, "ymax"] <- mean(c(tmp$yminstart, tmp$ymaxstart))
    }

    if(tmp$yminstart > tmp$yminend & tmp$xminstart == tmp$xminend) {
      flows[i, "xmin"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "xmax"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "ymin"] <- tmp$yminstart
      flows[i, "ymax"] <- tmp$ymaxend
    }

    if(tmp$yminstart < tmp$yminend & tmp$xminstart == tmp$xminend) {
      flows[i, "xmin"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "xmax"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "ymin"] <- tmp$ymaxstart
      flows[i, "ymax"] <- tmp$yminend
    }

    if(tmp$interaction == TRUE & tmp$direct_interaction == FALSE) {
      flows[i, "xmin"] <- tmp$xlabelstart
      flows[i, "xmax"] <- tmp$xminend
      flows[i, "ymin"] <- tmp$ymaxstart
      flows[i, "ymax"] <- tmp$ylabelend
    }

    if(tmp$yminstart > tmp$ymaxend & tmp$xmaxstart < tmp$xminend) {
      flows[i, "xmin"] <- tmp$xmaxstart
      flows[i, "xmax"] <- tmp$xminend
      flows[i, "ymin"] <- tmp$ylabelstart
      flows[i, "ymax"] <- tmp$ylabelend
    }

    if(tmp$yminstart < tmp$ymaxend & tmp$xmaxstart < tmp$xminend) {
      flows[i, "xmin"] <- tmp$xmaxstart
      flows[i, "xmax"] <- tmp$xminend
      flows[i, "ymin"] <- tmp$ylabelstart
      flows[i, "ymax"] <- tmp$ylabelend
    }

    if(tmp$xmaxstart == tmp$xmaxend & tmp$ymaxstart == tmp$ymaxend) {
      flows[i, "xmin"] <- tmp$xlabelstart - 0.25
      flows[i, "xmax"] <- tmp$xlabelend + 0.25
      flows[i, "ymin"] <- tmp$ymaxend
      flows[i, "ymax"] <- tmp$ymaxend
    }
  }

  # remove unneeded columns
  flows[ , c("xminstart", "xmaxstart", "yminstart", "ymaxstart",
           "xlabelstart", "ylabelstart", "xminend", "xmaxend",
           "yminend", "ymaxend", "xlabelend", "ylabelend")] <- NULL


  # label locations are mid points
  flows$xlabel <- with(flows, (xmax + xmin) / 2)
  flows$ylabel <- with(flows, (ymax + ymin) / 2) + 0.25  # label slightly above the arrrow
  flows$diff <- with(flows, abs(to-from))

  if(!is.null(varlocation_matrix)) {
    xdiffs <- with(flows, abs(xmin - xmax))
    xdiffs <- ifelse(xdiffs %in% c(0, 3), 0.5, 1)
    ydiffs <- with(flows, abs(ymin - ymax))
    ydiffs <- ifelse(ydiffs %in% c(0, 2), 0.5, 1)
    for(i in 1:nrow(flows)) {
      if(flows[i, "interaction"] == FALSE &
         flows[i, "direct_interaction"] == FALSE &
         flows[i, "to"] < 9900 &
         flows[i, "from"] > -9900) {
        flows[i, "diff"] <- xdiffs[i] + ydiffs[i]
      }
    }
  }

  # update vertical edges to go in and out at angles
  flows <- make_vdf_angled(flows, variables, model_settings)

  # update vertical edges to avoid overlaps
  flows <- fix_arrow_pos(flows)


  # # Get midpoints of in/out segments for external interactions "to" locations
  # if(nrow(extints) > 0) {
  #   extlinks <- subset(flows, label == "")
  #   extints <- merge(extints, variables[,c("xlabel", "ylabel", "id")], by.x = "from", by.y = "id")
  #   colnames(extints)[which(colnames(extints) == "xlabel")] <- "xmin"
  #   colnames(extints)[which(colnames(extints) == "ylabel")] <- "ymin"
  #   extints$xmax <- NA
  #   extints$ymax <- NA
  #   for(i in 1:nrow(extints)) {
  #     tmp1 <- extints[i, ]
  #     tmp1[ , c("xmax", "ymax")] <- NULL
  #     tmp2 <- extlinks[which(tmp1$to == extlinks$from), ]
  #     if(tmp2$to == tmp2$from) {
  #       tmp3 <- merge(tmp1, tmp2[, c("xmax", "ymax", "from")],
  #                        by.x = "to", by.y = "from")
  #       tmp3$ymax <- tmp3$ymax + 0.75
  #       tmp3$xmax <- tmp3$xmax + 0.17
  #     } else {
  #       tmp3 <- merge(tmp1, tmp2[, c("xlabel", "ylabel", "from")],
  #                     by.x = "to", by.y = "from")
  #     }
  #     # colnames(tmp3) <- c("to", "from", "label", "interaction", "link",
  #     #                        "xmin", "ymin", "xmax", "ymax")
  #     extints[i, ] <- tmp3
  #   }
  #   extints$xlabel <- with(extints, (xmax + xmin) / 2)
  #   extints$ylabel <- with(extints, (ymax + ymin) / 2) + 0.25
  #   extints$diff <- with(extints, abs(to-from))
  #
  #   flows <- rbind(flows, extints)
  # }





  # split up the edges into constituent parts:
  # - curved segments
  # - straight (horizontal) segments
  # - vertical segments
  # - feedback segments (curved back onto same node)
  # cdf <- subset(flows, (diff > 1 & diff < 9000) & (to != from) | interaction == TRUE)
  # sdf <- subset(flows, (diff <= 1 | diff >= 9000) & interaction == FALSE)
  # vdf <- subset(sdf, abs(diff) >= 9900)
  # sdf <- subset(sdf, abs(diff) < 9900)
  # fdf <- subset(sdf, to == from)
  # sdf <- subset(sdf, to != from)

  # Add offsets to straight edges. The offset depends on the variation
  # in x and y. If xstart == xend, then this is a vertical alignment and
  # y offsets are applied. If ystart == yend, then this is a
  # horizontal alignment and x offsets are applied. If vertical, the midpoints
  # are also updated to move the label to the right of the arrow.
  # if(nrow(sdf) != 0) {
  #   for(i in 1:nrow(sdf)) {
  #     if(sdf[i, "xstart"] == sdf[i, "xend"]) {
  #       sdf[i, "ystart"] <- sdf[i, "ystart"] - yoff
  #       sdf[i, "yend"] <- sdf[i, "yend"] + yoff
  #       sdf[i, "ymid"] <- (sdf[i, "yend"] + sdf[i, "ystart"]) / 2
  #       sdf[i, "xmid"] <- ((sdf[i, "xend"] + sdf[i, "xstart"]) / 2) + 0.25  # label to right
  #     } else {
  #       sdf[i, "xstart"] <- sdf[i, "xstart"] + xoff
  #       sdf[i, "xend"] <- sdf[i, "xend"] - xoff
  #     }
  #   }
  # }



  flows <- set_curvature(flows, variables)


  ### TODO Delete after testing
  # Update start and end points for curved arrows that bypass nodes,
  # these need to start/end from the top/bottom of the nodes. If the
  # arrow goes right to left, it will start and end on top of nodes. If
  # the arrow goes left to right, it will start and end on the bottom of
  # nodes. Similar logic applies if the diagram is positioned vertically
  # rather than horizontally.
  # if(nrow(cdf) > 0) {
  #   for(i in 1:nrow(cdf)) {
  #     if(cdf[i, "interaction"] == FALSE &
  #        cdf[i, "direct_interaction"] == FALSE) {
  #       if(sign(cdf[i, "diff"]) == 1) {
  #         cdf[i, "xstart"] <- cdf[i, "xstart"] + xoff
  #         cdf[i, "xend"] <- cdf[i, "xend"] - xoff
  #         cdf[i, "ystart"] <- cdf[i, "ystart"] + yoff
  #         cdf[i, "yend"] <- cdf[i, "yend"] + yoff
  #         cdf[i, "labely"] <- cdf[i, "labely"] + yoff
  #       }
  #       if(sign(cdf[i, "diff"]) == -1) {
  #         cdf[i, "xstart"] <- cdf[i, "xstart"] - xoff
  #         cdf[i, "xend"] <- cdf[i, "xend"] + xoff
  #         cdf[i, "ystart"] <- cdf[i, "ystart"] - yoff
  #         cdf[i, "yend"] <- cdf[i, "yend"] - yoff
  #         cdf[i, "labely"] <- cdf[i, "labely"] - yoff
  #       }
  #     }
  #   }
  # }

  # if(nrow(cdf) != 0) {
  #   for(i in 1:nrow(cdf)) {
  #     if(cdf[i, "interaction"] == TRUE &
  #        cdf[i, "direct_interaction"] == FALSE) {
  #       if(cdf[i, "xmin"] == cdf[i, "xend"]) {
  #         cdf[i, "xmin"] <- cdf[i, "xstart"] + xoff
  #         cdf[i, "ymin"] <- cdf[i, "ystart"] - yoff
  #       }
  #     }
  #   }
  # }
  #

  ### TODO Delete after testing
  # # test to make sure splits are unique and sum up to original data frame
  # test <- nrow(vdf) + nrow(sdf) + nrow(cdf) + nrow(fdf) == nrow(flows)
  # if(!test) {
  #   stop(paste0("Edges data frame is not splitting appropriately.\n",
  #               "       Contact package maintainer."))
  # }

  # set curvature of feedback loops. this is pretty different from the
  # "regular" curvature settings, so we made a separat function for this
  # operation.
  flows <- set_feedback_curvature(flows)

  # update external interaction arrows now that all other positioning
  # is final
  # ext <- extints
  # edf <- flows
  # ndf <- variables
  extints <- update_external_interaction_positions(extints, variables)

  # combine all flows
  flows <- rbind(flows, extints)

  # now drop "hidden" nodes without labels
  variables <- subset(variables, label != "")

  # set to/from columns to NA if value is not in node dataframe
  flows <- set_node_to_na(flows, variables)

  # remove rows with no location information
  flows <- remove_na_rows(flows)

  # convert direct interaction to flag to regular interaction flag,
  # now only relevant for plotting
  flows <- update_interactions(flows)

  # update all to and froms such that each is the variable label
  flows <- update_tofroms(flows, variables)

  # remove the row column
  variables$row <- NULL
  flows$row <- NULL


  ### TODO Delete after testing
  # change the label to full name, if requested
  # this will be move farther up once code to adjust box size to text is
  # implemented

  # # first check that varnames are provided, if not cause error
  # if(model_settings$use_varnames & is.null(model_settings$varnames)) {
  #   stop("If you want to use `varnames` you need to specify them.")
  # }
  #
  # # SHOULD CHECK HERE TO MAKE SURE VARNAMES HAS THE RIGHT LENGTH - MAYBE ALREADY DONE ABOVE?
  # if(model_settings$use_varnames) {
  #   nodes$plot_label <- nodes$name
  # } else {
  #   nodes$plot_label <- nodes$label
  # }

  # sdf$labelx <- sdf$xmid
  # sdf$labely <- sdf$ymid
  # horizontal_edges <- subset(sdf, select = -c(diff, linkto, linkfrom, xmid, ymid))
  # if(nrow(horizontal_edges) > 0) {
  #   horizontal_edges$curvature <- 0
  # } else {
  #   horizontal_edges$curvature <- numeric()
  # }

  # vdf$labelx <- vdf$xmid
  # vdf$labely <- vdf$ymid
  # vertical_edges <- subset(vdf, select = -c(diff, interaction, linkto,
  #                                           linkfrom, xmid, ymid))
  # if(nrow(vertical_edges) > 0) {
  #   vertical_edges$curvature <- 0
  #   vertical_edges$interaction <- FALSE
  # } else {
  #   vertical_edges$curvature <- numeric()
  #   vertical_edges$interaction <- logical()
  # }

  # cdf$row <- NULL
  # curved_edges <- subset(cdf, select = -c(diff, linkto, linkfrom, ymid, xmid))

  # fdf$labelx <- fdf$xmid
  # fdf$labely <- fdf$ymid
  # fdf$labely <- fdf$labely + 0.85  # this offset makes the label a little above the big curved arrow
  # # last, adjust the xs and ys to get arrow above and feeding back into the node
  # fdf$xstart <- fdf$xstart-0.25
  # fdf$ystart <- fdf$ystart+0.5
  # fdf$xend <- fdf$xend+0.25
  # fdf$yend <- fdf$yend+0.5
  # feedback_edges <-  subset(fdf, select = -c(diff, linkto, linkfrom, interaction,
  #                                            xmid, ymid))
  # if(nrow(feedback_edges) > 0) {
  #   feedback_edges$curvature <- -2
  #   feedback_edges$interaction <- FALSE
  # } else {
  #   feedback_edges$curvature <- numeric()
  #   feedback_edges$interaction <- logical()
  # }
  #
  #
  # # combine all the edge data frames
  # flows <- rbind(horizontal_edges,
  #                vertical_edges,
  #                curved_edges,
  #                feedback_edges)

  # update interaction column to be type column, one of
  # main, interaction, or external.
  flows$type <- "main"
  flows$type <- ifelse(flows$interaction == TRUE, "interaction", flows$type)
  flows$type <- ifelse(flows$interaction == FALSE & (is.na(flows$to) | is.na(flows$from)),
                       "external", flows$type)
  flows$interaction <- NULL


  # add text size arguments
  # nodes$plot_label_size <- model_settings$var_label_size
  # variables <- nodes  # rename for user facing data frame

  #sort flows by type, main/external/interaction
  flows = rbind(flows[flows$type=="main",],flows[flows$type=="external",],flows[flows$type=="interaction",])


  #add a row id so it's easier for users to know which row to alter
  flows$id = 1:nrow(flows)

  # add a math column to differentiate from label if needed
  flows$math <- flows$label

  # update flows column ordering
  flows <- flows[, c("id", "to", "from", "label", "xmin", "xmax", "ymin", "ymax",
                     "xlabel", "ylabel", "curvature", "type", "math")]


  #remove row names, those are confusing
  rownames(flows) <- NULL
  rownames(variables) <- NULL

  # apply default aesthetics
  dflist <- apply_default_aesthetics(list(variables = variables,
                                          flows = flows))

  # Add inputs to return list.
  dflist$inputs <- list(model_list = model_list,
                        model_settings = model_settings)

  return(dflist)
}
