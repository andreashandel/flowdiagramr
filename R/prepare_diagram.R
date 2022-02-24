#' Create data frames for plotting from model elements.
#'
#' @description
#' This function takes as input a model
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
#' \item `variables`: A character vector specifying the names of all variables.
#' \item `flows`: A list that is the same length as `variables`. Each sub-list
#'     element is a character vector of any length specifying the flows into
#'     and out of the variable. Note that **flowdiagramr** assumes that the
#'     order of `flows` and `variables` match.
#' \item IMPORTANT: All `variables` entries must start with an upper case letter,
#' followed by any combination of letters and numbers (e.g.,
#' S, Si, or Aml2). All parameters contained in `flows`
#' must start with a lower case letter
#' followed by any combination of letters and numbers (e.g.,
#' b, bBmax, kS, p21S). All variables and parameters MUST be separated by
#' math notation (e.g., +, -, *, /).
#' Most math functions (e.g., `sin`, `cos`) are currently not supported.
#' \item See examples and details below and vignettes.
#' }
#'
#' @param model_settings A list of optional model settings. The following
#'     elements are supported. If not provided, all default to a value of 1.
#' \itemize{
#' \item `varlocations`: A matrix containing all `model_list$variables` entries in specific locations on a grid. See examples.
#' \item `varbox_x_size`: Either a scalar or a vector that changes the default
#'     width of variable boxes. For example, `varbox_x_size = 1.5` makes each box
#'     1.5 units in width. If a scalar, the value is used for all variables.
#'     If a vector, the values are applied to the variables in the order
#'     provided in `model_list$vars`.
#' \item `varbox_y_size`: Same as `varbox_x_size` but for the height of the boxes.
#' \item `varspace_x_size`:  Either a scalar or a vector that changes the spacing between
#'     variable boxes in the x/horizontal dimension.
#'     To use this, you need to provide a `varlocations` matrix.
#'     If `varspace_x_size` is a scalar, all spaces between boxes in the x direction will be the same.
#'     For example, `varspace_x_size = 1.5` puts 1.5 units of space in the x direction between boxes.
#'     If you provide a vector, it needs to be of dimension one less than the number of columns in `varlocations`.
#'     Spacing starts at the left, thus the first number is the spacing between the first column and second column, etc.
#' \item `varspace_y_size`:  Same as `varspace_y_size` but for the vertical dimension.
#'     If you provide a vector, it needs to be of dimension one less than the number of rows in `varlocations`.
#'     Spacing starts at the bottom, thus the first number is the spacing between the lowest and second lowest row, etc.
#' \item See examples and details below and vignettes.
#' }
#'
#' @return A list of two data frames containing all necessary information
#'         for the model variables/boxes and flows/arrows to be plotted
#'         by the \code{\link{make_diagram}} function.
#'         The data frames are:
#' \itemize{
#'   \item `variables`: A data frame containing information for all variables.
#'   The data frame contains these columns:
#'
#'   \itemize{
#'     \item `name`: The name of the variable as provided in the model specification.
#'     \item `label`: A potential alternative label for the box. By default same as `name`.
#'     \item `id`: A numeric id for each variable.
#'     \item `xmin`: Left edge location of variable box.
#'     \item `xmax`: Right edge location of variable  box.
#'     \item `ymin`: Lower edge of location variable box.
#'     \item `ymax`: Upper edge of location variable  box.
#'     \item `xlabel`: Horizontal position (midpoint) of label.
#'     \item `ylabel`: Vertical position (midpoint) of label.
#'   }
#'
#'   \item `flows`: A data frame containing information for all flows.
#'   The data frame contains these columns:
#'   \itemize{
#'     \item `type`: Type of flow. One of main, interaction, or external.
#'     \item `id`: A numeric id for each flow.
#'     \item `from`: The variable from which the arrow originate. That is, the
#'     variable donating the flow.
#'     \item `to`: The variable to which the arrow will point. That is, the
#'     variable receiving the flow.
#'     \item `label`: The label of the flow. Typically a mathematical expression.
#'     \item `xmin`: The starting horizontal position of the arrow.
#'     \item `xmax`: The ending horizontal position of the arrow.
#'     \item `ymin`: The starting vertical position of the arrow.
#'     \item `ymax`: The ending vertical position of the arrow.
#'     \item `xlabel`: Horizontal position (midpoint) of label.
#'     \item `ylabel`: Vertical position (midpoint) of label.
#'     \item `curvature`: The amount of curvature applied to arrow.
#'     Higher numbers indicate more curvature; 0 = straight line.
#'     \item `math`: The math from the flows specified by the user. This is a
#'     duplicate of `label` so that user can update `label` as desired but
#'     retain the original math for reference.
#'   }
#' }
#' @details `variables` needs to be specified as a vector of model variables,
#' e.g., variables <- c("Pred","Prey").
#' `flows` need to be specified as a list, with each list entry containing the
#' flows/processes for each variable in the order in which the variables appear.
#' Flows need to be named according to VARIABLENAME_flows.
#' Example: flows <- list(Pred_flows = c(`r*Pred`, `-k1*Pred*Prey`),
#'                        Prey_flows = c(`g*Prey`, `-k2*Pred*Prey`) )
#' Each flow, i.e. each entry in the flow vector, needs to be a valid
#' mathematical expression made up of vars and parameters.
#' The rules are as described above.
#' As an example, the following includes a parameter *b* and two variables, *S*
#' and *I*: `b*S*I`. The following includes a parameter *s* and two
#' variables, *Bg* and *I2*: `Bg*s*I2`.
#' See more examples below and in the vignettes.
#'
#' @examples
#' #basic model specification
#' variables <- c("S","I","R")
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' mymodel <- list(variables = variables, flows = flows)
#' diag_list <- prepare_diagram(model_list = mymodel)
#' mydiag <- make_diagram(diag_list)
#'
#' #adding optional specifications
#' varlocations <-  matrix(data = c("S", "", "R",
#'                                  "", "I", "" ),
#'                         nrow = 2, ncol = 3, byrow = TRUE)
#' mysettings <- list(varlocations = varlocations)
#' diag_list <- prepare_diagram(model_list = mymodel, model_settings = mysettings)
#' mydiag <- make_diagram(diag_list)
#'
#' #another simple model for pathogen (prey) and immune response (predator)
#' variables = c("Pat","Imm")
#' flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
#'                  Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
#' mymodel = list(variables = variables, flows = flows)
#' diag_list <- prepare_diagram(mymodel)
#' mydiag <- make_diagram(diag_list)
#'
#' #manually switch to vertical layout
#' varlocations <-  matrix(data = c("Pat", "Imm"),
#'                         nrow = 2, byrow = TRUE)
#' mysettings <- list(varlocations = varlocations)
#' diag_list <- prepare_diagram(mymodel,mysettings)
#' mydiag <- make_diagram(diag_list)
#'
#' @export


# this function calls the following helper functions
# add_default_aes()
# add_locations()
# add_plus_signs()
# check_model_list()
# check_model_settings()
# fix_arrow_pos()
# make_vdf_angled()
# remove_na_rows()
# set_curvature()
# set_feedback_curvature()
# set_node_to_na()
# update_external_interaction_positions()
# update_interactions()
# update_straight_labels()
# update_tofroms()



prepare_diagram <- function(model_list,
                            model_settings = list(
                              varlocations = NULL,
                              varbox_x_size = NULL,
                              varbox_y_size = NULL,
                              varspace_x_size = NULL,
                              varspace_y_size = NULL)
                            )
{

  #############################################
  #############################################
  # Code block that does various checks and processing of input
  # This code block uses these helper functions:
  # check_model_list()
  # check_model_settings()
  #############################################
  #############################################



  ######################################################################
  #check to make sure model_list is provided
  #and is a properly specified model
  ######################################################################
  if (is.null(model_list))
  {
    stop('Argument model_list is required.')
  }
  checkmsg <- check_model_list(model_list)
  if(!is.null(checkmsg))
  {
    stop(checkmsg)
  }

  ######################################################################
  # check all user-provided model_settings to make sure entries are what they should be
  ######################################################################
  if (!is.null(model_settings))
  {
    checkmsg <- check_model_settings(model_list, model_settings)
    if(!is.null(checkmsg))
    {
      stop(checkmsg)
    }
  }

  ######################################################################
  # Set model_settings components that are not user-provided
  ######################################################################
  # For each model_settings component, if user didn't set it,
  # we set a default here
  # default for varlocations is a matrix with a single row
  # If user did not provide values for sizing/spacing,
  # we set vectors of length nvars and nvars-1 for box and space sizing
  # each with the default value of 1
  # note that we assign it to model_settings.
  # this is needed to be passed into helper functions like make_vdf_angled
  # these updated settings will also be returned as part of the list of values this function returns

  # Check that if either both varlocations or varspace* arguments are provided
  # or neither are provided.
  if(!is.null(model_settings$varlocations) &
     (is.null(model_settings$varspace_x_size) |
      is.null(model_settings$varspace_y_size)) |
     is.null(model_settings$varlocations) &
     (!is.null(model_settings$varspace_x_size) |
      !is.null(model_settings$varspace_y_size))) {
    stop("varlocations and varspace arguments in model_settings must both be provided if providing one or the other.")
  }

  # Check that the length of all varspace arguments are one less than
  # the number of variables
  nsizes <- length(model_list$variables) - 1
  if(!(is.null(model_settings$varspace_x_size)) |
     !(is.null(model_settings$varbox_y_size))) {
    if(!length(model_settings$varspace_x_size) %in% c(1, nsizes) |
       !length(model_settings$varspace_y_size) %in%  c(1,nsizes)) {
      stop("varspace arguments must be of length 1 or the number of variables minus 1.")
    }
  }



  ## TODO(andrew,andreas): Finalize varspace* and varbox* usage
  ## It is required for add_locations(), but
  ## we could drop its usage there or simplify...

  nvars = length(model_list$variables)
  if (is.null(model_settings$varlocations)) {model_settings$varlocations = matrix(model_list$variables,nrow=1)}
  if (is.null(model_settings$varbox_x_size)) {model_settings$varbox_x_size = rep(0.5,nvars)}
  if (is.null(model_settings$varbox_y_size)) {model_settings$varbox_y_size = rep(0.5,nvars)}
  if (is.null(model_settings$varspace_x_size)) {model_settings$varspace_x_size = rep(1,nvars-1)}
  if (is.null(model_settings$varspace_y_size)) {model_settings$varspace_y_size = rep(1,nvars-1)}

  ######################################################################
  # Vectorize all entries box/space size entries
  # If user provided a single number for box and space size, we turn it into vectors here
  # this way we can consistently operate on vectors of the right size everywhere
  if (length(model_settings$varbox_x_size)==1) {model_settings$varbox_x_size = rep(model_settings$varbox_x_size,nvars)}
  if (length(model_settings$varbox_y_size)==1) {model_settings$varbox_y_size = rep(model_settings$varbox_y_size,nvars)}
  if (length(model_settings$varspace_x_size)==1) {model_settings$varspace_x_size = rep(model_settings$varspace_x_size,nvars-1)}
  if (length(model_settings$varspace_y_size)==1) {model_settings$varspace_y_size = rep(model_settings$varspace_y_size,nvars-1)}


  #############################################
  #############################################
  # At this stage, all input checking and processing should be done
  #############################################
  #############################################




  #############################################
  # Some processing/definitions to make code below
  # more concise
  #############################################


  # This pulls out all list elements in model_settings and assigns them
  # to individual variables with their respective names
  # this is done for convenience so we don't have to keep calling
  # model_settings$varlocations and can just call varlocations, etc
  for(i in 1:length(model_settings)) {
    assign(names(model_settings)[i], value = model_settings[[i]])
  }

  #assign to variables outside of model_list
  #basically same as above for model_settings
  variable_names <- model_list$variables  # vector of names
  flows_list <- model_list$flows  # a list flows for each variable

  #number of variables/compartments in model
  nvars <- length(variable_names)


  #############################################
  #############################################
  # Code block that starts processing variables
  # This code block uses these helper functions:
  # add_locations()
  #############################################
  #############################################

  # Create a data frame for all variables
  variables <- data.frame(
    id = 1:nvars,  # numeric id for nodes
    name = variable_names  # names for labels
  )

  #############################################
  # Add location information for each variable and add to data frame
  # See comments within function for details
  # this function only adds location information to real/named variables
  # provided by the user.
  variables <- add_locations(
    variables,
    varlocations,
    varbox_x_size,
    varbox_y_size,
    varspace_x_size,
    varspace_y_size
  )




  #############################################
  #############################################
  # Code block that starts processing flows
  # This code block uses these helper functions:
  # add_plus_signs()
  # get_vars_pars()
  #############################################
  #############################################

  #add implicit + signs to make explicit before additional parsing
  flows_list <- add_plus_signs(flows_list)

  #turns flow list into matrix, adding NA
  #(from modelbuilder code base)
  #variables are along rows and flows along columns.
  flowmat <- t(sapply(flows_list, `length<-`, max(lengths(flows_list))))

  # if there are just two variables and a single flow between them,
  # the flowmat is oriented incorrectly (nodes across columns). this
  # can be diagnosed by checking to see if flowmat has rownames.
  # if not, the matrix needs to be transposed.
  if(is.null(rownames(flowmat))) {
    flowmat <- t(flowmat)
  }

  #strip leading +/- from flows and replace with no space
  flowmatred <- sub("\\+|-","",flowmat)

  #extract only the + or - signs from flows so we know the direction
  signmat <- gsub("(\\+|-).*","\\1",flowmat)

  ############################################################
  #Loop over all variables, for each variable, loop over flows
  ############################################################
  #create a flows data frame for storing the flow information
  flows <- data.frame()  # empty until binded to during first iteration of loop

  #start loop over variables (rows in the flowmatred matrix)
  for(i in 1:nrow(flowmatred))
  {
    varflowsfull <- flowmat[i, ] #all flows with sign for current variable
    varflows <- flowmatred[i, ] #all flows for current variable
    varflowsigns <- signmat[i, ] #signs of flows for current variable

    #remove NA entries because these only show up to match the
    #matrix dimensions needed given the variable with the largest
    #number of flows in/out.
    varflows <- varflows[!is.na(varflows)]

    #start loop over all the flows in/out of the current variable (node)
    for(j in 1:length(varflows)) {
      currentflowfull <- varflowsfull[j]
      currentflow <- varflows[j]
      currentsign <- varflowsigns[j]

      # Find the variables for which the current flow appears, i.e., what
      # other rows of the matrix does it show up in.
      connectvars <- unname(which(flowmatred == currentflow, arr.ind = TRUE)[,1])

      # Extract the variable names in the flow expression
      varspars <- unique( get_vars_pars(currentflowfull))
      varfirsts <- substr(varspars, start = 1, stop = 1)  #get first letters

      #vars is now a vector of the variables that are in the flow math
      # AH: DOES THIS WORK OF VARIABLES HAVE THE SAME STARTING LETTER, SAY P1, P2, P3?
      # ATT: Yes. This bit of code is designed simply to extract any variables
      #      that start with an upper case letter (state variable) and are
      #      present in the current flow. So, if P1 and P2 are in this flow
      #      they both will be found.
      varvec <- varspars[which(varfirsts %in% LETTERS)]  #variables are UPPERCASE

      #extract the numeric ids for the variables in this flow
      varsids <- variables[which(variables$name %in% varvec), "id"]

      # add a connecting variable if the expression is only in one row but
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
        if(length(connectvars) == 1 & length(varvec) == 0) {
          connectvars <- i
        }

        # If the flow does not show up in any other rows (connectvars == 1)
        # and there is at least one variable in the flow math, then the
        # connecting variable(s) will either be the current variable once
        # (indicating an inflow like births) or the current variable twice
        # (indicating a feedback flow)
        if(length(connectvars) == 1 & length(varvec) >= 1){

          # if the current (i) variable does not show up in the flow math
          # then the connecting variable is just the current variable once,
          # indicating a independent inflow from out of the system (e.g., birth)
          if(!variables[i] %in% varvec) {
            connectvars <- i
          }

          # is the the current (i) variables shows up in the flow math, then
          # the connecting variables are the current variable twice, indicating
          # a feedback loop
          if(variables[i] %in% varvec) {
            connectvars <- c(i, i)
          }
        }

        # If there are more than one unique connecting variables, then
        # the connecting variables are simply those defined above by
        # searching the matrix of flows and/or the variables in the expression
        if(length(connectvars) > 1) {
          connectvars <- connectvars
        }
      } #end function block for inflows


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
        flows <- dplyr::bind_rows(flows, tmp)
      } #end function block for outflows

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
          flows <- dplyr::bind_rows(flows, tmp)
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
        flows <- dplyr::bind_rows(flows, tmp)
      }

      # interaction flag if two variables are in the flow
      if(length(varvec) > 1) {
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

    }  #end loop over all flows for a given variable
  }  #end loop over all variables

  ############################################################
  ############################################################
  # finished creating all flows for dataframe
  ############################################################
  ############################################################


  ############################################################
  # some cleanup of flow data frame
  # things that the code above didn't do quite right
  ############################################################

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

  # set up columns needed to define interactions and subset out for interaction
  # settings separate from other flows. these are all merged back together
  # after interactions are given appropriate settings.
  flows$linkfrom <- NA  #empty column for interaction flows, but needed for binding
  flows$linkto <- NA  #empty column for interaction flows, but needed for binding
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
      v <- get_vars_pars(tmp$label)  #strips away math, leaving just letters
      vf <- substr(v, start = 1, stop = 1)  #get first letters
      v <- v[which(vf %in% LETTERS)]  #subset to upper case VARIABLES
      ids <- subset(variables, name %in% v)[ , "id"]  #extract the relevant numeric ids

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
    flows <- dplyr::bind_rows(flows, ints, intflows)
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
  # or go to. We need these to start placing arrows in the correct spots, the
  # start and end positions.

  # Out of the system
  outdummies <- NULL  # make a null object so it can be checked easily later
  # find the number of outflows external from the system, which is the
  # length of the "to" column once the flows are subsetted to rows
  # that have NA in the "to" column (i.e., that don't go to a variable in the
  # system) and the interaction flag is FALSE
  numnas <- length(flows[is.na(flows$to) & flows$interaction == FALSE, "to"])
  if(numnas > 0) {
    # vectorized approach for making a numeric sequence starting at
    # 9991 and going through 999n, where n i numnas.
    # NOTE: The implicit assumption here is that there are no more than
    #       nine (9) outflows external to system.
    outdummies <- as.numeric(paste0("999", c(1:numnas)))

    # assign the new numeric ids to the subset of flows that are outflows
    # external of the system (same subset as above to get the number
    # of outflows)
    flows[is.na(flows$to) & flows$interaction == FALSE, "to"] <- outdummies
  }

  # In to the system
  indummies <- NULL # make a null object so it can be checked easily later
  # here we find the number of inflows, defined by flows that do not have a
  # "from" (from = NA) and are not interactions
  numnas <- length(flows[is.na(flows$from) & flows$interaction == FALSE, "from"])
  if(numnas > 0) {
    # inflows start with numeric id -9991 and go through -999n, where
    # n is numnas.
    # NOTE: The implicit assumption here is that there are no more than
    #       nine (9) inflows external to system.
    indummies <- as.numeric(paste0("-999", c(1:numnas)))

    # assign the new numeric ids to the subset of flows that are inflows
    # external of the system (same subset as above to get the number
    # of inflows)
    flows[is.na(flows$from) & flows$interaction == FALSE, "from"] <- indummies
  }

  # Make dummy compartment for "links" in interactions
  linkdummies <- NULL  # make a null object so it can be checked easily later
  # links are arrows that have no "to" because they intersect another arrow.
  # so to = NA AND interation = TRUE AND linto is not NA defines links
  numlinks <- length(flows[is.na(flows$to) &
                           flows$interaction == TRUE &
                           !is.na(flows$linkto), "to"])
  if(numlinks > 0) {
    # linkdummies start with numeric id 5551 and go through -555n, where
    # n is numlinks.
    # NOTE: The implicit assumption here is that there are no more than
    #       nine (9) links.
    linkdummies <- as.numeric(paste0("555", c(1:numlinks)))

    # assign the new numeric ids to the subset of flows that are links
    # between a variable and a flow (same subset as above to get the number
    # of links)
    flows[is.na(flows$to) &
            flows$interaction == TRUE &
            !is.na(flows$linkto), "to"] <- linkdummies
  }



  # Add dummy compartments to nodes dataframe
  # only do this is at least one of the objects created above is numeric
  # since we made all the objects exist and NULL, this works regardless
  # of how many of the objects actually have new ids
  if(is.numeric(outdummies) | is.numeric(indummies) | is.numeric(linkdummies)) {
    exnodes <- data.frame(id = c(outdummies, indummies, linkdummies),  # the new ids
                          name = NA) #,  # no names because they are dummies
                          #row = 1)  # assume they are on row 1, this gets updated later if needed, but we need a value here for rbinding

    # TODO: Remove commented line below after testing, might not be needed
    # exnodes[setdiff(names(variables), names(exnodes))] <- NA
    #variables <- rbind(variables, exnodes)
    #switching to bind_rows so I can combine data frames with unequal number of columns
    #missing columns in exnodes are set to NA
    variables <- dplyr::bind_rows(variables, exnodes)
  }



  # update inflow node positions from nowhere (e.g. births)
  # these are identified by any id less than -9990
  inflownodes <- subset(variables, id < -9990)$id
  # loop over the inflownode ids and find the flow that connects to the
  # current id. Then extract the id of the variable to which the flow
  # goes to. The location of each "dummy" variable is then defined relative
  # to the "real" variable to which the flow goes in to. By default, we place
  # the dummy variable box varspace_y_size units above the real variable box.
  for(id in inflownodes) {
    # find the id of the variable to which this dummy goes in to
    newxyid <- flows[which(flows$from == id), "to"]
    # extract the location information of the "to" variable
    newxy <- variables[which(variables$id == newxyid), c("xmin", "xmax", "ymin", "ymax")]
    # update box locations by moving the y locations up

    ## TODO(andrew): the mean() usage here is a workaround. update once
    ## decision is made about varspace* and varbox*
    newxy$ymax <- newxy$ymax + mean(varspace_y_size)  # above the variable
    newxy$ymin <- newxy$ymin + mean(varspace_y_size)  # above the variable

    # add in the new location information to replace the NAs
    variables[which(variables$id == id), c("xmin", "xmax", "ymin", "ymax")] <- newxy

    # caluclate midpoints for labels as the means
    newmids <- c((newxy$xmin+newxy$xmax)/2, (newxy$ymin+newxy$ymax)/2)

    # add in the new midpoints as label locations to replace the NAs
    variables[which(variables$id == id), c("xlabel", "ylabel")] <- newmids
  } #end loop over inflownodes

  # update outflow node positions to nowhere (e.g., deaths)
  # these are identified by any id greater than -9990
  outflownodes <- subset(variables, id > 9990)$id
  # loop over the outflownode ids and find the flow that connects to the
  # current id. Then extract the id of the variable from which the flow
  # originates. The location of each "dummy" variable is then defined relative
  # to the "real" variable to which the flow goes in to. By default, we place
  # the dummy variable box varspace_y_size units above the real variable box.
  for(id in outflownodes) {
    # find the id of the variable from which the dummy originates
    newxyid <- flows[which(flows$to == id), "from"]
    # extract the location information of the "from" variable
    newxy <- variables[which(variables$id == newxyid), c("xmin", "xmax", "ymin", "ymax")]
    # update box locations by moving the y locations down

    ## TODO(andrew): the mean() usage here is a workaround. update once
    ## decision is made about varspace* and varbox*
    newxy$ymax <- newxy$ymax - mean(varspace_y_size)   # below the variable
    newxy$ymin <- newxy$ymin - mean(varspace_y_size)   # below the variable

    # add in the new location information to replace the NAs
    variables[which(variables$id == id), c("xmin", "xmax", "ymin", "ymax")] <- newxy

    # caluclate midpoints for labels as the means
    newmids <- c((newxy$xmin+newxy$xmax)/2, (newxy$ymin+newxy$ymax)/2)

    # add in the new midpoints as label locations to replace the NAs
    variables[which(variables$id == id), c("xlabel", "ylabel")] <- newmids
  } #end loop over outflownodes

  # update invisible interaction link nodes, i.e., nodes that need to sit
  # at the midpoint of some other arrow, but not be drawn
  # these are identified by ids that are greater than 5550 and less than 9990
  linknodes <- subset(variables, id > 5550 & id < 9990)$id
  # loop over the linknode ids and find the variables that the arrow is linking
  # e.g., if there is a flow from S -> I and that flow is mediated by the
  # number in S and I, there is the a solid flow of mass (->) from S to I and
  # a linking arrow that goes from I to the middle of the arrow from S to I.
  # So our goal here is to find the mass flow arrow and define a node at the
  # midpoint so the linking arrow has an end point.
  for(id in linknodes) {
    # find the starting point for the mass flow arrow
    start <- flows[which(flows$to == id), "linkfrom"]
    # find the end point for the mass flow arrow
    end <- flows[which(flows$to == id), "linkto"]
    # the new x location is the midpoint, which we can define as the mean
    # of the midpoints (label locations) of the two variable nodes that
    # are being connected. This is done in both the x and y positions.
    newx1 <- variables[which(variables$id == start), "xlabel"] #middle of from node
    newx2 <- variables[which(variables$id == end), "xlabel"] #middle fo to node
    newx <- (newx1+newx2)/2  # midpoint of the physical arrow
    newy1 <- variables[which(variables$id == start), "ylabel"] #middle of from node
    newy2 <- variables[which(variables$id == end), "ylabel"] #middle of to node
    newy <- (newy1+newy2)/2  # midpoint of the physical arrow

    # replace the NA locations with the new x,y locations
    variables[which(variables$id == id), c("xlabel", "ylabel")] <- c(newx, newy)

    # set min/max to midpoint for ease because these are not actually
    # drawn, therefore rectangle boundaries do not need to be accurate
    variables[which(variables$id == id), c("xmin", "ymin")] <- c(newx, newy)
    variables[which(variables$id == id), c("xmax", "ymax")] <- c(newx, newy)
  }


  #########################
  # At this point, all variables, both real ones and dummy ones, have been processed.
  # The remaining code deals with creating and placing the flows into/out-of variables, as well as interaction flows.
  # The variables will only be manipulated one more time below to remove the dummy variables
  #########################


  # Subset out interactions to in/out flows
  # these are arrows that are drawn from a variable to an external (birth/death)
  # or feedback flow. The predator-prey model is an example of this. These
  # are defined in our flows data frame as flows that are interactions
  # but do not have a "linkto" id because they only come from a variable.
  # the external interactions are subsetted out and given some id information
  # to be processed later and then binded back to the "core" flows after they
  # have been processed, too.
  extints <- subset(flows, interaction == TRUE & is.na(linkto))
  if(nrow(extints) > 0) {  #only do this loop if there is something to loop over, avoids errors
    for(i in 1:nrow(extints)) {
      tmp <- extints[i, ]  #get the row to process
      v <- get_vars_pars(tmp$label)  #remove the math notation
      vf <- substr(v, start = 1, stop = 1)  #get first letters of each character element
      v <- v[which(vf %in% LETTERS)]  #subset v to just variables (no parameters)
      ids <- subset(variables, label %in% v)[ , "id"] #get the ids for all variables in the flow notation
      id <- ids[which(ids != tmp$from)] #subset to the id that does not equal the id from which the flow originates
      extints[i, "to"] <- id #set the "to" column to the id at which the flow should terminate
    }
  }


  # Create segment coordinates by merging with node locations
  # first merge by the from locations (starts)
  flows <- merge(flows, variables[,c("xmin", "xmax", "ymin", "ymax", "xlabel", "ylabel", "id")],
               by.x = "from", by.y = "id")
  # now merge by the to locations (ends). the suffixes arguments adds start
  # and end to duplicate columns.
  flows <- merge(flows, variables[,c("xmin", "xmax", "ymin", "ymax", "xlabel", "ylabel", "id")],
               by.x = "to", by.y = "id", suffixes = c("start", "end"))

  # add columns to be populated. these will be population by either the
  # "start" or "end" positions defined above by the merge, dependent on the
  # relationships between the start and end positions themselves. see below
  # for logic
  flows$xmin <- NA_real_
  flows$xmax <- NA_real_
  flows$ymin <- NA_real_
  flows$ymax <- NA_real_



  # update arrow start and end points based on relationship between to
  # and from positions
  for(i in 1:nrow(flows)) {
    tmp <- flows[i, ]
    # if the start and end variables are in the same row (y = y) AND
    # the start and end variables are in different columns (x != x), then
    # we set the y values for start and end to the mean of the y start
    # variable box (the middle) and the xmin location is the max x of
    # the left-most (starting) box and the min x of the right-most (ending) box
    if(tmp$yminstart == tmp$yminend & tmp$xminstart != tmp$xminend) {
      flows[i, "xmin"] <- tmp$xmaxstart
      flows[i, "xmax"] <- tmp$xminend
      flows[i, "ymin"] <- mean(c(tmp$yminstart, tmp$ymaxstart))
      flows[i, "ymax"] <- mean(c(tmp$yminstart, tmp$ymaxstart))
    }

    # if the start variable is above the end variable (y1 > y2) AND
    # the start and end variables are in the same column (x = x), then
    # we set the ymin of the arrow the bottom of the originating box and
    # the ymax of the arrow to the top of the terminating box. the x location
    # for start and end is set to the middle of the box (mean of top and bottom)
    if(tmp$yminstart > tmp$yminend & tmp$xminstart == tmp$xminend) {
      flows[i, "xmin"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "xmax"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "ymin"] <- tmp$yminstart
      flows[i, "ymax"] <- tmp$ymaxend
    }

    # if the start variable is below the end variable (y1 < y2) AND
    # the start and end variables are in the same column (x = x), then
    # we set the ymin of the arrow the top of the originating box and
    # the ymax of the arrow to the bottom of the terminating box. the x location
    # for start and end is set to the middle of the box (mean of top and bottom)
    if(tmp$yminstart < tmp$yminend & tmp$xminstart == tmp$xminend) {
      flows[i, "xmin"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "xmax"] <- mean(c(tmp$xminstart, tmp$xmaxstart))
      flows[i, "ymin"] <- tmp$ymaxstart
      flows[i, "ymax"] <- tmp$yminend
    }

    # if the flow is a non-direct interaction (e.g., intersects a mass flow),
    # then the flow starts at the top-middle of the originating box and
    # the pre-defined end point of the dummy variable
    if(tmp$interaction == TRUE & tmp$direct_interaction == FALSE) {
      flows[i, "xmin"] <- tmp$xlabelstart  # middle of originating box
      flows[i, "xmax"] <- tmp$xminend  # middle of arrow, based on preprocessing
      flows[i, "ymin"] <- tmp$ymaxstart  # top of originating box
      flows[i, "ymax"] <- tmp$ylabelend  # middle of arrow, based on preprocessing
    }

    # if the start variable is above the ending variable (y1 > y2) AND
    # the start variable is to the left of the ending variable (x1 > x2), then
    # the flow start is set to the right-middle of the originating box and
    # the flow end is set to the left-middle of the terminating box. this
    # creates an angled flow arrow pointing down and to the right.
    if(tmp$yminstart > tmp$ymaxend & tmp$xmaxstart < tmp$xminend) {
      flows[i, "xmin"] <- tmp$xmaxstart # right side of originating box
      flows[i, "xmax"] <- tmp$xminend  # left side of terminating box
      flows[i, "ymin"] <- tmp$ylabelstart # middle of originating box
      flows[i, "ymax"] <- tmp$ylabelend # middle of terminating box
    }

    # if the start variable is below the ending variable (y1 < y2) AND
    # the start variable is to the right of the ending variable (x1 < x2), then
    # the flow start is set to the left-middle of the originating box and
    # the flow end is set to the right-middle of the terminating box. this
    # creates an angled flow arrow pointing up and to the left.
    if(tmp$yminstart < tmp$ymaxend & tmp$xmaxstart < tmp$xminend) {
      flows[i, "xmin"] <- tmp$xmaxstart # left side of originating box
      flows[i, "xmax"] <- tmp$xminend # right side of terminating box
      flows[i, "ymin"] <- tmp$ylabelstart # middle of originating box
      flows[i, "ymax"] <- tmp$ylabelend # middle of terminating box
    }

    # if the flow starts and ends in the same place, this is a feedback
    # flow that needs minor offsets in the x direction.
    if(tmp$xmaxstart == tmp$xmaxend & tmp$ymaxstart == tmp$ymaxend) {
      flows[i, "xmin"] <- tmp$xlabelstart - 0.25  # minor offset to the left for start
      flows[i, "xmax"] <- tmp$xlabelend + 0.25  # minor offset to the right for end
      flows[i, "ymin"] <- tmp$ymaxend  # top of the originating box
      flows[i, "ymax"] <- tmp$ymaxend  # top of the originating box
    }
  }



  # remove unneeded columns
  flows[ , c("xminstart", "xmaxstart", "yminstart", "ymaxstart",
           "xlabelstart", "ylabelstart", "xminend", "xmaxend",
           "yminend", "ymaxend", "xlabelend", "ylabelend")] <- NULL


  # label locations are mid points, which are means of the start and end positions
  # a minor offset is applied to the ylabel location to get the label slightly
  # above arrows. This tends to work OK regardless of flow direction
  flows$xlabel <- with(flows, (xmax + xmin) / 2)
  flows$ylabel <- with(flows, (ymax + ymin) / 2) + 0.25  # label slightly above the arrrow

  # add a diff column so we can identify flows that traverse more than
  # one variable. these will be updated to have curvature that goes over
  # or under the nodes it is bypassing. works best with just a couple. if there
  # is lots of traversing, then manual intervention will be required by the user
  flows$diff <- with(flows, abs(to-from))


  ## TODO(andrew): Remove after exhaustive testing...don't think it is needed
  ##      anymore.
  # if(!is.null(varlocation_matrix)) {
  #   xdiffs <- with(flows, abs(xmin - xmax))
  #   xdiffs <- ifelse(xdiffs %in% c(0, 3), 0.5, 1)
  #   ydiffs <- with(flows, abs(ymin - ymax))
  #   ydiffs <- ifelse(ydiffs %in% c(0, 2), 0.5, 1)
  #   for(i in 1:nrow(flows)) {
  #     if(flows[i, "interaction"] == FALSE &
  #        flows[i, "direct_interaction"] == FALSE &
  #        flows[i, "to"] < 9900 &
  #        flows[i, "from"] > -9900) {
  #       flows[i, "diff"] <- xdiffs[i] + ydiffs[i]
  #     }
  #   }
  # }


  # update vertical edges to go in and out at angles
  flows <- make_vdf_angled(flows, variables, model_settings)

  # update vertical edges to avoid overlaps
  flows <- fix_arrow_pos(flows)

  # set default curvature of all flows
  flows <- set_curvature(variables, flows)


  # set curvature of feedback loops. this is pretty different from the
  # "regular" curvature settings, so we made a separate function for this
  # operation.
  flows <- set_feedback_curvature(flows)

  # update external interaction arrows now that all other positioning
  # is final
  extints <- update_external_interaction_positions(extints, variables)


  # combine all flows
  flows <- dplyr::bind_rows(flows, extints)

  # now drop "hidden" nodes without labels
  variables <- subset(variables, !is.na(name))

  # set to/from columns in flows to NA if value is not in node dataframe
  flows <- set_node_to_na(flows, variables)

  # remove rows with no location information
  flows <- remove_na_rows(flows)

  # convert direct interaction to flag to regular interaction flag,
  # now only relevant for plotting
  flows <- update_interactions(flows)

  # update all to and froms such that each is the variable label
  # until now, the to/from in flows has just been numeric. these
  # need to be character strings for the variable labels for plotting
  flows <- update_tofroms(flows, variables)

  # update flow labels for straight connecting flows that run vertically
  flows <- update_straight_labels(flows)


  # update interaction column to be type column, one of
  # main, interaction, or external. this is needed for plotting
  flows$type <- "main"  # intialize the column as all "main" flows
  # set the interaction flows according to the interaction flag
  flows$type <- ifelse(flows$interaction == TRUE, "interaction", flows$type)
  # external flows are not interactions and either the to or from location is dummy (NA)
  flows$type <- ifelse(flows$interaction == FALSE & (is.na(flows$to) | is.na(flows$from)),
                       "external", flows$type)
  flows$interaction <- NULL  # remove the interaction column

  #sort flows by type, main/external/interaction
  flows <- dplyr::bind_rows(
    flows[flows$type == "main", ],
    flows[flows$type == "external", ],
    flows[flows$type == "interaction", ]
  )

  #add a row id so it's easier for users to know which row to alter
  flows$id = 1:nrow(flows)

  # add a math column to differentiate from label if needed
  flows$math <- flows$label

  # update flows column ordering
  flows <- flows[, c("id", "from", "to", "label", "xmin", "xmax", "ymin", "ymax",
                     "xlabel", "ylabel", "curvature", "type", "math")]


  #remove row names, those are confusing
  rownames(flows) <- NULL
  rownames(variables) <- NULL

  # add default aesthetics
  dflist <- add_default_aes(variables, flows)

  return(dflist)
}
