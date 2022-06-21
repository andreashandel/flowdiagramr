#' Create data frames for plotting from model elements.
#'
#' @description
#' This function takes as input a model consisting of variables/compartments
#' and flows, and creates a list of data frames with label and position
#' information for plotting a flow diagram.
#' The resulting object is used as an input to \code{\link{make_diagram}},
#' which creates a **ggplot2** based diagram. The function attempts to make
#' decent decisions regarding the placement of variables (boxes),
#' flows (arrows), and labels. However, complex models with complex diagrams
#' will likely need user modification. This is documented in the vignettes.
#'
#' @param model_list A list of model elements. This list is required and
#' must contain these two elements:
#' \itemize{
#' \item `variables`: A character vector specifying the names of all variables.
#' \item `flows`: A list that is the same length as `variables`. Each sub-list
#'     element is a character vector of any length specifying the flows into
#'     and out of the variable. Note that **flowdiagramr** assumes that the
#'     order of `flows` matches that of the `variables` vector.
#' \item IMPORTANT: All `variables` entries must start with an upper case letter,
#'     followed by any combination of letters and numbers
#'     (e.g., S, Si, or Aml2). All parameters contained in `flows` must start
#'     with a lower case letter followed by any combination of letters and
#'     numbers (e.g., b, bBmax, kS, p21S). All variables and parameters MUST
#'     be separated by math notation (e.g., +, -, *, /). Most math functions
#'     (e.g., `sin`, `cos`) are currently not supported.
#' \item See examples and details below and vignettes.
#' }
#'
#' @param model_settings A list of optional settings to adjust layout. The
#'     following elements are supported. If not provided, they default to a
#'     single row and all sizes of 1.
#' \itemize{
#' \item `varlocations`: A matrix containing all `model_list$variables` entries
#'     in specific locations on a grid. See examples.
#' \item `varbox_x_size`: Either a scalar or a vector that changes the default
#'     width of variable boxes. For example, `varbox_x_size = 1.5` makes each
#'     box 1.5 units in width. If a scalar, the value is used for all variables.
#'     If a vector, the length must correspond to number of variables,
#'     and the provided values are applied to the variables in the order
#'     provided in `model_list$vars`.
#' \item `varbox_y_size`: Same as `varbox_x_size` but for the height of
#'     the boxes.
#' \item `varspace_x_size`:  Either a scalar or a vector that changes the
#'     spacing between variable boxes in the x/horizontal dimension. If
#'     `varspace_x_size` is a scalar, all spaces between boxes in the x
#'     direction will be the same. For example, `varspace_x_size = 1.5` puts
#'     1.5 units of space in the x direction between boxes. If you provide a
#'     vector, it needs to be of dimension one less than the number of columns
#'     in `varlocations`. Spacing starts at the left, thus the first number is
#'     the spacing between the first column and second column, etc. Spacing
#'     is measured between the right edge of one box to the left edge of
#'     the next box.
#' \item `varspace_y_size`:  Same as `varspace_y_size` but for the vertical
#'     dimension. If you provide a vector, it needs to be of dimension one
#'     less than the number of rows in `varlocations`. Spacing starts at the
#'     bottom, thus the first number is the spacing between the lowest and
#'     second lowest row, etc. Spacing is measured between the bottom edge
#'     of one box to the top edge og the next box.
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
#'   \itemize{
#'     \item `id`: A numeric id for each variable.
#'     \item `name`: The name of the variable as provided in the model
#'         specification.
#'     \item `xmin`: Left edge location of variable box.
#'     \item `xmax`: Right edge location of variable  box.
#'     \item `ymin`: Lower edge of location variable box.
#'     \item `ymax`: Upper edge of location variable  box.
#'     \item `xlabel`: Horizontal position (midpoint) of label.
#'     \item `ylabel`: Vertical position (midpoint) of label.
#'     \item `label_text`: Text that will appear as the label of the box. Can
#'         be different from `name`.
#'     \item `outline_color`: The outline color of variable boxes.
#'     \item `fill_color`: The fill color of the variable boxes.
#'     \item `label_color`: The color of the box labels for each variable.
#'     \item `label_size`: Text size for variable labels.
#'   }
#'
#'   \item `flows`: A data frame containing information for all flows.
#'   The data frame contains these columns:
#'   \itemize{
#'     \item `id`: A numeric id for each flow.
#'     \item `name`: The name of the flow. Typically a mathematical expression.
#'         If a main flow with an interaction, this name is for id purposes
#'         only because the `label_text` will be the actual label displayed
#'         in the diagram. Thus, the name might be duplicated in other rows.
#'     \item `type`: Type of flow. One of main, interaction, or external.
#'     \item `from`: The variable from which the arrow originate. That is, the
#'         variable donating the flow.
#'     \item `to`: The variable to which the arrow will point. That is, the
#'         variable receiving the flow.
#'     \item `xstart`: The starting horizontal position of the arrow.
#'     \item `xend`: The ending horizontal position of the arrow.
#'     \item `ystart`: The starting vertical position of the arrow.
#'     \item `yend`: The ending vertical position of the arrow.
#'     \item `xlabel`: Horizontal position (midpoint) of label.
#'     \item `ylabel`: Vertical position (midpoint) of label.
#'     \item `curvature`: The amount of curvature applied to arrow.
#'         Higher numbers indicate more curvature; 0 = straight line.
#'     \item `label_text`: The label that will appear in the diagram. This is a
#'         duplicate of `name` so that user can update `label_text` as desired
#'         but retain the original math for reference.
#'     \item `line_color`: The color of the flow arrow line.
#'     \item `line_size`: The size (width) of the flow arrow line.
#'     \item `line_type`: The linetype of the flow arrow line.
#'     \item `label_color`: Color of `label_text`.
#'     \item `label_size`: The text size of `label_text`.
#'     \item `arrow_size`: The size of the arrow point on the flow line.
#'     \item `show_arrow`: Logical for whether to plot the flow arrow line
#'         (TRUE) or not (FALSE).
#'   }
#' }
#' @details
#'    `variables` needs to be specified as a vector of model variables,
#'     e.g., `variables <- c("Pred","Prey")`. `flows` need to be specified as a
#'    list, with each list entry containing the flows/processes for each
#'    variable in the order in which the variables appear. Flows need to be
#'    named according to `VARIABLENAME_flows`.
#'
#'    Example:
#'
#'    \code{flows <- list(Pred_flows = c(r*Pred, -k1*Pred*Prey),
#'    Prey_flows = c(g*Prey, -k2*Pred*Prey))}. Each flow, i.e. each entry in
#'    the flow vector, needs to be a valid mathematical expression made up of
#'    variables and parameters. The rules are as described above.
#'    As an example, the following includes a parameter *b* and two variables,
#'    *S* and *I*: `b*S*I`. The following includes a parameter *s* and two
#'    variables, *Bg* and *I2*: `Bg*s*I2`. See more examples below and in
#'    the vignettes.
#'
#'    The variables and flows data frames returned in the output list from this
#'    function contain a few columns that are provided to make it easier for
#'    the user to make changes to the data frames manually, but are not used
#'    by the package to make the diagram itself. In the `variables` data frame,
#'    `id` and `name` are unique identifiers that are not used by the package
#'    to make the diagram -- changing these will have no impact on the final
#'    diagram. In the `flows` data frame, `id`, `name`, `from`, and `to` are
#'    identifiers provided to make it easier for the user to understand each
#'    row of the data frame. Changing these columns will have no impact on the
#'    final diagram. All other columns contain information that impacts the
#'    drawn diagram itself. Users can update them -- and may want to in many
#'    cases -- but any updates to values in the remaining columns will be seen
#'    in the diagram itself. See the description of the output data frames below.
#'
#' @examples
#' # basic model specification
#' variables <- c("S","I","R")
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' mymodel <- list(variables = variables, flows = flows)
#' diag_list <- prepare_diagram(model_list = mymodel)
#' mydiag <- make_diagram(diag_list)
#'
#'
#'
#' # adding optional specifications
#' varlocations <-  matrix(data = c("S", "", "R",
#'                                  "", "I", "" ),
#'                         nrow = 2, ncol = 3, byrow = TRUE)
#' mysettings <- list(varlocations = varlocations)
#' diag_list <- prepare_diagram(model_list = mymodel, model_settings = mysettings)
#' mydiag <- make_diagram(diag_list)
#'
#'
#'
#' # use of model_settings to change sizes and spacing, including vectorization
#' variables <- c("S","I","R")
#' flows <- list(S_flows = c("-b*S*I"),
#'               I_flows = c("b*S*I","-g*I"),
#'               R_flows = c("g*I"))
#' mymodel <- list(variables = variables, flows = flows)
#'
#' var_locs <- matrix(c("S", "", "R", "", "I", ""), byrow = TRUE, nrow = 2)
#' mysettings = list(
#'   varlocations = var_locs,
#'   varbox_x_size = c(1,2,1),
#'   varbox_y_size = c(0.5,0.5,2),
#'   varspace_x_size = 2,
#'   varspace_y_size = 1)
#'
#' diag_list <- prepare_diagram(model_list = mymodel,
#'                              model_settings = mysettings)
#' make_diagram(diag_list)
#'
#'
#' # another simple model for pathogen (prey) and immune response (predator)
#' variables = c("Pat","Imm")
#' flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
#'                  Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
#' mymodel = list(variables, flows)
#' diag_list <- prepare_diagram(mymodel)
#' mydiag <- make_diagram(diag_list)
#'
#'
#'
#' # manually switch to vertical layout
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
# get_vars_pars()
# set_curvature()
# set_feedback_curvature()
# set_node_to_na()
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

  #############################################################################
  ## CONDUCT PRELIMINARY CHECKS ###############################################
  #############################################################################
  #############################################
  #############################################
  # Code block that does various checks and processing of input
  # This code block uses these helper functions:
  # check_model_list()
  # check_model_settings()
  #############################################
  #############################################

  #check to make sure model_list is provided
  #and is a properly specified model
  if (is.null(model_list))
  {
    stop('Argument model_list is required.')
  }
  checkmsg <- check_model_list(model_list)
  if(!is.null(checkmsg))
  {
    stop(checkmsg)
  }

  #in case the user didn't explicitly name the variable and flow elements, we do it here
  names(model_list) <- c("variables","flows")

  # check all user-provided model_settings to make sure entries are what
  # they should be
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
  # also, vectorize all entries box/space size entries
  ######################################################################
  # For each model_settings component, if user didn't set it,
  # we set a default here
  # default for varlocations is a matrix with a single row
  # If user did not provide values for sizing/spacing,
  # we set vectors of length nvars and nvars-1 for box and space sizing
  # each with the default value of 1
  # note that we assign it to model_settings.
  # these updated settings will also be returned as part of the list of values
  # this function returns

  #first, if varlocations matrix is not provided, make a single-row matrix
  if (is.null(model_settings$varlocations)) {
    model_settings$varlocations = matrix(model_list$variables,nrow=1)
  }

  # determine number of variables, rows and columns
  nvars = length(model_list$variables)
  nrows = nrow(model_settings$varlocations)
  ncols = ncol(model_settings$varlocations)

  # if user didn't provide a value, we use default of 1
  # as many box size numbers as there are boxes/variables
  if (is.null(model_settings$varbox_x_size)) {
    model_settings$varbox_x_size = rep(1, nvars)
  }
  if (is.null(model_settings$varbox_y_size)) {
    model_settings$varbox_y_size = rep(1, nvars)
  }
  # one more row/column less for spacing than is in the matrix
  if (is.null(model_settings$varspace_x_size)) {
    model_settings$varspace_x_size = rep(1, ncols - 1)
  }
  if (is.null(model_settings$varspace_y_size)) {
    model_settings$varspace_y_size = rep(1, nrows - 1)
  }

  # If user provided a single number for box and space size, we turn it into vectors here
  # this way we can consistently operate on vectors of the right size everywhere
  if (length(model_settings$varbox_x_size) == 1) {
    model_settings$varbox_x_size = rep(model_settings$varbox_x_size, nvars)
  }
  if (length(model_settings$varbox_y_size) == 1) {
    model_settings$varbox_y_size = rep(model_settings$varbox_y_size, nvars)
  }

  if (length(model_settings$varspace_x_size) == 1) {
    model_settings$varspace_x_size = rep(model_settings$varspace_x_size, ncols - 1)
  }
  if (length(model_settings$varspace_y_size) == 1) {
    model_settings$varspace_y_size = rep(model_settings$varspace_y_size, nrows - 1)
  }

  #############################################
  #############################################
  # At this stage, all input checking and processing should be done
  #############################################
  #############################################


  ############################################################################
  ## EXTRACT INFORMATION FROM USER-PROVIDED LISTS ############################
  ############################################################################
  #############################################
  #############################################
  # Code block that does some processing
  # to make rest of code more concise
  #############################################
  #############################################

  # This pulls out all list elements in model_settings and assigns them
  # to individual variables with their respective names
  # this is done for convenience so we don't have to keep calling
  # model_settings$varlocations and can just call varlocations, etc
  varlocations <- model_settings$varlocations
  varbox_x_size <- model_settings$varbox_x_size
  varbox_y_size <- model_settings$varbox_y_size
  varspace_x_size <- model_settings$varspace_x_size
  varspace_y_size <- model_settings$varspace_y_size


  #assign to variables outside of model_list
  #basically same as above for model_settings
  variable_names <- model_list$variables  # vector of names
  flows_list <- model_list$flows  # a list flows for each variable


  #############################################
  #############################################
  # End code block that extracts lists
  #############################################
  #############################################





  ############################################################################
  ## PROCESS VARIABLES #######################################################
  ############################################################################
  #############################################
  #############################################
  # Code block that goes through all variables and
  # creates the variables data frame
  # This code block uses these helper functions:
  # add_locations()
  #############################################
  #############################################

  #number of variables/compartments in model
  nvars <- length(variable_names)

  # Create a data frame for all variables
  variables <- data.frame(
    id = 1:nvars,  # numeric id for nodes
    name = variable_names  # names for labels
  )

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
  # End code block that processes variables
  # At this stage, the variable data frame is complete and done
  #############################################
  #############################################





  ############################################################################
  ## PROCESS FLOWS ###########################################################
  ############################################################################
  #############################################
  #############################################
  # Code block that starts processing flows
  # This code block uses these helper functions:
  # add_plus_signs()
  # get_vars_pars()
  #############################################
  #############################################

  # if a flow element is a single empty character string, then no flow is
  # generated and those flows can be removed here
  first_elements <- sapply(flows_list, "[[", 1)
  nonempty_flows <- which(first_elements != "")
  flows_list <- flows_list[nonempty_flows]

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
      varspars <- unique(get_vars_pars(currentflowfull))
      varfirsts <- substr(varspars, start = 1, stop = 1)  #get first letters

      # varfirsts is now a vector of the variables AND parameters that
      # are in the flow math
      # extract any variables that start with an upper case letter
      # (state variable) and are present in the current flow. So, if P1 and P2
      # are in this flow they both will be found.
      varvec <- varspars[which(varfirsts %in% LETTERS)]  #variables are UPPERCASE

      #extract the numeric ids for the variables in this flow
      varsids <- variables[which(variables$name %in% varvec), "id"]


      ####
      ## This first chunk further processes the connectvars vector
      ## information. After this, the flows diagram can be created effectively.
      ## For clarity, this chunk is kept separate from the creation of the
      ## flows data frame below; thus, one may notice redundant IF/THEN
      ## statements.
      ####
      # add a connecting variable if the expression contains only one
      # variable, is only in one row of the flow matrix, and the row in
      # which it occurs does not correspond with the variable in the expression.
      # this is rare. but can occur in predator-prey style models.
      # the multiple condition IF statement checks:
      #  1. That there is one, and only one, variable in the expression
      #  2. The expression occurs in one, and only one, row of the flow matrix
      #  3. That the variable in the expression is not the variable row in which
      #     the expression occurs in the flow matrix.
      # Note that these must be nested.
      if(length(varsids) == 1) {
        if(length(unique(connectvars)) == 1) {
          if(!(unique(connectvars) %in% varsids)) {
            connectvars <- c(connectvars, varsids)
            # also create a flag for adding interaction, this is used below
            flag <- TRUE
          }
        }
      }

      # Assign connecting variables for inflows (+ flows).
      # This block is just to update the connectvars vector. the flows
      # data frame is created using this information below in a separate
      # if/then block
      if(currentsign == "+") {
        # If the flow does not show up in any other rows (connectvars == 1)
        # and there are no variables in the flow math, then the only connecting
        # variable is the current (i) variable
        if(length(connectvars) == 1 & length(varvec) == 0) {
          connectvars <- i
        }

        # If the flow does not show up in any other rows (length(connectvars) == 1)
        # and there is at least one variable in the flow math, then the
        # connecting variable(s) will either be the current variable once
        # (indicating an inflow like births) or the current variable twice
        # (indicating a feedback flow)
        if(length(connectvars) == 1 & length(varvec) >= 1){

          # if the current (i) variable does not show up in the flow math
          # then the connecting variable is just the current variable once,
          # indicating a independent inflow from out of the system (e.g., birth)
          if(!variables$name[i] %in% varvec) {
            connectvars <- i
          }

          # is the the current (i) variables shows up in the flow math, then
          # the connecting variables are the current variable twice, indicating
          # a feedback loop
          if(variables$name[i] %in% varvec) {
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


      ####
      ## This chunk uses information about the sign of the flow and the
      ## connecting variables (connectvars) to generate a flows data frame
      ## with columns for: from, to, label, interation, out_interaction,
      ## and direct_interaction.
      ####
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
                          name = currentflow,
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
                            name = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE,
                            direct_interaction = FALSE)
          flows <- dplyr::bind_rows(flows, tmp)
        }
      }

      # If the current sign is positive and the length of connecting variables
      # is equal to two, then it is:
      #   a feedback loop (1 unique connecting variable)
      #   a physical flow between two unique variables
      #   an interaction flow between to unique variables
      if(currentsign == "+" & length(connectvars) == 2) {
        # These are feedbacks of somekind
        if(length(unique(connectvars)) == 1) {
          tmp <- data.frame(from = i,
                            to = i,
                            name = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE,
                            direct_interaction = FALSE)
        } else {
          # These are physical flows between two variables
          tmp <- data.frame(from = connectvars[connectvars!=i],
                            to = i,
                            name = currentflow,
                            interaction = FALSE,
                            out_interaction = FALSE,
                            direct_interaction = FALSE)

          # update interaction flag if flag exists
          if(exists("flag")) {
            tmp$direct_interaction <- TRUE

            # remove flag from the environment
            rm(flag)
          }
        }
        flows <- dplyr::bind_rows(flows, tmp)
      }

      # add an interaction flag if two variables are in the flow
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
  # finished creating all flows for the flows dataframe
  # not all information is present or correct yet
  # code block below further update the flows DF
  # At this stage, the flows dataframe has the following columns:
  # from, to, name, interaction, out_interaction, direct_interaction
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

  # keep original name for all flows. this gets overwritten when the interaction
  # flow is added. but we want to retain this for later for the user
  flows$orig_name <- flows$name

  # Parse the meaning of duplicate labels. Usually this is a complex mix
  # of a direct, physical flows and interactions from several other
  # state variables. We assume that the "main" flow among the "auxilliary"
  # duplicate flows is the one that traverses left-to-right (e.g., 1 to 2)
  # with the smallest gap and has no interaction flags.
  dups <- as.matrix(table(flows$name))  # tally the occurences of each flow
  dupids <- rownames(dups)[which(dups[,1] > 1)]  # grab the one with >1 occurence
  if(length(dupids) > 0) {
    flowdups <- subset(flows, name %in% dupids)  # take a subset of the edge data frame
    flows <- subset(flows, !(name %in% dupids))  # restrict flows to non-duplicate flows
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
    flows[which(flows$out_interaction == TRUE), "name"] <- ""  # take away the name for the physical flow
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
    intflows$name <- ""  # strip the name from the physical flow
    intflows <- unique(intflows)  # just keep unique flows
    intflows$interaction <- FALSE  # reset interaction to false b/c a main flow now

    # Redefine the from, to, and link columns for the interaction
    # arrows. "to" is NA until updated to meet at the center
    # of the physical flow arrow.
    for(i in 1:nrow(ints)) {
      tmp <- ints[i, ]
      v <- get_vars_pars(tmp$name)  #strips away math, leaving just letters
      vf <- substr(v, start = 1, stop = 1)  #get first letters
      v <- v[which(vf %in% LETTERS)]  #subset to upper case VARIABLES
      ids <- variables[variables$name %in% v, "id"]  #extract the relevant numeric ids

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

  # Keep only distinct rows, but take extra care to avoid uniquness due to
  # original name. this takes a bit of bookkeeping
  tmp <- flows
  tmp$orig_name <- NULL
  tmp <- unique(tmp)
  tmp$orig_name <- flows[rownames(tmp), "orig_name"]
  rm(flows)
  flows <- tmp
  rm(tmp)

  #########################################
  #########################################
  # At this stage, the flows dataframe has the following columns:
  # from, to, name, interaction, direct_interaction, linkfrom, linkto
  #########################################
  #########################################





  #############################################################################
  ## ADD SPATIAL INFORMATION TO FLOWS #########################################
  #############################################################################
  #########################################
  # This next large chunk assigns spatial information to all flows.
  # Spatial information includes: xmin, xmax, xlabel, ymin, ymax, ylabel.
  #########################################
  #########################################

  ####
  ## Direct, physical flows
  ####
  # These are simple flows from one variable to another, identified by
  # flows that have real numbers in the from and to columns, and are also
  # not links
  simple_flows <- subset(flows, !is.na(from) & !is.na(to) & is.na(linkfrom))

  if(nrow(simple_flows) > 0) { # only execute if these exist
    # add columns for to-be added information
    simple_flows$xmin <- NA_real_
    simple_flows$xmax <- NA_real_
    simple_flows$ymin <- NA_real_
    simple_flows$ymax <- NA_real_

    # Loop over the simple flows and identify the relative positions of the
    # to and from variables. This is necessary because start and end points
    # of arrows will change if the alignment of the nodes is horizontal
    # or vertical
    for(i in 1:nrow(simple_flows)) {
      tmp <- simple_flows[i,]
      from_node <- subset(variables, id == tmp$from)
      to_node <- subset(variables, id == tmp$to)

      # if the start and end variables are in the same row (y = y) AND
      # the start and end variables are in different columns (x != x), then
      # we set the y values for start and end to the mean of the y start
      # variable box (the middle) and the xmin location is the max x of
      # the left-most (starting) box and the min x of the right-most (ending) box
      if(from_node$ymin == to_node$ymin & from_node$xmin != to_node$xmin) {
        if(from_node$xmin > to_node$xmin) {
          # flow from right to left if from-node to right of to-node
          simple_flows[i, "xmin"] <- from_node$xmin # left edge
          simple_flows[i, "xmax"] <- to_node$xmax  # right edge
        } else {  # otherwise flow left to right
          simple_flows[i, "xmin"] <- from_node$xmax # right edge
          simple_flows[i, "xmax"] <- to_node$xmin  # left edge
        }
        simple_flows[i, "ymin"] <- mean(c(from_node$ymin, from_node$ymax)) # middle
        simple_flows[i, "ymax"] <- mean(c(to_node$ymin, to_node$ymax)) # middle
      }

      # if the start variable is above the end variable (y1 > y2) AND
      # the start and end variables are in the same column (x ranges overalap),
      # then we set the ymin of the arrow the bottom of the originating box and
      # the ymax of the arrow to the top of the terminating box. the x location
      # for start and end is set to the middle of the box (mean of top and bottom)

      # overlap test for x ranges to apply to all vertical alignments
      overlap_test <- from_node$xmax >= to_node$xmin & from_node$xmin <= to_node$xmax
      if(from_node$ymin > to_node$ymin & overlap_test) {
        simple_flows[i, "xmin"] <- mean(c(from_node$xmin, from_node$xmax)) # middle
        simple_flows[i, "xmax"] <- mean(c(to_node$xmin, to_node$xmax)) # middle
        simple_flows[i, "ymin"] <- from_node$ymin # bottom
        simple_flows[i, "ymax"] <- to_node$ymax # top
      }

      # if the start variable is below the end variable (y1 < y2) AND
      # the start and end variables are in the same column (x = x), then
      # we set the ymin of the arrow the top of the originating box and
      # the ymax of the arrow to the bottom of the terminating box. the x location
      # for start and end is set to the middle of the box (mean of top and bottom)
      if(from_node$ymin < to_node$ymin & overlap_test) {
        simple_flows[i, "xmin"] <- mean(c(from_node$xmin, from_node$xmax)) # middle
        simple_flows[i, "xmax"] <- mean(c(to_node$xmin, to_node$xmax)) # middle
        simple_flows[i, "ymin"] <- from_node$ymax # top
        simple_flows[i, "ymax"] <- to_node$ymin # bottom
      }

      # if the start variable is above the ending variable (y1 > y2) AND
      # the start variable is to the left of the ending variable (x1 < x2), then
      # the flow start is set to the right-middle of the originating box and
      # the flow end is set to the left-middle of the terminating box. this
      # creates an angled flow arrow pointing down and to the right.
      if(from_node$ymin > to_node$ymin & !overlap_test & from_node$xmin < to_node$xmin) {
        simple_flows[i, "xmin"] <- from_node$xmax # right edge
        simple_flows[i, "xmax"] <- to_node$xmin  # left edge
        simple_flows[i, "ymin"] <- mean(c(from_node$ymin, from_node$ymax)) # middle
        simple_flows[i, "ymax"] <- mean(c(to_node$ymin, to_node$ymax)) # middle
      }

      # if the start variable is above the ending variable (y1 > y2) AND
      # the start variable is to the right of the ending variable (x1 > x2), then
      # the flow start is set to the left-middle of the originating box and
      # the flow end is set to the right-middle of the terminating box. this
      # creates an angled flow arrow pointing down and to the left.
      if(from_node$ymin > to_node$ymin & !overlap_test & from_node$xmin > to_node$xmin) {
        simple_flows[i, "xmin"] <- from_node$xmin # left edge
        simple_flows[i, "xmax"] <- to_node$xmax  # right edge
        simple_flows[i, "ymin"] <- mean(c(from_node$ymin, from_node$ymax)) # middle
        simple_flows[i, "ymax"] <- mean(c(to_node$ymin, to_node$ymax)) # middle
      }

      # if the start variable is below the ending variable (y1 < y2) AND
      # the start variable is to the left of the ending variable (x1 < x2), then
      # the flow start is set to the right-middle of the originating box and
      # the flow end is set to the left-middle of the terminating box. this
      # creates an angled flow arrow pointing up and to the right.
      if(from_node$ymin < to_node$ymin & !overlap_test & from_node$xmin < to_node$xmin) {
        simple_flows[i, "xmin"] <- from_node$xmax # right edge
        simple_flows[i, "xmax"] <- to_node$xmin # left edge
        simple_flows[i, "ymin"] <- mean(c(from_node$ymin, from_node$ymax)) # middle
        simple_flows[i, "ymax"] <- mean(c(to_node$ymin, to_node$ymax)) # middle
      }

      # if the start variable is below the ending variable (y1 < y2) AND
      # the start variable is to the right of the ending variable (x1 > x2), then
      # the flow start is set to the left-middle of the originating box and
      # the flow end is set to the right-middle of the terminating box. this
      # creates an angled flow arrow pointing up and to the left.
      if(from_node$ymin < to_node$ymin & !overlap_test & from_node$xmin > to_node$xmin) {
        simple_flows[i, "xmin"] <- from_node$xmin # left edge
        simple_flows[i, "xmax"] <- to_node$xmax # right edge
        simple_flows[i, "ymin"] <- mean(c(from_node$ymin, from_node$ymax)) # middle
        simple_flows[i, "ymax"] <- mean(c(to_node$ymin, to_node$ymax)) # middle
      }

      # if the flow starts and ends in the same place, this is a feedback
      # flow that needs minor offsets in the x direction.
      if(from_node$xmin == to_node$xmin & from_node$ymin == to_node$ymin) {
        middle <- mean(c(from_node$xmin, from_node$xmax))
        simple_flows[i, "xmin"] <- middle - 0.25  # minor offset to the left for start
        simple_flows[i, "xmax"] <- middle + 0.25  # minor offset to the right for end
        simple_flows[i, "ymin"] <- from_node$ymax  # top
        simple_flows[i, "ymax"] <- to_node$ymax  # top
      }
    } # end loop over simple, physical flows
  } # end direct physical flows if/then for existence


  ####
  ## In flows
  ####
  # These flows only have a to id and from is NA
  in_flows <- subset(flows, is.na(from) & !is.na(to) & is.na(linkfrom))

  if(nrow(in_flows) > 0) { # only exectute if these exist
    # The xlabel, ymax locations define the top/middle of the node, which
    # is xmax/ymax for in-flows
    in_flows <- merge(in_flows, variables[,c("xlabel","ymax", "id")],
                      by.x = "to",
                      by.y = "id")
    in_flows$xmax <- in_flows$xlabel
    in_flows$xlabel <- NULL  # remove the column

    # ymin is the y starting point of the arrow, defined as the end point (ymax) + 0.5
    in_flows$ymin <- in_flows$ymax + 0.5

    # xmin is the x starting point of the arrow, defined as left-edge of the node
    left_edges <- variables[,c("id", "xmin")]
    in_flows <- merge(in_flows, left_edges, by.x = "to", by.y = "id")
  } # end in flows if/then for existence


  ####
  ## Out flows
  ####
  # These flows only have a from id and to is NA
  # also cannot be a interaction
  out_flows <- subset(flows, !is.na(from) & is.na(to) &
                              is.na(linkfrom) & interaction == FALSE)

  if(nrow(out_flows) > 0) { # only execute if these exist
    # The xlabel, ymin locations define the bottom/middle of the node, which
    # is xmin/ymin for out-flows
    out_flows <- merge(out_flows, variables[,c("xlabel","ymin", "id")],
                       by.x = "from",
                       by.y = "id")
    out_flows$xmin <- out_flows$xlabel
    out_flows$xlabel <- NULL  # remove the column

    # ymax is the y end point of the arrow, defined as the start point (ymin) - 0.5
    out_flows$ymax <- out_flows$ymin - 0.5

    # xmax is the x end point of the arrow, defined as right-edge of the node
    right_edges <- variables[,c("id", "xmax")]
    out_flows <- merge(out_flows, right_edges, by.x = "from", by.y = "id")
  } # end out flows if/then for existence


  ####
  ## Interaction flows
  ####
  # These are flows where the interaction column is TRUE and both linkfrom
  # and linkto have values
  int_flows <- subset(flows, interaction == TRUE & !is.na(linkfrom) & !is.na(linkto))

  if(nrow(int_flows) > 0) { # only execute if these exist
    # add columns for to-be added information
    int_flows$xmin <- NA_real_
    int_flows$xmax <- NA_real_
    int_flows$ymin <- NA_real_
    int_flows$ymax <- NA_real_

    # the end point is the middle of the simple flow going from "linkfrom" to "linkto"
    # loop over the out_flows to get correct matching of the simple flows
    for(i in 1:nrow(int_flows)) {
      int_tmp <- int_flows[i,]  # get one row to work with
      simple_to <- int_tmp$linkto # to variable id
      simple_from <- int_tmp$linkfrom # from variable id

      # get the direct flow arrow for end positions
      simple_tmp <- subset(simple_flows, to == simple_to & from == simple_from)
      this_ymax <- mean(c(simple_tmp$ymin, simple_tmp$ymax)) # middle of direct flow
      this_xmax <- mean(c(simple_tmp$xmin, simple_tmp$xmax)) # middle of direct flow

      # get the from variable for start positions
      var_tmp <- subset(variables, id == int_tmp$from)
      this_ymin <- var_tmp$ymax # top of box
      this_xmin <- mean(c(var_tmp$xmin, var_tmp$xmax)) # middle of box

      # replace NAs
      int_flows[i, "xmin"] <- this_xmin
      int_flows[i, "xmax"] <- this_xmax
      int_flows[i, "ymin"] <- this_ymin
      int_flows[i, "ymax"] <- this_ymax

      # remove from memory to avoid overwriting potential
      rm(this_xmin, this_xmax, this_ymax,this_ymin)
    } # end interaction variable loop
  } # end interaction variable if/then for existence


  ####
  ## External interaction flows
  ####
  # These are interaction arrows that go from a state variable (node) to
  # another arrow that is either an outflow, inflow, or feedback flow.
  # Regular interactions for physical flows between variables are already
  # handled above. These external interaction flows are special, and we
  # treat them as such.
  # External flows are identified as having the interaction as TRUE and
  # the linkto is NA.
  ext_flows <- subset(flows, interaction == TRUE & is.na(linkto))
  # We also need to know the locations of all the other flows, so create
  # a temporary flows dataframe here
  other_flows <- dplyr::bind_rows(simple_flows, in_flows, out_flows, int_flows)
  # update vertical edges to avoid overlaps
  other_flows <- fix_arrow_pos(other_flows)
  # Now loop through the ext_flows for spatial processing, if there is at least 1 row
  if(nrow(ext_flows) > 0) {
    # add columns for to-be added information
    ext_flows$xmin <- NA_real_
    ext_flows$xmax <- NA_real_
    ext_flows$ymin <- NA_real_
    ext_flows$ymax <- NA_real_
    for(i in 1:nrow(ext_flows)) {
      tmp <- ext_flows[i,]

      # use the flow math to determine if this is associated with an
      # outflow or inflow
      direction <- signmat[which(flowmatred == tmp$name)]

      if(direction == "-") {
        # if an outflow (direction == "-"), then this is associated with a
        # row in the other_flows data frame where the from location
        # is different than the from location in the tmp data frame AND
        # the name is empty
        to_flow <- NULL  # null out to avoid errors
        to_flow <- other_flows[other_flows$from != tmp$from &
                                 is.na(other_flows$to) &
                                 other_flows$name == "", ]
        # this can sometimes produce a data frame with an NA row because of
        # an NA in the fields used above in the logical constraint, that
        # row is dropped here
        drops <- which(is.na(to_flow$from) & is.na(to_flow$to))
        if(length(drops) > 0){
          to_flow <- to_flow[-drops, ]
        }


        # And it is associated with the variable in the from element
        from_node <- NULL  # null this out to avoid errors
        from_node <- variables[variables$id == tmp$from, ]

        # For these complex interactions, we assume a horizontal flow
        # arrangment, user must update if more complex
        if(from_node$xlabel > mean(c(to_flow$xmax, to_flow$xmax))) {
          # this implies and arrow going from right to left
          tmp$xmin <- from_node$xmin # left edge
          tmp$xmax <- mean(c(to_flow$xmin, to_flow$xmax)) # middle
          tmp$ymin <- from_node$ylabel # middle
          tmp$ymax <- mean(c(to_flow$ymin, to_flow$ymax)) # middle
        } else { # assume left to right
          tmp$xmin <- from_node$xmax # right edge
          tmp$xmax <- mean(c(to_flow$xmin, to_flow$xmax)) # middle
          tmp$ymin <- from_node$ylabel # middle
          tmp$ymax <- mean(c(to_flow$ymin, to_flow$ymax)) # middle
        } # end left-right if/then
      } # end direction "-" if

      if(direction == "+") {
        # if the flow is a "+", then this associated with either a feedback
        # flow or an external flow into the system
        # first find the to_flow, which will be the flow with a to variable
        # that is not the current from variable and the name is empty
        to_flow <- NULL  # null out to avoid errors
        to_flow <- other_flows[other_flows$to != tmp$from &
                                 other_flows$name == "", ]
        # this can sometimes produce a data frame with an NA row because of
        # an NA in the fields used above in the logical constraint, that
        # row is dropped here
        drops <- which(is.na(to_flow$from) & is.na(to_flow$to))
        if(length(drops) > 0){
          to_flow <- to_flow[-drops, ]
        }

        # And it is associated with the variable in the from element
        from_node <- NULL  # null this out to avoid errors
        from_node <- variables[variables$id == tmp$from, ]

        # For these complex interactions, we assume a horizontal flow
        # arrangment, user must update if more complex
        if(from_node$xlabel > mean(c(to_flow$xmax, to_flow$xmax))) {
          # this implies and arrow going from right to left
          tmp$xmin <- from_node$xmin # left edge
          tmp$xmax <- mean(c(to_flow$xmin, to_flow$xmax)) # middle
          tmp$ymin <- from_node$ylabel # middle
          tmp$ymax <- mean(c(to_flow$ymin, to_flow$ymax)) # middle
        } else { # assume left to right
          tmp$xmin <- from_node$xmax # right edge
          tmp$xmax <- mean(c(to_flow$xmin, to_flow$xmax)) # middle
          tmp$ymin <- from_node$ylabel # middle
          tmp$ymax <- mean(c(to_flow$ymin, to_flow$ymax)) # middle
        } # end left-right if/then

        # Last, the "+" flows can be either an external entry or a feedback.
        # External entries will have no "from", feedbacks will. If the flow
        # is pointing to an external entry, then all location information is
        # fine. If it is pointing to a feedback loop, we need to update it
        # slightly to hit the curve. Manual adjustments will be required by the
        # user, still.
        if(!is.na(tmp$from)) {
          tmp$ymax <- tmp$ymax + 0.5  # this hits the top of the curve, generally
        }

      } # end direction "+"
      loc_cols <- c("xmin", "xmax", "ymin", "ymax")
      ext_flows[i, loc_cols] <- tmp[ , loc_cols]
    } # end external flow loop
  } # end external flow if


  ####
  ## Combine flows back together
  ####
  flows <- NULL # set original df to null to avoid/identify any potential errors
  flows <- dplyr::bind_rows(other_flows, ext_flows)


  ####
  ## Add label locations for all flows
  ####
  # label locations are mid points, which are means of the start and end positions
  flows$xlabel <- with(flows, (xmax + xmin) / 2)
  flows$ylabel <- with(flows, (ymax + ymin) / 2)

  # set default curvature of all flows, this also applies label updates
  # to curved arrows, so we do this before making minor adjustments below
  flows <- set_curvature(variables, flows)

  # apply a minor offset to move the label away from the line
  # this is done flow by flow to determine if it is vertical or horizontal
  for(i in 1:nrow(flows)) {
    tmp <- flows[i, ]
    # processing for direct flows
    if(tmp$interaction == FALSE & is.na(tmp$from) == FALSE & is.na(tmp$to) == FALSE) {
      if(tmp$xmin == tmp$xmax) { # vertical
        flows[i, "xlabel"] <- flows[i, "xlabel"] - 0.25  # move to left
      } else { # horizontal
        flows[i, "ylabel"] <- flows[i, "ylabel"] + 0.1  # move up
      }
    } else if((is.na(tmp$from) | is.na(tmp$to)) &
              tmp$interaction == FALSE) { # processing for in/out flows
      flows[i, "xlabel"] <- flows[i, "xlabel"] + 0.2  # move to right
    }

    # processing for interactions
    if(tmp$interaction == TRUE) {
      if(tmp$xmax == tmp$xmin) {  # vertical
        flows[i, "xlabel"] <- flows[i, "xlabel"] + 0.02
      } else { # horizontal
        flows[i, "ylabel"] <- flows[i, "ylabel"] + 0.2
      }
    }
  }

  # add a diff column so we can identify flows that traverse more than
  # one variable. these will be updated to have curvature that goes over
  # or under the nodes it is bypassing. works best with just a couple. if there
  # is lots of traversing, then manual intervention will be required by the user
  flows$diff <- with(flows, abs(to-from))

  # set curvature of feedback loops. this is pretty different from the
  # "regular" curvature settings, so we made a separate function for this
  # operation.
  flows <- set_feedback_curvature(flows)

  # set to/from columns in flows to NA if value is not in node dataframe
  flows <- set_node_to_na(flows, variables)

  # remove rows with no location information
  flows <- flows[!is.na(flows$xmin) &
                   !is.na(flows$xmax) &
                   !is.na(flows$ymin) &
                   !is.na(flows$ymax), ]

  # convert direct interaction to flag to regular interaction flag,
  # now only relevant for plotting
  # get row ids for the "direct interactions"
  ids <- which(flows$interaction == FALSE & flows$direct_interaction == TRUE)
  # set interaction to TRUE since this now is just for plotting aesthetics
  flows[ids, "interaction"] <- TRUE
  # remove the direct_interaction column because all processing is complete
  flows$direct_interaction <- NULL  # delete the flagging column

  # update all to and froms such that each is the variable label
  # until now, the to/from in flows has just been numeric. these
  # need to be character strings for the variable labels for plotting
  flows <- update_tofroms(flows, variables)


  # update interaction column to be type column, one of
  # main, interaction, or external. this is needed for plotting
  flows$type <- "main"  # intialize the column as all "main" flows
  # set the interaction flows according to the interaction flag
  flows$type <- ifelse(flows$interaction == TRUE, "interaction", flows$type)
  # external flows are not interactions and either the to or from id NA
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

  # add a unique id for users when interacting with update_diagram()
  # for variables, this is just the name columne
  # for flows, the unique id is the first letter of the "type" and
  # then the collapsed original name, separated by "_"
  part1 <- substr(flows$type, 1, 1)
  paste_it_too <- function(x) {  # little helper function for lapply
    ch <- get_vars_pars(x)
    paste0(ch, collapse = "")
  }
  part2 <- unlist(lapply(flows$orig_name, paste_it_too))
  # overwrite orig_name, gets changed to name in add_default_aes after
  # label column is created
  flows$orig_name <- paste0(part1, "_", part2)

  # update flows column ordering
  flows <- flows[, c("id",
                     "orig_name",
                     "name",
                     "type",
                     "from",
                     "to",
                     "xmin",
                     "xmax",
                     "ymin",
                     "ymax",
                     "xlabel",
                     "ylabel",
                     "curvature")]

  # update flows x/y min/max column names to be start/end for clarity
  cols_to_change <- which(colnames(flows) %in% c("xmin","xmax","ymin","ymax"))
  colnames(flows)[cols_to_change] <- c("xstart","xend","ystart","yend")

  # update variables column ordering
  variables <- variables[ , c("id",
                              "name",
                              "xmin",
                              "xmax",
                              "ymin",
                              "ymax",
                              "xlabel",
                              "ylabel")]


  # remove row names, those are confusing
  rownames(flows) <- NULL
  rownames(variables) <- NULL

  # add default aesthetics and unique ids
  dflist <- add_default_aes(variables, flows)

  return(dflist)
}
