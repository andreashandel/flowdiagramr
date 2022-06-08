# This is a internal development testing script for the
# flowdiagramr R package. Mostly used by ATT and AH for
# finding edge cases where errors might occur.



# Load flowdiagramr! ------------------------------------------------------

library(flowdiagramr)



# Simple SIR model for an easy test ---------------------------------------

# make model
variables = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
model_list = list(variables = variables, flows = flows)

check_model_list(model_list)


dfs <- prepare_diagram(model_list)
make_diagram(dfs)  # prints the model diagram
make_diagram(dfs, with_grid = TRUE)  # show the grid


# quick test of update
newsettings <- list(var_label_color = c(S = "green", I = "blue", R = "red"),
                    var_xlabel = c(all = -0.25, R = 0.25),
                    var_ylabel = c(all = 0.25, R = -0.25),
                    flow_line_size = c(interaction = 1.5),
                    flow_line_color = c(all = "grey25",
                                        interaction = "orange",
                                        e_n = "red"),
                    flow_xstart = c(i_bSI = -0.5),
                    flow_xlabel = c(all = 0.1, i_bSI = -0.25),
                    flow_ylabel = c(i_bSI = -0.1),
                    flow_label_text = c(i_bSI = "transmission"))
diag_list_up <- update_diagram(dfs, diagram_settings = newsettings)
make_diagram(diag_list_up)

# and one with an error
newsettings <- list(var_label_color = c(S = "green", I = "blue", R = "red"),
                    flow_line_size = c(interaction = 1.5),
                    flow_line_color = c(all = "grey25",
                                        interaction = "orange",
                                        e_n = "red"),
                    flow_xstart = c(z_n = 0.5))  # ERROR HERE IN NAME
diag_list_up <- update_diagram(dfs, diagram_settings = newsettings)


# Test locations for SIR --------------------------------------------------

varlocs1 = matrix(c("S","","R","","I",""),byrow=TRUE,nrow=2)
varlocs2 = matrix(c("S","I","R"),byrow=TRUE,nrow=3)
varlocs3 = matrix(data = c("S", "",
                           "", "I",
                           "R", "" ),
                        nrow = 3, ncol = 2, byrow = TRUE)


# two rows, with I in second row
model_settings = list(varlocations = varlocs1, varspace_y_size = 1)
make_diagram(prepare_diagram(model_list, model_settings))

# vertical diagram -- not always the best looking
model_settings = list(varlocations = varlocs2, varspace_y_size = 1)
make_diagram(prepare_diagram(model_list, model_settings), with_grid = F)

# two columns, 3 rows
# AH: this fails
model_settings = list(varlocations = varlocs3)
diagram_list <- prepare_diagram(model_list , model_settings)
make_diagram(diagram_list)



# Test model_settings and errors/warnings ---------------------------------

# these settings are good model settings, should work
model_settings1g = list(
  varbox_x_size = 0.5,
  varbox_y_size = 2)

model_settings2g = list(
  varlocations = varlocs2,
  varbox_x_size = 0.5,
  varbox_y_size = 0.5,
  varspace_x_size = 1,
  varspace_y_size = 1)

model_settings3g = list(
  varlocations = varlocs1,
  varbox_x_size = c(1,2,1),
  varbox_y_size = c(0.5,0.5,2),
  varspace_x_size = 1,
  varspace_y_size = 1)

model_settings4g = list(
  varlocations = varlocs1,
  varbox_x_size = c(1,2,1),
  varbox_y_size = c(0.5,0.5,2)
  )

model_settings5g = list(
  varlocations = varlocs1,
  varbox_x_size = c(1,0.5,1),
  varbox_y_size = c(0.5,2,2)
)

model_settings6g = list(
  varlocations = varlocs2,
  varspace_y_size = c(1,2)
)

# now try them out...no errors/warnings should be produced
diagram_list_orig <- prepare_diagram(model_list)
diagram_list1g <- prepare_diagram(model_list, model_settings1g)
diagram_list2g <- prepare_diagram(model_list, model_settings2g)
diagram_list3g <- prepare_diagram(model_list, model_settings3g)
diagram_list4g <- prepare_diagram(model_list, model_settings4g)
diagram_list5g <- prepare_diagram(model_list, model_settings5g)
diagram_list6g <- prepare_diagram(model_list, model_settings6g)

# do they all print as expected?
make_diagram(diagram_list_orig)
make_diagram(diagram_list1g)
make_diagram(diagram_list2g)
make_diagram(diagram_list3g)
make_diagram(diagram_list4g)



# these are bad settings, should fail
model_settings1b = list(
  varlocations = varlocs1,
  varbox_x_size = 0.5,
  varbox_y_size = 0.5,
  varspace_x_size = c(1, 1),
  varspace_y_size = c(0.1, 0.1))
prepare_diagram(model_list, model_settings1b)  # isses Error


# test sizes for each box
variables = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
model_list = list(variables = variables, flows = flows)
test_settings = list(varlocations = matrix(data = c("S", "",
                                                   "", "I",
                                                   "R", "" ),
                                          nrow = 3, ncol = 2, byrow = TRUE),
                    varbox_x_size = c(1, 1.5, 1),
                    varbox_y_size = c(2, 1, 1),
                    varspace_x_size = 1.5,
                    varspace_y_size = c(0.5,1)
)

dlist <- prepare_diagram(model_list, test_settings)
make_diagram(dlist, with_grid = TRUE)




# Test model updating -----------------------------------------------------

# this should work
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(
  diagram_list,
  diagram_settings = list(main_flow_line_color = "orange"))
make_diagram(diagram_list_new)

# this should work
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(
  diagram_list,
  diagram_settings = list(main_flow_line_color = "orange",
                          main_flow_arrow_size = 2,
                          interaction_flow_show_arrow = FALSE))
make_diagram(diagram_list_new)

# this should issue a warning about no new settings
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(diagram_list)  # warning issued

# this should work
diagram_settings <- list(var_outline_color = c("black", "white", "red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)  # good!

#this should work
diagram_settings <- list(var_outline_color = c("black", "white", "red"),
                         var_fill_color = c("red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)  # good!

# this should error out - it does
diagram_settings <- list(var_outline_color = c("black", "red"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - wrong number of entries again
# it does error out
diagram_settings <- list(var_outline_color = c("solid", "solid"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - it does!
diagram_settings <- list(main_flow_linetype = c("black", "red"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this breaks, good.
diagram_settings <- list(main_flow_linetype = c("black", "red", "orange"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)


# this should work; it does
newylabs <- dfs$flows$ylabel
newcurve <- dfs$flows$curvature
newylabs[1] <- 1.2
newcurve[1] <- -1
new_settings <- list(flow_ylabel = newylabs, flow_curvature = newcurve)
newdf <- update_diagram(dfs, new_settings)
make_diagram(newdf)


# this should throw an error; it does
newylabs <- 1.75
new_settings <- list(flow_ylabel = newylabs)
newdf <- update_diagram(dfs, new_settings)
make_diagram(newdf)



# Test make_diagram -------------------------------------------------------

make_diagram(diagram_list_orig)

# this works
make_diagram(diagram_list_ok)



# test predator prey model
variables = c("Pat","Imm")
flows     = list(Pat_flows = c("g*Pat*(1-Pat/pmax)", "-dP*Pat", "-k*Pat*Imm"),
                 Imm_flows = c("r*Pat*Imm", "-dI*Imm"))
model_list = list(variables = variables, flows = flows)
diagram_list <- prepare_diagram(model_list)
make_diagram(diagram_list)


# environmental transmission
variables = c("S","I","R","P")
flows = list(S_flows = c("n","-m*S","-bI*S*I", "-bP*S*P"),
             I_flows = c("bI*S*I", "bP*S*P", "-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"),
             P_flows = c("q*I", "-c*P")
)

mymodel = list(variables = variables, flows = flows)
diagram_list = prepare_diagram(mymodel)
make_diagram(diagram_list)

# better positioning
myvarlocs = matrix(c("","P","","S","I","R"),byrow=TRUE,nrow=2)
model_settings = list(varlocations = myvarlocs)
make_diagram(prepare_diagram(mymodel, model_settings = model_settings))



# Check help pages --------------------------------------------------------

?flowdiagramr
?prepare_diagram
?update_diagram
?make_diagram
?write_diagram





# Check a too-complex model -----------------------------------------------

varlabels = c("Sc","Ic","Rc","Sa","Ia","Ra","P")
varnames = c("Susceptible Children","Infected Children","Recovered Children",
             "Susceptible adults","Infected adults","Recovered adults",
             "Pathogen in Environment")
flows = list(Sc_flows = c("-Sc*bcc*Ic","-Sc*bca*Ia","-Sc*bcp*P"),
             Ic_flows = c("Sc*bcc*Ic","Sc*bca*Ia","Sc*bcp*P","-gc*Ic"),
             Rc_flows = c("gc*Ic"),
             Sa_flows = c("-Sa*bac*Ic","-Sa*baa*Ia","-Sa*bap*P"),
             Ia_flows = c("Sa*bac*Ic","Sa*baa*Ia","Sa*bap*P","-ga*Ia"),
             Ra_flows = c("ga*Ia"),
             P_flows = c("sc*Ic","sa*Ia","-d*P")
)
varlocations = matrix(data = c("Sc", "Ic", "Rc",
                               "",   "P",   "",
                               "Sa", "Ia", "Ra"),nrow = 3, byrow = TRUE)
model_list = list(variables = varlabels, flows = flows)
model_settings = list(varlocations = NULL, varbox_x_scaling = 1,
                      varbox_y_scaling = 1,
                      varspace_x_scaling = 1,
                      varspace_y_scaling = 1)
diagram_list <- prepare_diagram(model_list)
make_diagram(diagram_list)

