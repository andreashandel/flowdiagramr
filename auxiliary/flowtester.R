# This is a internal development testing script for the
# flowdiagramr R package. Mostly used by ATT and AH for
# finding edge cases where errors might occur.


# Load flowdiagramr! ------------------------------------------------------

library(flowdiagramr)

######################
# More tests, all currently showing something wrong ---------------------------------------
######################

# I think this should produce just a stand-alone R box
# instead it has an inflow
variables = c("S","I","R")
flows = list(
  S_flows = c("-b*S*I"),
  I_flows = c("b*S*I","-g*I"),
  R_flows = c("")
)
mymodel = list(variables = variables, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# The following 3 examples show problems with duplicate flows
# Can probably be all fixed by throwing an error during checking?

# The -bSI flow is a duplicate
# seems like right now it gets processed twice.
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I", "-b*S*I"),
  I_flows = c("b*S*I","-g*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

#Here, with the bSI flow duplicate
# it shows two different flows (label is there twice)
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I"),
  I_flows = c("b*S*I","b*S*I","-g*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# this variant of including a duplicate flow produces an error
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I"),
  I_flows = c("b*S*I","-g*I","b*S*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# this has flows in a weird/wrong form
# basically a single outflow matched by 2 inflows
# right now the bSI inflow term in S is ignored
# I think logically this is a wrong model
# we should throw an error for any flows where the number of inflows/outflows doesn't match
# unless there is a single inflow/outflow and no matching out/in, since those are the external flows.
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I", "b*S*I"),
  I_flows = c("b*S*I","-g*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)


# this should probably throw an error too
# user might have wanted b1SI and b2SI with flows going back and forth
# I guess if the same flow shows up in more than one place, we should throw an error
# I can't think of a reason why one would need exactly the same flow in more than one place
# usually if it's an outflow somewhere, it's inflow somewhere else
# If a user wanted say an inflow at a constant rate into compartments S and I
# they could label it 2 different ways, e.g. "r1" and "r2".
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I", "b*S*I"),
  I_flows = c("b*S*I","-g*I", "-b*S*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# this model with an outflow right back into itself
# seems to be a poorly formulated model
# right now an error is produced, but it's confusing
# maybe we should do error checking to make sure no single compartment
# has a setup like this, with flows in and out
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I", "-b*S", "b*S"),
  I_flows = c("b*S*I","-g*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)


# this currently treats S^2 as a single variable and preserves it
# I think that's ok right now? We don't really support "to the power" yet though, do we?
# The diagram looks ok based on the model, just wondering if/how things can go wrong with the ^ symbol
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S^2*I"),
  I_flows = c("b*S^2*I","-g*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# trying the power thing again
# seems actually ok, but the arrow goes through a box
# so placement of interaction arrow needs some tweaking
# otherwise seems ok
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S^2*I"),
  I_flows = c("b*S^2*I","-g*I"),
  R_flows = c("g*I","-k*R*I^2")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)

# this is a trick one. The model is properly formulated
# the arrow placement is somewhat off
# i think flipping curvature for cSI would make it right
# though the double-directional arrow between S and I is hard to read
# such loops show up I think rarely enough that we could ask the user
# to manually offset one of the arrows between S and I so it's clear those are
# 2 separate loops/processes
# UPDATE: I think back and forth might actually happen, see my updated goldilocks example
# (now as blog post)
varlabels = c("S", "I", "R")
flows = list(
  S_flows = c("-b*S*I", "c*S*I"),
  I_flows = c("b*S*I","-g*I", "-c*S*I"),
  R_flows = c("g*I")
)
mymodel = list(variables = varlabels, flows = flows)
diagram_list <- prepare_diagram(mymodel)
make_diagram(diagram_list)




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
# diag_list_up <- update_diagram(dfs, diagram_settings = newsettings)



# quick test of write_diagram
write_diagram(diag_list_up)
fs::file_delete("diagram_code.R")

# quick test of modelbuilder converter
source("./auxiliary/test-models/mbsir.R")  # makes an object named mbsir
convert_from_modelbuilder(mbmodel = mbsir)




# Test locations for SIR --------------------------------------------------

varlocs1 = matrix(c("S","","R","","I",""),byrow=TRUE,nrow=2)
varlocs2 = matrix(c("S","I","R"),byrow=TRUE,nrow=3)
varlocs3 = matrix(data = c("S", "",
                           "", "I",
                           "R", "" ),
                        nrow = 3, ncol = 2, byrow = TRUE)


# two rows, with I in second row
model_settings = list(varlocs1)
make_diagram(prepare_diagram(model_list, model_settings))

model_settings = list(varlocations = varlocs1)
make_diagram(prepare_diagram(model_list, model_settings))

# vertical diagram -- not always the best looking
model_settings = list(varlocations = varlocs2, varspace_y_size = 1)
make_diagram(prepare_diagram(model_list, model_settings), with_grid = F)

# two columns, 3 rows

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
  diagram_settings = list(flow_line_color = c(main = "orange")))
make_diagram(diagram_list_new)

# this should work
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(
  diagram_list,
  diagram_settings = list(flow_line_color = c(main = "orange"),
                          flow_arrow_size = c(main = 2),
                          flow_show_arrow = c(interaction = FALSE)))
make_diagram(diagram_list_new)

# this should issue a warning about no new settings
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(diagram_list)  # warning issued

# this should work
diagram_settings <- list(var_outline_color = c(S = "black", I = "white", R = "red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)  # good!

#this should work
diagram_settings <- list(var_outline_color = c(S = "black", I = "white", R = "red"),
                         var_fill_color = c(all ="red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)  # good!

# this should error out - it does
diagram_settings <- list(var_outline_color = c(J = "black", K = "red"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - wrong number of entries again
# it does error out
diagram_settings <- list(var_outline_color = c(all = "solid"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - it does!
diagram_settings <- list(flow_line_type = c(main = "black"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)




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

