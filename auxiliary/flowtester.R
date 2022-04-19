# library(flowdiagramr)

#make model
variables = c("S","I","R")
flows = list(S_flows = c("n", "-b*S*I", "-m*S"),
             I_flows = c("+b*S*I","-g*I", "-m*I"),
             R_flows = c("g*I", "-m*R"))
model_list = list(variables = variables, flows = flows)

dfs <- prepare_diagram(model_list)
make_diagram(dfs)

varlocs1 = matrix(c("S","","R","","I",""),byrow=TRUE,nrow=2)
varlocs2 = matrix(c("S","I","R"),byrow=TRUE,nrow=3)

model_settings = list(varlocations = varlocs2, varspace_y_size = 1)
make_diagram(prepare_diagram(model_list, model_settings), with_grid = F)


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


#####################
# check prepare_diagram
#####################

diagram_list_orig <- prepare_diagram(model_list)
diagram_list1g <- prepare_diagram(model_list, model_settings1g)
diagram_list2g <- prepare_diagram(model_list, model_settings2g)
diagram_list3g <- prepare_diagram(model_list, model_settings3g)
diagram_list4g <- prepare_diagram(model_list, model_settings4g)
diagram_list5g <- prepare_diagram(model_list, model_settings5g)
diagram_list6g <- prepare_diagram(model_list, model_settings6g)

make_diagram(diagram_list_orig)
make_diagram(diagram_list1g)
make_diagram(diagram_list2g)
make_diagram(diagram_list3g)
make_diagram(diagram_list4g)



# these are bad settings, should fail
model_settings1b = list(
  varlocations = varlocs,
  varbox_x_size = 0.5,
  varbox_y_size = 0.5,
  varspace_x_size = c(1, 1),
  varspace_y_size = c(0.1, 0.1))





#####################
# check update_diagram
#####################

#eventually this should probably just throw an error
diagram_list <- prepare_diagram(model_list)
diagram_list_new <- update_diagram(diagram_list)

#this should work
diagram_settings <- list(var_outline_color = c("black", "white", "red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)

#this should work
diagram_settings <- list(var_outline_color = c("black", "white", "red"),
                         var_fill_color = c("red"))
diagram_list_ok <- update_diagram(diagram_list, diagram_settings)
make_diagram(diagram_list_ok)

# this should error out - it does
diagram_settings <- list(var_outline_color = c("black", "red"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - wrong number of entries again
# it does error out
diagram_settings <- list(var_outline_color = c("solid", "solid"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# this should also error out - or be caught later, when doing make_diagram
# currently does not, and instead runs. That will break make_diagram
# ATT: this works because there are 2 main flows
diagram_settings <- list(main_flow_linetype = c("black", "red"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)

# ATT: this breaks
diagram_settings <- list(main_flow_linetype = c("black", "red", "orange"))
diagram_list_new <- update_diagram(diagram_list, diagram_settings)



#####################
# check make_diagram
#####################

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











#
# model_settings = list(varnames = varnames, varlocations = varlocations, use_varnames = TRUE)
#
#
# diagram_list <- prepare_diagram(model_list, model_settings)
#
# write_diagram(model_list, model_settings)
# write_diagram(diagram_list = diagram_list)
#
# ?make_diagram()
#
# make_diagram(diagram_list)
# make_diagram(diagram_list, diagram_settings = list(main_flow_on = FALSE))
# make_diagram(diagram_list, diagram_settings = list(external_flow_on = FALSE))
# make_diagram(diagram_list, diagram_settings = list(interaction_flow_on = FALSE))
# make_diagram(diagram_list, diagram_settings = list(interaction_flow_label_on = FALSE))
# make_diagram(diagram_list, diagram_settings = list(main_flow_label_on = FALSE))
# make_diagram(diagram_list, diagram_settings = list(external_flow_label_on = FALSE))
# make_diagram(diagram_list, diagram_settings = list(external_flow_label_on = FALSE,
#                                                    external_flow_on = FALSE))
#
# make_diagram(diagram_list, diagram_settings = list(main_flow_color = "blue",
#                                                    interaction_flow_color = "red",
#                                                    external_flow_color = "orange",
#                                                    external_flow_linetype = "dotted",
#                                                    var_fill_color = c("salmon", "cyan"),
#                                                    var_outline_color = "dodgerblue",
#                                                    main_flow_size = 5))
#
#
# make_diagram(diagram_list, diagram_settings = list(main_flow_color = "blue",
#                                                    interaction_flow_color = "red",
#                                                    external_flow_color = "orange",
#                                                    external_flow_linetype = 6,
#                                                    var_fill_color = c("salmon", "cyan"),
#                                                    var_outline_color = "dodgerblue",
#                                                    main_flow_size = 5))
#
#
#
#
#
#
#
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
#
# diagram_settings <- list(
#   var_fill_color = c("#6aa4c8", "#eb5600", "#1a9988", "#2987c2", "#e38b59", "#5c948c", "#e8e656"),
#   interaction_flow_label_size = 4,
#   interaction_flow_color = "blue",
#   with_grid = TRUE
# )
# model_plot <- make_diagram(diagram_list, diagram_settings)
# plot(model_plot)
#
